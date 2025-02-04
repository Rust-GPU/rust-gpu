use anyhow::{Context, Result, anyhow};
use regex::Regex;
use std::{
    collections::{HashMap, HashSet},
    fs,
    io::Write,
    path::{Path, PathBuf},
    sync::Arc,
};
use tracing::{debug, error, info};
use tracing_subscriber::FmtSubscriber;

// Comment directive parsing.
static DEFAULT_REGEX_STR: &str = r#"^\s*//\s*default-(vertex|fragment)\s*$"#;
static COMPUTE_OUTPUT_REGEX_STR: &str = r#"^\s*//\s*output:\s*(.+)\s*$"#;
static DISPATCH_REGEX_STR: &str = r#"^\s*//\s*dispatch:\s*(\d+),\s*(\d+),\s*(\d+)\s*$"#;
// Detecting shader type.
// TODO(@LegNeato): expose this on SpirvBuilder.
static SPIRV_VERTEX_REGEX: &str = r#"^\s*#\s*\[\s*spirv\s*\(\s*vertex\s*\)\s*\]\s*$"#;
static SPIRV_FRAGMENT_REGEX: &str = r#"^\s*#\s*\[\s*spirv\s*\(\s*fragment\s*\)\s*\]\s*$"#;
static SPIRV_COMPUTE_REGEX: &str = r#"^\s*#\s*\[\s*spirv\s*\(\s*compute\s*(\(|\)\s*\))?.*\]\s*$"#;

// Convert a path (e.g. "pipelines/graphics/simple") into a test name with "::"
fn format_lib_path(path: &Path) -> String {
    path.to_string_lossy().replace(&['/', '\\'][..], "::")
}

/// Filesystem abstraction to enable testing.
pub trait FileSystem {
    fn read_dir(&self, path: &Path) -> Result<Vec<PathBuf>>;
    fn read_to_string(&self, path: &Path) -> Result<String>;
    fn exists(&self, path: &Path) -> bool;
}

/// Real filesystem operations.
pub struct RealFs;
impl FileSystem for RealFs {
    fn read_dir(&self, path: &Path) -> Result<Vec<PathBuf>> {
        let mut entries = Vec::new();
        for entry in fs::read_dir(path)? {
            entries.push(entry?.path());
        }
        Ok(entries)
    }
    fn read_to_string(&self, path: &Path) -> Result<String> {
        fs::read_to_string(path).map_err(|e| e.into())
    }
    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum ShaderType {
    Vertex,
    Fragment,
    Compute,
}
impl ShaderType {
    fn wgsl_filename(&self) -> &'static str {
        match self {
            ShaderType::Vertex => "vertex.wgsl",
            ShaderType::Fragment => "fragment.wgsl",
            ShaderType::Compute => "compute.wgsl",
        }
    }
    fn entry_point(&self) -> &'static str {
        match self {
            ShaderType::Vertex => "main_vs",
            ShaderType::Fragment => "main_fs",
            ShaderType::Compute => "main_cs",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum PipelineType {
    Graphics,
    Compute,
    Mixed,
}
impl PipelineType {
    fn from_path(path: &Path) -> Option<Self> {
        if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
            let lower = name.to_lowercase();
            if lower.contains("compute") {
                Some(PipelineType::Compute)
            } else if lower.contains("graphics") {
                Some(PipelineType::Graphics)
            } else if lower.contains("mixed") {
                Some(PipelineType::Mixed)
            } else {
                None
            }
        } else {
            None
        }
    }
    fn expected_shader_types(&self) -> HashSet<ShaderType> {
        match self {
            PipelineType::Graphics => {
                let mut set = HashSet::new();
                set.insert(ShaderType::Vertex);
                set.insert(ShaderType::Fragment);
                set
            }
            PipelineType::Compute => {
                let mut set = HashSet::new();
                set.insert(ShaderType::Compute);
                set
            }
            PipelineType::Mixed => {
                let mut set = HashSet::new();
                set.insert(ShaderType::Vertex);
                set.insert(ShaderType::Fragment);
                set.insert(ShaderType::Compute);
                set
            }
        }
    }
    // Convert to string for output file naming.
    fn as_str(&self) -> &'static str {
        match self {
            PipelineType::Graphics => "graphics",
            PipelineType::Compute => "compute",
            PipelineType::Mixed => "mixed",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ShaderLanguage {
    Wgsl,
    Rust,
}
impl ShaderLanguage {
    fn as_str(&self) -> &'static str {
        match self {
            ShaderLanguage::Wgsl => "wgsl",
            ShaderLanguage::Rust => "rust",
        }
    }
}

/// Compute shader output type.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ComputeOutputType {
    U32,
    U8Slice,
    F32,
}
impl ComputeOutputType {
    fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_lowercase().as_str() {
            "u32" => Some(ComputeOutputType::U32),
            "[u8]" => Some(ComputeOutputType::U8Slice),
            "f32" => Some(ComputeOutputType::F32),
            _ => None,
        }
    }
    fn buffer_size(&self) -> u64 {
        match self {
            ComputeOutputType::U32 | ComputeOutputType::F32 => 4,
            ComputeOutputType::U8Slice => 16,
        }
    }
}

#[derive(Debug)]
struct Config {
    default_vertex: bool,
    default_fragment: bool,
    compute_output: ComputeOutputType,
    dispatch: (u32, u32, u32),
}
impl Config {
    fn parse(
        content: &str,
        default_regex: &Regex,
        compute_output_regex: &Regex,
        dispatch_regex: &Regex,
    ) -> Result<Self> {
        let mut default_vertex = false;
        let mut default_fragment = false;
        let mut compute_output = None;
        let mut dispatch = (1, 1, 1);
        for line in content.lines() {
            if let Some(caps) = default_regex.captures(line) {
                match &caps[1].to_lowercase()[..] {
                    "vertex" => default_vertex = true,
                    "fragment" => default_fragment = true,
                    _ => {}
                }
            }
            if let Some(caps) = compute_output_regex.captures(line) {
                compute_output = Some(
                    ComputeOutputType::from_str(&caps[1])
                        .ok_or_else(|| anyhow!("Invalid compute output type: {}", &caps[1]))?,
                );
            }
            if let Some(caps) = dispatch_regex.captures(line) {
                let x = caps[1].parse::<u32>()?;
                let y = caps[2].parse::<u32>()?;
                let z = caps[3].parse::<u32>()?;
                dispatch = (x, y, z);
            }
        }
        let compute_output = compute_output.unwrap_or(ComputeOutputType::U32);
        Ok(Config {
            default_vertex,
            default_fragment,
            compute_output,
            dispatch,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ShaderSource {
    Wgsl(String),
    SpirV(Vec<u32>),
}
type ShaderCollection = HashMap<ShaderType, ShaderSource>;

fn get_default_shader(shader_type: ShaderType) -> &'static str {
    match shader_type {
        ShaderType::Vertex => {
            r#"
@vertex
fn main_vs() -> @builtin(position) vec4<f32> {
    return vec4<f32>(0.0, 0.0, 0.0, 1.0);
}
"#
        }
        ShaderType::Fragment => {
            r#"
@fragment
fn main_fs() -> @location(0) vec4<f32> {
    return vec4<f32>(1.0, 1.0, 1.0, 1.0);
}
"#
        }
        ShaderType::Compute => {
            r#"
@compute
fn main_cs() {
    // Default compute shader does nothing.
}
"#
        }
    }
}

fn collect_shader_sources(
    fs: &dyn FileSystem,
    dir: &Path,
    expected: &HashSet<ShaderType>,
    found: &HashSet<ShaderType>,
    config: &Config,
) -> Result<ShaderCollection> {
    let mut shaders = ShaderCollection::new();
    for shader_type in expected {
        if found.contains(shader_type) {
            let path = dir.join(shader_type.wgsl_filename());
            let source = fs
                .read_to_string(&path)
                .with_context(|| format!("Reading WGSL file {:?} failed", path))?;
            shaders.insert(*shader_type, ShaderSource::Wgsl(source));
        } else {
            let use_default = match shader_type {
                ShaderType::Vertex => config.default_vertex,
                ShaderType::Fragment => config.default_fragment,
                ShaderType::Compute => false,
            };
            if use_default {
                shaders.insert(
                    *shader_type,
                    ShaderSource::Wgsl(get_default_shader(*shader_type).to_string()),
                );
                info!("Using default {:?} shader.", shader_type);
            } else {
                return Err(anyhow!(
                    "Shader {:?} not defined and no default provided.",
                    shader_type
                ));
            }
        }
    }
    Ok(shaders)
}

#[derive(Debug)]
struct ShaderLibrary {
    path: PathBuf,
    pipeline_type: PipelineType,
    config: Config,
    shaders: ShaderCollection,
    compile_flags: Vec<String>,
}
impl ShaderLibrary {
    #[allow(clippy::too_many_arguments)]
    fn load(
        fs: &dyn FileSystem,
        path: PathBuf,
        default_regex: &Regex,
        compute_output_regex: &Regex,
        dispatch_regex: &Regex,
        vertex_regex: &Regex,
        fragment_regex: &Regex,
        compute_regex: &Regex,
    ) -> Result<Self> {
        let rust_file = if path.is_file() {
            path.clone()
        } else if path.is_dir() {
            path.join("src").join("lib.rs")
        } else {
            return Err(anyhow!("Invalid shader library path: {:?}", path));
        };
        if !fs.exists(&rust_file) {
            return Err(anyhow!("Missing Rust file at {:?}", rust_file));
        }
        let content = fs
            .read_to_string(&rust_file)
            .with_context(|| format!("Failed to read file {:?}", rust_file))?;
        let config = Config::parse(
            &content,
            default_regex,
            compute_output_regex,
            dispatch_regex,
        )?;
        let compile_flags = content
            .lines()
            .filter(|line| line.trim_start().starts_with("// compile-flags:"))
            .flat_map(|line| {
                line.trim_start()["// compile-flags:".len()..]
                    .trim()
                    .split(',')
                    .map(|flag| flag.trim().to_string())
                    .collect::<Vec<String>>()
            })
            .collect::<Vec<String>>();
        let pipeline_type = if let Some(pt) = PipelineType::from_path(&path) {
            pt
        } else {
            let found: HashSet<_> = content
                .lines()
                .filter_map(|line| {
                    if vertex_regex.is_match(line) {
                        return Some(ShaderType::Vertex);
                    }
                    if fragment_regex.is_match(line) {
                        return Some(ShaderType::Fragment);
                    }
                    if compute_regex.is_match(line) {
                        return Some(ShaderType::Compute);
                    }
                    None
                })
                .collect();
            if found.contains(&ShaderType::Compute)
                && (found.contains(&ShaderType::Vertex) || found.contains(&ShaderType::Fragment))
            {
                PipelineType::Mixed
            } else if found.contains(&ShaderType::Compute) {
                PipelineType::Compute
            } else {
                PipelineType::Graphics
            }
        };
        let expected = pipeline_type.expected_shader_types();
        let found: HashSet<_> = content
            .lines()
            .filter_map(|line| {
                if vertex_regex.is_match(line) {
                    return Some(ShaderType::Vertex);
                }
                if fragment_regex.is_match(line) {
                    return Some(ShaderType::Fragment);
                }
                if compute_regex.is_match(line) {
                    return Some(ShaderType::Compute);
                }
                None
            })
            .collect();
        if found.is_empty() {
            return Err(anyhow!("No spirv annotations found in {:?}", rust_file));
        }
        let shaders = collect_shader_sources(fs, &path, &expected, &found, &config)?;
        let extra: HashSet<_> = found.difference(&expected).collect();
        if !extra.is_empty() {
            return Err(anyhow!("Extra spirv annotations: {:?}", extra));
        }
        Ok(ShaderLibrary {
            path,
            pipeline_type,
            config,
            shaders,
            compile_flags,
        })
    }
}

fn compile_spirv_shaders(lib: &ShaderLibrary) -> Result<ShaderCollection> {
    let mut builder = spirv_builder::SpirvBuilder::new(&lib.path, "spirv-unknown-vulkan1.1")
        .print_metadata(spirv_builder::MetadataPrintout::None)
        .release(true)
        .multimodule(false)
        .shader_panic_strategy(spirv_builder::ShaderPanicStrategy::SilentExit)
        .relax_block_layout(true)
        .uniform_buffer_standard_layout(true)
        .scalar_block_layout(false)
        .skip_block_layout(false)
        .preserve_bindings(true);
    for flag in &lib.compile_flags {
        builder = builder.extra_arg(flag.clone());
    }
    let artifact = builder
        .build()
        .context("Failed to compile SPIR-V shaders")?;
    let spv_path = match artifact.module {
        spirv_builder::ModuleResult::SingleModule(path) => path,
        spirv_builder::ModuleResult::MultiModule(modules) => {
            if modules.len() == 1 {
                modules.into_iter().next().unwrap().1
            } else {
                return Err(anyhow!("Multiple SPIR-V modules produced; expected one."));
            }
        }
    };
    let spv_bytes = fs::read(&spv_path)
        .with_context(|| format!("Reading SPIR-V binary from {:?} failed", spv_path))?;
    if spv_bytes.len() % 4 != 0 {
        return Err(anyhow!("SPIR-V binary length is not a multiple of 4"));
    }
    let spv_u32: Vec<u32> = bytemuck::cast_slice(&spv_bytes).to_vec();
    let mut shaders = ShaderCollection::new();
    for shader_type in lib.pipeline_type.expected_shader_types() {
        shaders.insert(shader_type, ShaderSource::SpirV(spv_u32.clone()));
    }
    Ok(shaders)
}

fn write_binary_output_file(
    variant: ShaderLanguage,
    lib_path: &Path,
    pipeline: PipelineType,
    data: &[u8],
) -> Result<PathBuf> {
    let lib_name = lib_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("lib");
    let filename = format!("{}_{}_{}", lib_name, variant.as_str(), pipeline.as_str());
    let mut temp_file = tempfile::Builder::new()
        .prefix(&filename)
        .suffix(".bin")
        .tempfile()?;
    temp_file.write_all(data)?;
    let (_file, path) = temp_file.keep().map_err(|e| anyhow!(e))?;
    Ok(path)
}

fn write_image_output_file(
    variant: ShaderLanguage,
    lib_path: &Path,
    pipeline: PipelineType,
    data: &[u8],
) -> Result<PathBuf> {
    use image::{ImageBuffer, ImageFormat, Rgba};
    let width = 640;
    let height = 480;
    if data.len() != (width * height * 4) as usize {
        return Err(anyhow!("Unexpected data length"));
    }
    let mut data_rgba = Vec::with_capacity(data.len());
    for chunk in data.chunks_exact(4) {
        data_rgba.push(chunk[2]); // red
        data_rgba.push(chunk[1]); // green
        data_rgba.push(chunk[0]); // blue
        data_rgba.push(chunk[3]); // alpha
    }
    let img_buf = ImageBuffer::<Rgba<u8>, _>::from_raw(width, height, data_rgba)
        .ok_or_else(|| anyhow!("Failed to create image buffer"))?;
    let lib_name = lib_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("lib");
    let filename = format!("{}_{}_{}", lib_name, variant.as_str(), pipeline.as_str());
    let temp_file = tempfile::Builder::new()
        .prefix(&filename)
        .suffix(".tiff")
        .tempfile()?;
    img_buf.save_with_format(temp_file.path(), ImageFormat::Tiff)?;
    let (_file, path) = temp_file.keep().map_err(|e| anyhow!(e))?;
    Ok(path)
}

mod runners {
    use super::*;
    use std::borrow::Cow;
    pub trait Runner {
        fn run(
            &self,
            device: &wgpu::Device,
            queue: &wgpu::Queue,
            shaders: &ShaderCollection,
            config: &Config,
            variant: ShaderLanguage,
            lib_path: &Path,
        ) -> Result<PathBuf>;
    }
    pub struct ComputeRunner;
    impl Runner for ComputeRunner {
        fn run(
            &self,
            device: &wgpu::Device,
            queue: &wgpu::Queue,
            shaders: &ShaderCollection,
            config: &Config,
            variant: ShaderLanguage,
            lib_path: &Path,
        ) -> Result<PathBuf> {
            let source = shaders
                .get(&ShaderType::Compute)
                .ok_or_else(|| anyhow!("Missing compute shader"))?;
            let module = match source {
                ShaderSource::Wgsl(code) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Compute Shader WGSL"),
                        source: wgpu::ShaderSource::Wgsl(code.into()),
                    })
                }
                ShaderSource::SpirV(binary) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Compute Shader SPIR-V"),
                        source: wgpu::ShaderSource::SpirV(Cow::Borrowed(binary)),
                    })
                }
            };
            let pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
                label: Some("Compute Pipeline"),
                layout: None,
                module: &module,
                entry_point: Some(ShaderType::Compute.entry_point()),
                cache: Default::default(),
                compilation_options: Default::default(),
            });
            let buffer_size = config.compute_output.buffer_size();
            let buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("Result Buffer"),
                size: buffer_size,
                usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC,
                mapped_at_creation: false,
            });
            let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
                layout: &pipeline.get_bind_group_layout(0),
                entries: &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: buffer.as_entire_binding(),
                }],
                label: Some("Compute Bind Group"),
            });
            let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Compute Encoder"),
            });
            {
                let mut pass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
                    label: Some("Compute Pass"),
                    timestamp_writes: Default::default(),
                });
                pass.set_pipeline(&pipeline);
                pass.set_bind_group(0, &bind_group, &[]);
                pass.dispatch_workgroups(config.dispatch.0, config.dispatch.1, config.dispatch.2);
            }
            let output_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("Output Buffer"),
                size: buffer_size,
                usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
                mapped_at_creation: false,
            });
            encoder.copy_buffer_to_buffer(&buffer, 0, &output_buffer, 0, buffer_size);
            queue.submit(Some(encoder.finish()));

            let slice = output_buffer.slice(..);
            let (sender, receiver) = futures::channel::oneshot::channel();
            slice.map_async(wgpu::MapMode::Read, move |result| {
                sender.send(result).unwrap();
            });
            device.poll(wgpu::Maintain::Wait);
            futures::executor::block_on(receiver).context("Mapping compute output failed")??;
            let data = slice.get_mapped_range().to_vec();
            output_buffer.unmap();
            write_binary_output_file(variant, lib_path, PipelineType::Compute, &data)
        }
    }
    pub struct GraphicsRunner;
    impl Runner for GraphicsRunner {
        fn run(
            &self,
            device: &wgpu::Device,
            queue: &wgpu::Queue,
            shaders: &ShaderCollection,
            _config: &Config,
            variant: ShaderLanguage,
            lib_path: &Path,
        ) -> Result<PathBuf> {
            let vertex = shaders
                .get(&ShaderType::Vertex)
                .ok_or_else(|| anyhow!("Missing vertex shader"))?;
            let fragment = shaders
                .get(&ShaderType::Fragment)
                .ok_or_else(|| anyhow!("Missing fragment shader"))?;
            let vertex_module = match vertex {
                ShaderSource::Wgsl(code) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Vertex Shader WGSL"),
                        source: wgpu::ShaderSource::Wgsl(code.into()),
                    })
                }
                ShaderSource::SpirV(binary) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Vertex Shader SPIR-V"),
                        source: wgpu::ShaderSource::SpirV(Cow::Borrowed(binary)),
                    })
                }
            };
            let fragment_module = match fragment {
                ShaderSource::Wgsl(code) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Fragment Shader WGSL"),
                        source: wgpu::ShaderSource::Wgsl(code.into()),
                    })
                }
                ShaderSource::SpirV(binary) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Fragment Shader SPIR-V"),
                        source: wgpu::ShaderSource::SpirV(Cow::Borrowed(binary)),
                    })
                }
            };
            let layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("Pipeline Layout"),
                bind_group_layouts: &[],
                push_constant_ranges: &[],
            });
            let render_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Render Pipeline"),
                layout: Some(&layout),
                vertex: wgpu::VertexState {
                    module: &vertex_module,
                    entry_point: Some(ShaderType::Vertex.entry_point()),
                    buffers: &[],
                    compilation_options: Default::default(),
                },
                fragment: Some(wgpu::FragmentState {
                    module: &fragment_module,
                    entry_point: Some(ShaderType::Fragment.entry_point()),
                    targets: &[Some(wgpu::ColorTargetState {
                        format: wgpu::TextureFormat::Bgra8UnormSrgb,
                        blend: Some(wgpu::BlendState::REPLACE),
                        write_mask: wgpu::ColorWrites::ALL,
                    })],
                    compilation_options: Default::default(),
                }),
                primitive: wgpu::PrimitiveState::default(),
                depth_stencil: None,
                multisample: wgpu::MultisampleState::default(),
                multiview: None,
                cache: Default::default(),
            });
            let tex_extent = wgpu::Extent3d {
                width: 640,
                height: 480,
                depth_or_array_layers: 1,
            };
            let output_tex = device.create_texture(&wgpu::TextureDescriptor {
                label: Some("Output Texture"),
                size: tex_extent,
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format: wgpu::TextureFormat::Bgra8UnormSrgb,
                view_formats: &[],
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
            });
            let output_view = output_tex.create_view(&wgpu::TextureViewDescriptor::default());
            let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Graphics Encoder"),
            });
            {
                let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                    label: Some("Render Pass"),
                    color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                        view: &output_view,
                        resolve_target: None,
                        ops: wgpu::Operations {
                            load: wgpu::LoadOp::Clear(wgpu::Color::BLUE),
                            store: wgpu::StoreOp::Store,
                        },
                    })],
                    depth_stencil_attachment: None,
                    timestamp_writes: Default::default(),
                    occlusion_query_set: Default::default(),
                });
                pass.set_pipeline(&render_pipeline);
                pass.draw(0..3, 0..1);
            }
            let buffer_size = (640 * 480 * 4) as u64;
            let output_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("Graphics Output Buffer"),
                size: buffer_size,
                usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
                mapped_at_creation: false,
            });
            encoder.copy_texture_to_buffer(
                wgpu::ImageCopyTexture {
                    texture: &output_tex,
                    mip_level: 0,
                    origin: wgpu::Origin3d::ZERO,
                    aspect: wgpu::TextureAspect::All,
                },
                wgpu::ImageCopyBuffer {
                    buffer: &output_buffer,
                    layout: wgpu::ImageDataLayout {
                        offset: 0,
                        bytes_per_row: Some(4 * 640),
                        rows_per_image: Some(480),
                    },
                },
                tex_extent,
            );
            queue.submit(Some(encoder.finish()));
            let slice = output_buffer.slice(..);
            let (sender, receiver) = futures::channel::oneshot::channel();
            slice.map_async(wgpu::MapMode::Read, move |result| {
                sender.send(result).unwrap();
            });
            device.poll(wgpu::Maintain::Wait);
            futures::executor::block_on(receiver).context("Mapping graphics output failed")??;
            let data = slice.get_mapped_range().to_vec();
            output_buffer.unmap();
            write_image_output_file(variant, lib_path, PipelineType::Graphics, &data)
        }
    }
    pub struct MixedRunner;
    impl Runner for MixedRunner {
        fn run(
            &self,
            device: &wgpu::Device,
            queue: &wgpu::Queue,
            shaders: &ShaderCollection,
            config: &Config,
            variant: ShaderLanguage,
            lib_path: &Path,
        ) -> Result<PathBuf> {
            // Run compute pass.
            let compute_source = shaders
                .get(&ShaderType::Compute)
                .ok_or_else(|| anyhow!("Missing compute shader"))?;
            let compute_module = match compute_source {
                ShaderSource::Wgsl(code) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Mixed Compute Shader WGSL"),
                        source: wgpu::ShaderSource::Wgsl(code.into()),
                    })
                }
                ShaderSource::SpirV(binary) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Mixed Compute Shader SPIR-V"),
                        source: wgpu::ShaderSource::SpirV(Cow::Borrowed(binary)),
                    })
                }
            };
            let compute_pipeline =
                device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
                    label: Some("Mixed Compute Pipeline"),
                    layout: None,
                    module: &compute_module,
                    entry_point: Some(ShaderType::Compute.entry_point()),
                    compilation_options: Default::default(),
                    cache: Default::default(),
                });
            let buffer_size = config.compute_output.buffer_size();
            let compute_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("Mixed Compute Buffer"),
                size: buffer_size,
                usage: wgpu::BufferUsages::STORAGE
                    | wgpu::BufferUsages::COPY_SRC
                    | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });
            let compute_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("Mixed Compute Bind Group"),
                layout: &compute_pipeline.get_bind_group_layout(0),
                entries: &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: compute_buffer.as_entire_binding(),
                }],
            });
            let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Mixed Encoder"),
            });
            {
                let mut compute_pass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
                    label: Some("Mixed Compute Pass"),
                    timestamp_writes: Default::default(),
                });
                compute_pass.set_pipeline(&compute_pipeline);
                compute_pass.set_bind_group(0, &compute_bind_group, &[]);
                compute_pass.dispatch_workgroups(
                    config.dispatch.0,
                    config.dispatch.1,
                    config.dispatch.2,
                );
            }
            // Run render pass.
            let vertex = shaders
                .get(&ShaderType::Vertex)
                .ok_or_else(|| anyhow!("Missing vertex shader"))?;
            let fragment = shaders
                .get(&ShaderType::Fragment)
                .ok_or_else(|| anyhow!("Missing fragment shader"))?;
            let vertex_module = match vertex {
                ShaderSource::Wgsl(code) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Mixed Vertex Shader WGSL"),
                        source: wgpu::ShaderSource::Wgsl(code.into()),
                    })
                }
                ShaderSource::SpirV(binary) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Mixed Vertex Shader SPIR-V"),
                        source: wgpu::ShaderSource::SpirV(Cow::Borrowed(binary)),
                    })
                }
            };
            let fragment_module = match fragment {
                ShaderSource::Wgsl(code) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Mixed Fragment Shader WGSL"),
                        source: wgpu::ShaderSource::Wgsl(code.into()),
                    })
                }
                ShaderSource::SpirV(binary) => {
                    device.create_shader_module(wgpu::ShaderModuleDescriptor {
                        label: Some("Mixed Fragment Shader SPIR-V"),
                        source: wgpu::ShaderSource::SpirV(Cow::Borrowed(binary)),
                    })
                }
            };
            let computed_buffer_bind_group_layout =
                device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: Some("Computed Buffer Bind Group Layout"),
                    entries: &[wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Storage { read_only: true },
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    }],
                });
            let computed_buffer_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("Computed Buffer Bind Group"),
                layout: &computed_buffer_bind_group_layout,
                entries: &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: compute_buffer.as_entire_binding(),
                }],
            });
            let render_pipeline_layout =
                device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some("Mixed Render Pipeline Layout"),
                    bind_group_layouts: &[&computed_buffer_bind_group_layout],
                    push_constant_ranges: &[],
                });
            let tex_extent = wgpu::Extent3d {
                width: 640,
                height: 480,
                depth_or_array_layers: 1,
            };
            let output_tex = device.create_texture(&wgpu::TextureDescriptor {
                label: Some("Mixed Render Texture"),
                size: tex_extent,
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format: wgpu::TextureFormat::Bgra8UnormSrgb,
                view_formats: &[],
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
            });
            let output_view = output_tex.create_view(&wgpu::TextureViewDescriptor::default());
            let render_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Mixed Render Pipeline"),
                layout: Some(&render_pipeline_layout),
                vertex: wgpu::VertexState {
                    module: &vertex_module,
                    entry_point: Some(ShaderType::Vertex.entry_point()),
                    buffers: &[],
                    compilation_options: Default::default(),
                },
                fragment: Some(wgpu::FragmentState {
                    module: &fragment_module,
                    entry_point: Some(ShaderType::Fragment.entry_point()),
                    targets: &[Some(wgpu::ColorTargetState {
                        format: wgpu::TextureFormat::Bgra8UnormSrgb,
                        blend: Some(wgpu::BlendState::REPLACE),
                        write_mask: wgpu::ColorWrites::ALL,
                    })],
                    compilation_options: Default::default(),
                }),
                primitive: wgpu::PrimitiveState::default(),
                depth_stencil: None,
                multisample: wgpu::MultisampleState::default(),
                multiview: None,
                cache: Default::default(),
            });
            {
                let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                    label: Some("Mixed Render Pass"),
                    color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                        view: &output_view,
                        resolve_target: None,
                        ops: wgpu::Operations {
                            load: wgpu::LoadOp::Clear(wgpu::Color::GREEN),
                            store: wgpu::StoreOp::Store,
                        },
                    })],
                    depth_stencil_attachment: None,
                    occlusion_query_set: Default::default(),
                    timestamp_writes: Default::default(),
                });
                render_pass.set_bind_group(0, &computed_buffer_bind_group, &[]);
                render_pass.set_pipeline(&render_pipeline);
                render_pass.draw(0..3, 0..1);
            }
            let buffer_size = (640 * 480 * 4) as u64;
            let output_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("Mixed Output Buffer"),
                size: buffer_size,
                usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
                mapped_at_creation: false,
            });
            encoder.copy_texture_to_buffer(
                wgpu::ImageCopyTexture {
                    texture: &output_tex,
                    mip_level: 0,
                    origin: wgpu::Origin3d::ZERO,
                    aspect: wgpu::TextureAspect::All,
                },
                wgpu::ImageCopyBuffer {
                    buffer: &output_buffer,
                    layout: wgpu::ImageDataLayout {
                        offset: 0,
                        bytes_per_row: Some(4 * 640),
                        rows_per_image: Some(480),
                    },
                },
                tex_extent,
            );
            queue.submit(Some(encoder.finish()));
            let slice = output_buffer.slice(..);
            let (sender, receiver) = futures::channel::oneshot::channel();
            slice.map_async(wgpu::MapMode::Read, move |result| {
                sender.send(result).unwrap();
            });
            device.poll(wgpu::Maintain::Wait);
            futures::executor::block_on(receiver)
                .context("Mapping mixed render output failed")??;
            let data = slice.get_mapped_range().to_vec();
            output_buffer.unmap();
            write_image_output_file(variant, lib_path, PipelineType::Mixed, &data)
        }
    }
}

fn get_runner(pipeline: PipelineType) -> Box<dyn runners::Runner> {
    match pipeline {
        PipelineType::Compute => Box::new(runners::ComputeRunner),
        PipelineType::Graphics => Box::new(runners::GraphicsRunner),
        PipelineType::Mixed => Box::new(runners::MixedRunner),
    }
}

async fn initialize_wgpu_async() -> Result<(wgpu::Device, wgpu::Queue)> {
    let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
        // Force Vulkan on Linux so we get coverage without going through `naga`
        // translation.
        #[cfg(target_os = "linux")]
        backends: wgpu::Backends::VULKAN,
        // Default to primary backends elsewhere
        #[cfg(not(target_os = "linux"))]
        backends: wgpu::Backends::PRIMARY,
        dx12_shader_compiler: Default::default(),
        flags: Default::default(),
        gles_minor_version: Default::default(),
    });
    let adapter = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: None,
            force_fallback_adapter: false,
        })
        .await
        .ok_or_else(|| anyhow!("Failed to find a suitable GPU adapter"))?;
    let (device, queue) = adapter
        .request_device(
            &wgpu::DeviceDescriptor {
                label: Some("wgpu Device"),
                // Enable SPIR-V passthrough on Linux
                #[cfg(target_os = "linux")]
                required_features: wgpu::Features::SPIRV_SHADER_PASSTHROUGH,
                // Default features elsewhere
                #[cfg(not(target_os = "linux"))]
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::default(),
                memory_hints: Default::default(),
            },
            None,
        )
        .await
        .context("Failed to create device")?;
    Ok((device, queue))
}

#[allow(clippy::too_many_arguments)]
fn find_shader_libraries(
    fs: &dyn FileSystem,
    dir: &Path,
    default_regex: &Regex,
    compute_output_regex: &Regex,
    dispatch_regex: &Regex,
    vertex_regex: &Regex,
    fragment_regex: &Regex,
    compute_regex: &Regex,
) -> Result<Vec<ShaderLibrary>> {
    let mut libraries = Vec::new();
    debug!("Reading directory {:?}", dir);
    for entry in fs
        .read_dir(dir)
        .with_context(|| format!("Reading directory {:?}", dir))?
    {
        let path = entry;
        if path.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
            match ShaderLibrary::load(
                fs,
                path.clone(),
                default_regex,
                compute_output_regex,
                dispatch_regex,
                vertex_regex,
                fragment_regex,
                compute_regex,
            ) {
                Ok(lib) => libraries.push(lib),
                Err(e) => error!("Error processing file {:?}: {:?}", path, e),
            }
        } else if path.is_dir() {
            if fs.exists(&path.join("Cargo.toml")) {
                match ShaderLibrary::load(
                    fs,
                    path.clone(),
                    default_regex,
                    compute_output_regex,
                    dispatch_regex,
                    vertex_regex,
                    fragment_regex,
                    compute_regex,
                ) {
                    Ok(lib) => libraries.push(lib),
                    Err(e) => error!("Error loading library in {:?}: {:?}", path, e),
                }
            } else {
                libraries.extend(find_shader_libraries(
                    fs,
                    &path,
                    default_regex,
                    compute_output_regex,
                    dispatch_regex,
                    vertex_regex,
                    fragment_regex,
                    compute_regex,
                )?);
            }
        }
    }
    Ok(libraries)
}

// --- Main as tester test harness ---

fn main() {
    // Initialize tracing.
    let subscriber = FmtSubscriber::builder()
        .with_max_level(tracing::Level::INFO)
        .finish();
    let _ = tracing::subscriber::set_global_default(subscriber);

    // Collect command-line arguments.
    let args: Vec<String> = std::env::args().collect();

    // Block on async initialization.
    let (device, queue) = match futures::executor::block_on(initialize_wgpu_async()) {
        Ok(pair) => pair,
        Err(e) => {
            eprintln!("Failed to initialize wgpu: {:?}", e);
            std::process::exit(1);
        }
    };
    let device = Arc::new(device);
    let queue = Arc::new(queue);

    // Setup regexes and filesystem.
    let fs = RealFs;
    let base_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    let pipelines_dir = base_dir.join("pipelines");
    if !fs.exists(&pipelines_dir) {
        eprintln!("The 'pipelines/' directory does not exist.");
        std::process::exit(1);
    }
    let default_regex = Regex::new(DEFAULT_REGEX_STR).unwrap();
    let compute_output_regex = Regex::new(COMPUTE_OUTPUT_REGEX_STR).unwrap();
    let dispatch_regex = Regex::new(DISPATCH_REGEX_STR).unwrap();
    let vertex_regex = Regex::new(SPIRV_VERTEX_REGEX).unwrap();
    let fragment_regex = Regex::new(SPIRV_FRAGMENT_REGEX).unwrap();
    let compute_regex = Regex::new(SPIRV_COMPUTE_REGEX).unwrap();

    // Only search in "compute", "graphics", and "mixed" subdirectories.
    let mut libraries = Vec::new();
    for sub in ["compute", "graphics", "mixed"].iter() {
        let dir = pipelines_dir.join(sub);
        if fs.exists(&dir) {
            libraries.extend(
                find_shader_libraries(
                    &fs,
                    &dir,
                    &default_regex,
                    &compute_output_regex,
                    &dispatch_regex,
                    &vertex_regex,
                    &fragment_regex,
                    &compute_regex,
                )
                .unwrap_or_else(|e| {
                    eprintln!("Error finding libraries in {:?}: {:?}", dir, e);
                    vec![]
                }),
            );
        }
    }

    // Create tester tests – one test per shader library.
    let mut tests = Vec::new();
    for lib in libraries {
        let test_name = format_lib_path(&lib.path);
        let lib_clone = lib;
        let device = Arc::clone(&device);
        let queue = Arc::clone(&queue);
        tests.push(tester::TestDescAndFn {
            desc: tester::TestDesc {
                name: tester::DynTestName(test_name.clone()),
                ignore: false,
                should_panic: tester::ShouldPanic::No,
                allow_fail: false,
                test_type: tester::TestType::UnitTest,
            },
            testfn: tester::TestFn::DynTestFn(Box::new(move || {
                // Run WGSL test.
                let runner = get_runner(lib_clone.pipeline_type);
                let out_wgsl = runner
                    .run(
                        &device,
                        &queue,
                        &lib_clone.shaders,
                        &lib_clone.config,
                        ShaderLanguage::Wgsl,
                        &lib_clone.path,
                    )
                    .expect("WGSL run failed");
                // Compile shaders to Rust (formerly spirv).
                let rust_shaders =
                    compile_spirv_shaders(&lib_clone).expect("Failed to compile SPIR-V shaders");
                let out_rust = runner
                    .run(
                        &device,
                        &queue,
                        &rust_shaders,
                        &lib_clone.config,
                        ShaderLanguage::Rust,
                        &lib_clone.path,
                    )
                    .expect("Rust run failed");
                let data_wgsl = fs::read(&out_wgsl).expect("Failed to read WGSL output file");
                let data_rust = fs::read(&out_rust).expect("Failed to read Rust output file");
                if data_wgsl != data_rust {
                    panic!(
                        "Output mismatch for {}:\nWGSL output: {:?}\nRust output: {:?}",
                        test_name, out_wgsl, out_rust
                    );
                }
            })),
        });
    }
    // Parse tester options.
    let opts = match tester::test::parse_opts(&args) {
        Some(Ok(o)) => o,
        Some(Err(e)) => {
            eprintln!("Error parsing test options: {}", e);
            std::process::exit(1);
        }
        None => tester::test::TestOpts {
            list: false,
            filters: vec![],
            filter_exact: false,
            force_run_in_process: false,
            exclude_should_panic: false,
            run_ignored: tester::RunIgnored::No,
            run_tests: true,
            bench_benchmarks: false,
            logfile: None,
            nocapture: false,
            color: tester::ColorConfig::AutoColor,
            format: tester::OutputFormat::Pretty,
            test_threads: Some(1),
            skip: vec![],
            time_options: None,
            options: tester::Options {
                display_output: true,
                panic_abort: true,
            },
        },
    };
    let passed = tester::run_tests_console(&opts, tests).expect("Failed to run tests");
    std::process::exit(if passed { 0 } else { 1 });
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        collections::{HashMap, HashSet},
        path::PathBuf,
    };

    fn init_tracing() {
        let _ = tracing_subscriber::fmt::try_init();
    }

    struct FakeFs {
        files: HashMap<PathBuf, String>,
        dirs: HashSet<PathBuf>,
    }
    impl FakeFs {
        fn new() -> Self {
            Self {
                files: HashMap::new(),
                dirs: HashSet::new(),
            }
        }
        fn add_file(&mut self, path: PathBuf, contents: String) {
            self.files.insert(path, contents);
        }
        fn add_dir(&mut self, path: PathBuf) {
            self.dirs.insert(path);
        }
    }
    impl FileSystem for FakeFs {
        fn read_dir(&self, path: &Path) -> Result<Vec<PathBuf>> {
            let parent_str = path.to_string_lossy();
            let mut entries = Vec::new();
            for file in self.files.keys() {
                if let Some(parent) = file.parent() {
                    if parent.to_string_lossy() == parent_str {
                        entries.push(file.clone());
                    }
                }
            }
            for dir in &self.dirs {
                if let Some(parent) = dir.parent() {
                    if parent.to_string_lossy() == parent_str {
                        entries.push(dir.clone());
                    }
                }
            }
            Ok(entries)
        }
        fn read_to_string(&self, path: &Path) -> Result<String> {
            self.files
                .get(path)
                .cloned()
                .ok_or_else(|| anyhow!("File not found: {:?}", path))
        }
        fn exists(&self, path: &Path) -> bool {
            self.files.contains_key(path) || self.dirs.contains(path)
        }
    }

    #[test]
    fn test_compute_output_type() {
        init_tracing();
        assert_eq!(
            ComputeOutputType::from_str("u32"),
            Some(ComputeOutputType::U32)
        );
        assert_eq!(
            ComputeOutputType::from_str("[u8]"),
            Some(ComputeOutputType::U8Slice)
        );
        assert_eq!(
            ComputeOutputType::from_str("f32"),
            Some(ComputeOutputType::F32)
        );
        assert_eq!(ComputeOutputType::from_str("unknown"), None);
    }

    #[test]
    fn test_config_parse() {
        init_tracing();
        let content = "\
            // default-vertex
            // output: [u8]
            // dispatch: 2, 3, 4
        ";
        let default_regex = Regex::new(DEFAULT_REGEX_STR).unwrap();
        let compute_output_regex = Regex::new(COMPUTE_OUTPUT_REGEX_STR).unwrap();
        let dispatch_regex = Regex::new(DISPATCH_REGEX_STR).unwrap();
        let config = Config::parse(
            content,
            &default_regex,
            &compute_output_regex,
            &dispatch_regex,
        )
        .unwrap();
        assert!(config.default_vertex);
        assert!(!config.default_fragment);
        assert_eq!(config.compute_output, ComputeOutputType::U8Slice);
        assert_eq!(config.dispatch, (2, 3, 4));
    }

    #[test]
    fn test_fake_fs_exists_and_read() {
        init_tracing();
        let mut fs = FakeFs::new();
        let file_path = PathBuf::from("a/b/file.txt");
        fs.add_file(file_path.clone(), "hello".to_string());
        fs.add_dir(PathBuf::from("a"));
        fs.add_dir(PathBuf::from("a/b"));
        assert!(fs.exists(&file_path));
        assert!(fs.exists(&PathBuf::from("a")));
        let contents = fs.read_to_string(&file_path).unwrap();
        assert_eq!(contents, "hello");
    }

    #[test]
    fn test_fake_fs_read_dir() {
        init_tracing();
        let mut fs = FakeFs::new();
        fs.add_dir(PathBuf::from("dir"));
        fs.add_file(PathBuf::from("dir/file1.txt"), "1".to_string());
        fs.add_file(PathBuf::from("dir/file2.txt"), "2".to_string());
        fs.add_dir(PathBuf::from("dir/subdir"));
        let entries = fs.read_dir(&PathBuf::from("dir")).unwrap();
        let mut names: Vec<_> = entries
            .iter()
            .map(|p| p.file_name().unwrap().to_string_lossy().into_owned())
            .collect();
        names.sort();
        assert_eq!(names, vec!["file1.txt", "file2.txt", "subdir"]);
    }

    #[test]
    fn test_find_shader_libraries_with_fake_fs() {
        init_tracing();
        let mut fs = FakeFs::new();

        fs.add_dir(PathBuf::from("pipelines"));
        fs.add_dir(PathBuf::from("pipelines/graphics"));
        fs.add_dir(PathBuf::from("pipelines/graphics/simple"));
        fs.add_file(
            PathBuf::from("pipelines/graphics/simple/Cargo.toml"),
            "dummy".to_string(),
        );
        fs.add_dir(PathBuf::from("pipelines/graphics/simple/src"));

        let lib_rs = "\
    #[spirv(vertex)]
    #[spirv(fragment)]
    // compile-flags: -Ctarget-feature=+FragmentFullyCoveredEXT, +ext:SPV_EXT_fragment_fully_covered
    // dispatch: 1, 1, 1
    ";

        fs.add_file(
            PathBuf::from("pipelines/graphics/simple/src/lib.rs"),
            lib_rs.to_string(),
        );

        fs.add_file(
            PathBuf::from("pipelines/graphics/simple/vertex.wgsl"),
            "vertex shader code".to_string(),
        );

        fs.add_file(
            PathBuf::from("pipelines/graphics/simple/fragment.wgsl"),
            "fragment shader code".to_string(),
        );

        let default_regex = Regex::new(DEFAULT_REGEX_STR).unwrap();
        let compute_output_regex = Regex::new(COMPUTE_OUTPUT_REGEX_STR).unwrap();
        let dispatch_regex = Regex::new(DISPATCH_REGEX_STR).unwrap();

        let spirv_vertex_regex = Regex::new(SPIRV_VERTEX_REGEX).unwrap();
        let spirv_fragment_regex = Regex::new(SPIRV_FRAGMENT_REGEX).unwrap();
        let spirv_compute_regex = Regex::new(SPIRV_COMPUTE_REGEX).unwrap();

        let libraries = find_shader_libraries(
            &fs,
            &PathBuf::from("pipelines"),
            &default_regex,
            &compute_output_regex,
            &dispatch_regex,
            &spirv_vertex_regex,
            &spirv_fragment_regex,
            &spirv_compute_regex,
        )
        .unwrap();

        assert_eq!(libraries.len(), 1);

        let lib = &libraries[0];
        assert_eq!(lib.pipeline_type, PipelineType::Graphics);

        assert_eq!(
            lib.shaders.get(&ShaderType::Vertex).unwrap(),
            &ShaderSource::Wgsl("vertex shader code".to_string())
        );

        assert_eq!(
            lib.shaders.get(&ShaderType::Fragment).unwrap(),
            &ShaderSource::Wgsl("fragment shader code".to_string())
        );

        let mut expected_flags = vec![
            "-Ctarget-feature=+FragmentFullyCoveredEXT".to_string(),
            "+ext:SPV_EXT_fragment_fully_covered".to_string(),
        ];

        expected_flags.sort();
        let mut actual_flags = lib.compile_flags.clone();
        actual_flags.sort();

        assert_eq!(expected_flags, actual_flags);
    }

    #[test]
    fn test_find_shader_libraries_only_in_allowed_dirs() {
        let mut fs = FakeFs::new();

        for sub in ["compute", "graphics", "mixed"].iter() {
            let base = PathBuf::from("pipelines").join(sub).join("test");
            fs.add_dir(base.join("src"));
            fs.add_file(base.join("Cargo.toml"), "dummy".to_string());
            let lib_rs = "\
                #[spirv(vertex)]
                #[spirv(fragment)]
                // compile-flags: -flag1, -flag2
                // dispatch: 1, 1, 1
            ";
            fs.add_file(base.join("src/lib.rs"), lib_rs.to_string());
            fs.add_file(base.join("vertex.wgsl"), "vertex shader".to_string());
            fs.add_file(base.join("fragment.wgsl"), "fragment shader".to_string());
        }

        let disallowed = PathBuf::from("pipelines").join("other").join("ignored");
        fs.add_dir(disallowed.join("src"));
        fs.add_file(disallowed.join("Cargo.toml"), "dummy".to_string());
        fs.add_file(disallowed.join("src/lib.rs"), "ignored".to_string());

        let default_regex = Regex::new(DEFAULT_REGEX_STR).unwrap();
        let compute_output_regex = Regex::new(COMPUTE_OUTPUT_REGEX_STR).unwrap();
        let dispatch_regex = Regex::new(DISPATCH_REGEX_STR).unwrap();
        let vertex_regex = Regex::new(SPIRV_VERTEX_REGEX).unwrap();
        let fragment_regex = Regex::new(SPIRV_FRAGMENT_REGEX).unwrap();
        let compute_regex = Regex::new(SPIRV_COMPUTE_REGEX).unwrap();

        let mut libraries = Vec::new();
        for sub in ["compute", "graphics", "mixed"].iter() {
            let dir = PathBuf::from("pipelines").join(sub);
            if fs.exists(&dir) {
                libraries.extend(
                    find_shader_libraries(
                        &fs,
                        &dir,
                        &default_regex,
                        &compute_output_regex,
                        &dispatch_regex,
                        &vertex_regex,
                        &fragment_regex,
                        &compute_regex,
                    )
                    .unwrap(),
                );
            }
        }
        assert_eq!(libraries.len(), 3);
        for lib in libraries {
            assert!(
                lib.path.to_string_lossy().contains("/compute/")
                    || lib.path.to_string_lossy().contains("/graphics/")
                    || lib.path.to_string_lossy().contains("/mixed/")
            );
        }
    }
}
