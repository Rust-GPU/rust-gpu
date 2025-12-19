//! This crate contains code to print the integer sequence at <https://oeis.org/A006877>:
//!
//! > In the '3x+1' problem, these values for the starting value set new records for number of
//! > steps to reach 1.
//!
//! each starting value is paired with the number of steps to reach 1 (a.k.a the "Collatz
//! sequence length").
//!
//! The individual Collatz sequence lengths are computed via a computer shader on the GPU.

use crate::{CompiledShaderModules, Options, maybe_watch};
use std::borrow::Cow;

use std::time::Duration;
use wgpu::util::DeviceExt;
use wgpu::{ExperimentalFeatures, ShaderSource};

pub fn start(options: &Options) {
    env_logger::init();

    let compiled_shader_modules = maybe_watch(options, None);

    futures::executor::block_on(start_internal(options, compiled_shader_modules));
}

async fn start_internal(options: &Options, compiled_shader_modules: CompiledShaderModules) {
    let backends = wgpu::Backends::from_env().unwrap_or(wgpu::Backends::PRIMARY);
    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
        backends,
        ..Default::default()
    });
    let adapter = wgpu::util::initialize_adapter_from_env_or_default(&instance, None)
        .await
        .expect("Failed to find an appropriate adapter");

    // Timestamping may not be supported
    let timestamping = adapter.features().contains(wgpu::Features::TIMESTAMP_QUERY)
        && adapter
            .features()
            .contains(wgpu::Features::TIMESTAMP_QUERY_INSIDE_PASSES);

    let mut required_features = if timestamping {
        wgpu::Features::TIMESTAMP_QUERY | wgpu::Features::TIMESTAMP_QUERY_INSIDE_PASSES
    } else {
        wgpu::Features::empty()
    };
    if !timestamping {
        eprintln!(
            "Adapter reports that timestamping is not supported - no timing information will be available"
        );
    }
    if options.force_spirv_passthru {
        required_features |= wgpu::Features::EXPERIMENTAL_PASSTHROUGH_SHADERS;
    }

    let (device, queue) = adapter
        .request_device(&wgpu::DeviceDescriptor {
            label: None,
            required_features,
            required_limits: wgpu::Limits::default(),
            experimental_features: if options.force_spirv_passthru {
                unsafe { ExperimentalFeatures::enabled() }
            } else {
                ExperimentalFeatures::disabled()
            },
            memory_hints: wgpu::MemoryHints::Performance,
            trace: Default::default(),
        })
        .await
        .expect("Failed to create device");
    drop(instance);
    drop(adapter);

    let timestamp_period: Option<f32> = if timestamping {
        Some(queue.get_timestamp_period())
    } else {
        None
    };
    let entry_point = "main_cs";

    // FIXME(eddyb) automate this decision by default.
    let module = compiled_shader_modules.spv_module_for_entry_point(entry_point);
    let module = if options.force_spirv_passthru {
        unsafe {
            let spirv = match &module.source {
                ShaderSource::SpirV(spirv) => spirv,
                _ => unreachable!(),
            };
            device.create_shader_module_passthrough(wgpu::ShaderModuleDescriptorPassthrough {
                label: module.label,
                spirv: Some(Cow::Borrowed(spirv)),
                ..Default::default()
            })
        }
    } else {
        device.create_shader_module(module)
    };

    let top = 2u32.pow(20);
    let src_range = 1..top;

    let src = src_range
        .clone()
        .flat_map(u32::to_ne_bytes)
        .collect::<Vec<_>>();

    let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: None,
        entries: &[wgpu::BindGroupLayoutEntry {
            binding: 0,
            count: None,
            visibility: wgpu::ShaderStages::COMPUTE,
            ty: wgpu::BindingType::Buffer {
                has_dynamic_offset: false,
                min_binding_size: None,
                ty: wgpu::BufferBindingType::Storage { read_only: false },
            },
        }],
    });

    let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: None,
        bind_group_layouts: &[&bind_group_layout],
        push_constant_ranges: &[],
    });

    let compute_pipeline = device.create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
        compilation_options: Default::default(),
        cache: None,
        label: None,
        layout: Some(&pipeline_layout),
        module: &module,
        entry_point: Some(entry_point),
    });

    let readback_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: None,
        size: src.len() as wgpu::BufferAddress,
        // Can be read to the CPU, and can be copied from the shader's storage buffer
        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    });

    let storage_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("Collatz Conjecture Input"),
        contents: &src,
        usage: wgpu::BufferUsages::STORAGE
            | wgpu::BufferUsages::COPY_DST
            | wgpu::BufferUsages::COPY_SRC,
    });

    let (timestamp_buffer, timestamp_readback_buffer) = if timestamping {
        let timestamp_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Timestamps buffer"),
            size: 16,
            usage: wgpu::BufferUsages::QUERY_RESOLVE | wgpu::BufferUsages::COPY_SRC,
            mapped_at_creation: false,
        });

        let timestamp_readback_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: 16,
            usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: true,
        });
        timestamp_readback_buffer.unmap();

        (Some(timestamp_buffer), Some(timestamp_readback_buffer))
    } else {
        (None, None)
    };

    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: None,
        layout: &bind_group_layout,
        entries: &[wgpu::BindGroupEntry {
            binding: 0,
            resource: storage_buffer.as_entire_binding(),
        }],
    });

    let queries = if timestamping {
        Some(device.create_query_set(&wgpu::QuerySetDescriptor {
            label: None,
            count: 2,
            ty: wgpu::QueryType::Timestamp,
        }))
    } else {
        None
    };

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });

    {
        let mut cpass = encoder.begin_compute_pass(&Default::default());
        cpass.set_bind_group(0, &bind_group, &[]);
        cpass.set_pipeline(&compute_pipeline);
        if timestamping && let Some(queries) = queries.as_ref() {
            cpass.write_timestamp(queries, 0);
        }
        cpass.dispatch_workgroups(src_range.len() as u32 / 64, 1, 1);
        if timestamping && let Some(queries) = queries.as_ref() {
            cpass.write_timestamp(queries, 1);
        }
    }

    encoder.copy_buffer_to_buffer(
        &storage_buffer,
        0,
        &readback_buffer,
        0,
        src.len() as wgpu::BufferAddress,
    );

    if timestamping
        && let (Some(queries), Some(timestamp_buffer), Some(timestamp_readback_buffer)) = (
            queries.as_ref(),
            timestamp_buffer.as_ref(),
            timestamp_readback_buffer.as_ref(),
        )
    {
        encoder.resolve_query_set(queries, 0..2, timestamp_buffer, 0);
        encoder.copy_buffer_to_buffer(
            timestamp_buffer,
            0,
            timestamp_readback_buffer,
            0,
            timestamp_buffer.size(),
        );
    }

    queue.submit(Some(encoder.finish()));
    let buffer_slice = readback_buffer.slice(..);
    if timestamping && let Some(timestamp_readback_buffer) = timestamp_readback_buffer.as_ref() {
        let timestamp_slice = timestamp_readback_buffer.slice(..);
        timestamp_slice.map_async(wgpu::MapMode::Read, |r| r.unwrap());
    }
    buffer_slice.map_async(wgpu::MapMode::Read, |r| r.unwrap());
    // NOTE(eddyb) `poll` should return only after the above callbacks fire
    // (see also https://github.com/gfx-rs/wgpu/pull/2698 for more details).
    device.poll(wgpu::PollType::wait_indefinitely()).unwrap();

    if timestamping
        && let (Some(timestamp_readback_buffer), Some(timestamp_period)) =
            (timestamp_readback_buffer.as_ref(), timestamp_period)
    {
        {
            let timing_data = timestamp_readback_buffer.slice(..).get_mapped_range();
            let timings = timing_data
                .chunks_exact(8)
                .map(|b| u64::from_ne_bytes(b.try_into().unwrap()))
                .collect::<Vec<_>>();

            println!(
                "Took: {:?}",
                Duration::from_nanos(
                    ((timings[1] - timings[0]) as f64 * f64::from(timestamp_period)) as u64
                )
            );
            drop(timing_data);
            timestamp_readback_buffer.unmap();
        }
    }

    let data = buffer_slice.get_mapped_range();
    let result = data
        .chunks_exact(4)
        .map(|b| u32::from_ne_bytes(b.try_into().unwrap()))
        .collect::<Vec<_>>();
    drop(data);
    readback_buffer.unmap();

    // Our `max` computation excludes `1`, so print that manually as the first number in the
    // sequence.
    println!("1: 0");
    let mut max = 0;
    for (src, out) in src_range.zip(result.iter().copied()) {
        if out == u32::MAX {
            println!("{src}: overflowed");
            break;
        } else if out > max {
            max = out;
            println!("{src}: {out}");
        }
    }
}
