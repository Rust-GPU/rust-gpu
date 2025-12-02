use crate::device::MyDevice;
use crate::single_command_buffer::SingleCommandBuffer;
use crate::swapchain::DrawFrame;
use anyhow::Context;
use ash::vk;
use shared::ShaderConstants;
use std::sync::Arc;

/// Manages the creation and recreation of [`MyRenderPipeline`], whenever new shader code ([`Self::set_shader_code`])
/// is submitted or the spec constant is changed ([`Self::set_sky_fs_sun_intensity_factor`])
pub struct MyRenderPipelineManager {
    pub device: Arc<MyDevice>,
    color_out_format: vk::Format,
    shader_code: Vec<u32>,
    pipeline: Option<MyRenderPipeline>,

    // Only used for sky-shader.
    // NOTE(eddyb) this acts like an integration test for specialization constants.
    sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor: u32,
    should_recreate: bool,
}

pub struct MyRenderPipeline {
    pub pipeline: vk::Pipeline,
    pub pipeline_layout: vk::PipelineLayout,
}

impl MyRenderPipelineManager {
    pub fn new(
        device: Arc<MyDevice>,
        color_out_format: vk::Format,
        shader_code: Vec<u32>,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            device,
            color_out_format,
            shader_code,
            pipeline: None,
            sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor: 100,
            should_recreate: true,
        })
    }

    #[inline]
    pub fn set_sky_fs_sun_intensity_factor(&mut self, factor: u32) {
        self.sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor = factor;
        self.should_recreate();
    }

    #[inline]
    pub fn get_sky_fs_sun_intensity_factor(&self) -> u32 {
        self.sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor
    }

    #[inline]
    pub fn set_shader_code(&mut self, shader_code: Vec<u32>) {
        self.shader_code = shader_code;
        self.should_recreate();
    }

    #[inline]
    pub fn should_recreate(&mut self) {
        self.should_recreate = true;
    }

    pub fn get_pipeline(&mut self) -> anyhow::Result<&MyRenderPipeline> {
        if self.should_recreate {
            self.rebuild_pipeline()?;
        }
        Ok(self.pipeline.as_ref().unwrap())
    }

    /// Update shaders and rebuild the pipeline
    fn rebuild_pipeline(&mut self) -> anyhow::Result<()> {
        unsafe {
            self.destroy_pipeline()?;

            let shader_module = self.device.create_shader_module(
                &vk::ShaderModuleCreateInfo::default().code(&self.shader_code),
                None,
            )?;

            let pipeline_layout = self.device.create_pipeline_layout(
                &vk::PipelineLayoutCreateInfo::default().push_constant_ranges(&[
                    vk::PushConstantRange::default()
                        .offset(0)
                        .size(size_of::<ShaderConstants>() as u32)
                        .stage_flags(vk::ShaderStageFlags::ALL),
                ]),
                None,
            )?;

            let mut pipelines =
                self
                    .device
                    .create_graphics_pipelines(vk::PipelineCache::null(), &[vk::GraphicsPipelineCreateInfo::default()
                        .stages(
                            &[
                                vk::PipelineShaderStageCreateInfo {
                                    module: shader_module,
                                    p_name: c"main_vs".as_ptr(),
                                    stage: vk::ShaderStageFlags::VERTEX,
                                    ..Default::default()
                                },
                                vk::PipelineShaderStageCreateInfo {
                                    module: shader_module,
                                    p_name: c"main_fs".as_ptr(),
                                    stage: vk::ShaderStageFlags::FRAGMENT,
                                    // NOTE(eddyb) this acts like an integration test for specialization constants.
                                    p_specialization_info: &vk::SpecializationInfo::default()
                                        .map_entries(&[vk::SpecializationMapEntry::default()
                                            .constant_id(0x5007)
                                            .offset(0)
                                            .size(4)])
                                        .data(&u32::to_le_bytes(
                                            self.sky_fs_spec_id_0x5007_sun_intensity_extra_spec_const_factor,
                                        )),
                                    ..Default::default()
                                },
                            ],
                        )
                        .vertex_input_state(&vk::PipelineVertexInputStateCreateInfo::default())
                        .input_assembly_state(&vk::PipelineInputAssemblyStateCreateInfo {
                            topology: vk::PrimitiveTopology::TRIANGLE_LIST,
                            ..Default::default()
                        })
                        .rasterization_state(&vk::PipelineRasterizationStateCreateInfo {
                            front_face: vk::FrontFace::COUNTER_CLOCKWISE,
                            line_width: 1.0,
                            ..Default::default()
                        })
                        .multisample_state(&vk::PipelineMultisampleStateCreateInfo {
                            rasterization_samples: vk::SampleCountFlags::TYPE_1,
                            ..Default::default()
                        })
                        .depth_stencil_state(&vk::PipelineDepthStencilStateCreateInfo::default())
                        .color_blend_state(
                            &vk::PipelineColorBlendStateCreateInfo::default()
                                .attachments(
                                    &[vk::PipelineColorBlendAttachmentState {
                                        blend_enable: 0,
                                        src_color_blend_factor: vk::BlendFactor::SRC_COLOR,
                                        dst_color_blend_factor: vk::BlendFactor::ONE_MINUS_DST_COLOR,
                                        color_blend_op: vk::BlendOp::ADD,
                                        src_alpha_blend_factor: vk::BlendFactor::ZERO,
                                        dst_alpha_blend_factor: vk::BlendFactor::ZERO,
                                        alpha_blend_op: vk::BlendOp::ADD,
                                        color_write_mask: vk::ColorComponentFlags::RGBA,
                                    }],
                                ),
                        )
                        .dynamic_state(
                            &vk::PipelineDynamicStateCreateInfo::default()
                                .dynamic_states(&[vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR]),
                        )
                        .viewport_state(
                            &vk::PipelineViewportStateCreateInfo::default()
                                .scissor_count(1)
                                .viewport_count(1),
                        )
                        .layout(pipeline_layout)
                        .push_next(&mut vk::PipelineRenderingCreateInfo::default().color_attachment_formats(&[self.color_out_format]))
                    ], None).map_err(|(_, e)| e)
                    .context("Unable to create graphics pipeline")?;

            // A single `pipeline_info` results in a single pipeline.
            assert_eq!(pipelines.len(), 1);
            self.pipeline = pipelines.pop().map(|pipeline| MyRenderPipeline {
                pipeline,
                pipeline_layout,
            });

            // shader modules are allowed to be deleted after the pipeline has been created
            self.device.destroy_shader_module(shader_module, None);
            Ok(())
        }
    }

    unsafe fn destroy_pipeline(&mut self) -> anyhow::Result<()> {
        unsafe {
            if let Some(pipeline) = self.pipeline.take() {
                // Figuring out when the pipeline stops being used is hard, so we take this shortcut
                self.device.device_wait_idle()?;

                self.device.destroy_pipeline(pipeline.pipeline, None);
                self.device
                    .destroy_pipeline_layout(pipeline.pipeline_layout, None);
            }
            Ok(())
        }
    }
}

impl MyRenderPipeline {
    pub fn render(
        &self,
        device: &MyDevice,
        cmd: vk::CommandBuffer,
        color_out: vk::ImageView,
        extent: vk::Extent2D,
        push_constants: ShaderConstants,
    ) -> anyhow::Result<()> {
        unsafe {
            let render_area = vk::Rect2D {
                offset: vk::Offset2D::default(),
                extent,
            };

            device.cmd_begin_rendering(
                cmd,
                &vk::RenderingInfo::default()
                    .render_area(render_area)
                    .layer_count(1)
                    .color_attachments(&[vk::RenderingAttachmentInfo::default()
                        .image_view(color_out)
                        .load_op(vk::AttachmentLoadOp::CLEAR)
                        .store_op(vk::AttachmentStoreOp::STORE)
                        .clear_value(vk::ClearValue {
                            color: vk::ClearColorValue {
                                float32: [0.0, 1.0, 0.0, 0.0],
                            },
                        })
                        .image_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)]),
            );
            device.cmd_bind_pipeline(cmd, vk::PipelineBindPoint::GRAPHICS, self.pipeline);
            device.cmd_set_viewport(
                cmd,
                0,
                &[vk::Viewport {
                    // contains a y-flip
                    x: 0.0,
                    y: extent.height as f32,
                    width: extent.width as f32,
                    height: -(extent.height as f32),
                    min_depth: 0.0,
                    max_depth: 1.0,
                }],
            );
            device.cmd_set_scissor(cmd, 0, &[render_area]);
            device.cmd_push_constants(
                cmd,
                self.pipeline_layout,
                vk::ShaderStageFlags::ALL,
                0,
                bytemuck::bytes_of(&push_constants),
            );
            device.cmd_draw(cmd, 3, 1, 0, 0);
            device.cmd_end_rendering(cmd);
            Ok(())
        }
    }
}

impl Drop for MyRenderPipelineManager {
    fn drop(&mut self) {
        unsafe {
            self.destroy_pipeline().ok();
        }
    }
}

/// The renderer manages our command buffer and submits the commands, using [`MyRenderPipeline`] for drawing.
pub struct MyRenderer {
    pub device: Arc<MyDevice>,
    pub pipeline: MyRenderPipelineManager,
    pub command: SingleCommandBuffer,
}

impl MyRenderer {
    pub fn new(pipeline: MyRenderPipelineManager) -> anyhow::Result<Self> {
        Ok(Self {
            command: SingleCommandBuffer::new(pipeline.device.clone())?,
            device: pipeline.device.clone(),
            pipeline,
        })
    }

    pub fn render_frame(
        &mut self,
        frame: DrawFrame,
        push_constants: ShaderConstants,
    ) -> anyhow::Result<()> {
        unsafe {
            let device = &self.device;
            let pipeline = self.pipeline.get_pipeline()?;
            let cmd = self.command.cmd;

            device.reset_command_pool(self.command.pool, vk::CommandPoolResetFlags::default())?;

            {
                device.begin_command_buffer(
                    cmd,
                    &vk::CommandBufferBeginInfo::default()
                        .flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT),
                )?;
                device.cmd_pipeline_barrier2(
                    cmd,
                    &vk::DependencyInfo::default().image_memory_barriers(&[
                        vk::ImageMemoryBarrier2::default()
                            .image(frame.image)
                            .src_access_mask(vk::AccessFlags2::NONE)
                            .src_stage_mask(vk::PipelineStageFlags2::ALL_COMMANDS)
                            .old_layout(vk::ImageLayout::UNDEFINED)
                            .dst_access_mask(vk::AccessFlags2::COLOR_ATTACHMENT_WRITE)
                            .dst_stage_mask(vk::PipelineStageFlags2::COLOR_ATTACHMENT_OUTPUT)
                            .new_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
                            .subresource_range(
                                vk::ImageSubresourceRange::default()
                                    .aspect_mask(vk::ImageAspectFlags::COLOR)
                                    .base_mip_level(0)
                                    .level_count(1)
                                    .base_array_layer(0)
                                    .layer_count(1),
                            ),
                    ]),
                );
                pipeline.render(device, cmd, frame.image_view, frame.extent, push_constants)?;
                device.cmd_pipeline_barrier2(
                    cmd,
                    &vk::DependencyInfo::default().image_memory_barriers(&[
                        vk::ImageMemoryBarrier2::default()
                            .image(frame.image)
                            .src_access_mask(vk::AccessFlags2::COLOR_ATTACHMENT_WRITE)
                            .src_stage_mask(vk::PipelineStageFlags2::COLOR_ATTACHMENT_OUTPUT)
                            .old_layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL)
                            .dst_access_mask(vk::AccessFlags2::NONE)
                            .dst_stage_mask(vk::PipelineStageFlags2::ALL_COMMANDS)
                            .new_layout(vk::ImageLayout::PRESENT_SRC_KHR)
                            .subresource_range(
                                vk::ImageSubresourceRange::default()
                                    .aspect_mask(vk::ImageAspectFlags::COLOR)
                                    .base_mip_level(0)
                                    .level_count(1)
                                    .base_array_layer(0)
                                    .layer_count(1),
                            ),
                    ]),
                );
                device.end_command_buffer(cmd)?;
            }

            device.queue_submit2(
                device.main_queue,
                &[vk::SubmitInfo2::default()
                    .wait_semaphore_infos(&[vk::SemaphoreSubmitInfo::default()
                        .semaphore(frame.acquire_semaphore)
                        .stage_mask(vk::PipelineStageFlags2::TOP_OF_PIPE)])
                    .command_buffer_infos(&[
                        vk::CommandBufferSubmitInfo::default().command_buffer(cmd)
                    ])
                    .signal_semaphore_infos(&[vk::SemaphoreSubmitInfo::default()
                        .semaphore(frame.draw_finished_semaphore)
                        .stage_mask(vk::PipelineStageFlags2::BOTTOM_OF_PIPE)])],
                frame.draw_finished_fence,
            )?;
            Ok(())
        }
    }
}
