use crate::codegen_cx::CodegenArgs;
use crate::target::{NagaTarget, SpirvTarget};
use rustc_session::Session;
use rustc_span::ErrorGuaranteed;

pub fn transpile(
    sess: &Session,
    cg_args: &CodegenArgs,
    spv_binary: &[u32],
) -> Result<Vec<u8>, ErrorGuaranteed> {
    let target = SpirvTarget::parse_target(sess.opts.target_triple.tuple())
        .expect("parsing should fail earlier");
    match target {
        #[cfg(feature = "naga")]
        SpirvTarget::Naga(NagaTarget::NAGA_WGSL) => {
            transpile::wgsl_transpile(sess, cg_args, spv_binary)
        }
        #[cfg(not(feature = "naga"))]
        SpirvTarget::Naga(_) => Err(sess.dcx().err(format!(
            "Target `{}` requires feature \"naga\" on rustc_codegen_spirv",
            target.target()
        ))),
        _ => Ok(bytemuck::cast_slice::<_, u8>(spv_binary).to_vec()),
    }
}

#[cfg(feature = "naga")]
mod transpile {
    use crate::codegen_cx::CodegenArgs;
    use naga::error::ShaderError;
    use naga::valid::Capabilities;
    use rustc_session::Session;
    use rustc_span::ErrorGuaranteed;

    pub fn wgsl_transpile(
        sess: &Session,
        _cg_args: &CodegenArgs,
        spv_binary: &[u32],
    ) -> Result<Vec<u8>, ErrorGuaranteed> {
        // these should be params via spirv-builder
        let opts = naga::front::spv::Options::default();
        let capabilities = Capabilities::all();
        let writer_flags = naga::back::wgsl::WriterFlags::empty();

        let module = naga::front::spv::parse_u8_slice(bytemuck::cast_slice(spv_binary), &opts)
            .map_err(|err| {
                sess.dcx().err(format!(
                    "Naga failed to parse spv: \n{}",
                    ShaderError {
                        source: String::new(),
                        label: None,
                        inner: Box::new(err),
                    }
                ))
            })?;
        let mut validator =
            naga::valid::Validator::new(naga::valid::ValidationFlags::default(), capabilities);
        let info = validator.validate(&module).map_err(|err| {
            sess.dcx().err(format!(
                "Naga validation failed: \n{}",
                ShaderError {
                    source: String::new(),
                    label: None,
                    inner: Box::new(err),
                }
            ))
        })?;

        let wgsl = naga::back::wgsl::write_string(&module, &info, writer_flags).map_err(|err| {
            sess.dcx()
                .err(format!("Naga failed to write wgsl : \n{err}"))
        })?;
        Ok(wgsl.into_bytes())
    }
}
