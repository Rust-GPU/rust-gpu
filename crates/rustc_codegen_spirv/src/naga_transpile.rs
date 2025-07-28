use crate::codegen_cx::CodegenArgs;
use crate::target::{NagaTarget, SpirvTarget};
use rustc_session::Session;
use rustc_span::ErrorGuaranteed;
use std::path::Path;

pub type NagaTranspile = fn(
    sess: &Session,
    cg_args: &CodegenArgs,
    spv_binary: &[u32],
    out_filename: &Path,
) -> Result<(), ErrorGuaranteed>;

pub fn should_transpile(sess: &Session) -> Option<NagaTranspile> {
    let target = SpirvTarget::parse_target(sess.opts.target_triple.tuple())
        .expect("parsing should fail earlier");
    match target {
        SpirvTarget::Naga(NagaTarget::NAGA_WGSL) => Some(transpile::wgsl_transpile),
        _ => None,
    }
}

mod transpile {
    use crate::codegen_cx::CodegenArgs;
    use naga::error::ShaderError;
    use naga::valid::Capabilities;
    use rustc_session::Session;
    use rustc_span::ErrorGuaranteed;
    use std::path::Path;

    pub fn wgsl_transpile(
        sess: &Session,
        _cg_args: &CodegenArgs,
        spv_binary: &[u32],
        out_filename: &Path,
    ) -> Result<(), ErrorGuaranteed> {
        // these should be params via spirv-builder
        let opts = naga::front::spv::Options::default();
        let capabilities = Capabilities::default();
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

        let wgsl_dst = out_filename.with_extension("wgsl");
        let wgsl = naga::back::wgsl::write_string(&module, &info, writer_flags).map_err(|err| {
            sess.dcx()
                .err(format!("Naga failed to write wgsl : \n{err}"))
        })?;

        std::fs::write(&wgsl_dst, wgsl).map_err(|err| {
            sess.dcx()
                .err(format!("failed to write wgsl to file: {err}"))
        })?;

        Ok(())
    }
}
