(function() {var implementors = {
"arrayvec":[["impl&lt;T, const CAP:&nbsp;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.usize.html\">usize</a>&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.slice.html\">[T]</a>&gt; for <a class=\"struct\" href=\"arrayvec/struct.ArrayVec.html\" title=\"struct arrayvec::ArrayVec\">ArrayVec</a>&lt;T, CAP&gt;"],["impl&lt;const CAP:&nbsp;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.usize.html\">usize</a>&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.str.html\">str</a>&gt; for <a class=\"struct\" href=\"arrayvec/struct.ArrayString.html\" title=\"struct arrayvec::ArrayString\">ArrayString</a>&lt;CAP&gt;"]],
"either":[["impl&lt;L, R, Target&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;Target&gt; for <a class=\"enum\" href=\"either/enum.Either.html\" title=\"enum either::Either\">Either</a>&lt;L, R&gt;<span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;L: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;Target&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;R: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;Target&gt;,</span>"],["impl&lt;L, R&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.str.html\">str</a>&gt; for <a class=\"enum\" href=\"either/enum.Either.html\" title=\"enum either::Either\">Either</a>&lt;L, R&gt;<span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;L: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.str.html\">str</a>&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;R: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.str.html\">str</a>&gt;,</span>"],["impl&lt;L, R&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/path/struct.Path.html\" title=\"struct std::path::Path\">Path</a>&gt; for <a class=\"enum\" href=\"either/enum.Either.html\" title=\"enum either::Either\">Either</a>&lt;L, R&gt;<span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;L: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/path/struct.Path.html\" title=\"struct std::path::Path\">Path</a>&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;R: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/path/struct.Path.html\" title=\"struct std::path::Path\">Path</a>&gt;,</span>"],["impl&lt;L, R&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/ffi/os_str/struct.OsStr.html\" title=\"struct std::ffi::os_str::OsStr\">OsStr</a>&gt; for <a class=\"enum\" href=\"either/enum.Either.html\" title=\"enum either::Either\">Either</a>&lt;L, R&gt;<span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;L: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/ffi/os_str/struct.OsStr.html\" title=\"struct std::ffi::os_str::OsStr\">OsStr</a>&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;R: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/ffi/os_str/struct.OsStr.html\" title=\"struct std::ffi::os_str::OsStr\">OsStr</a>&gt;,</span>"],["impl&lt;L, R&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/core/ffi/c_str/struct.CStr.html\" title=\"struct core::ffi::c_str::CStr\">CStr</a>&gt; for <a class=\"enum\" href=\"either/enum.Either.html\" title=\"enum either::Either\">Either</a>&lt;L, R&gt;<span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;L: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/core/ffi/c_str/struct.CStr.html\" title=\"struct core::ffi::c_str::CStr\">CStr</a>&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;R: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/core/ffi/c_str/struct.CStr.html\" title=\"struct core::ffi::c_str::CStr\">CStr</a>&gt;,</span>"],["impl&lt;L, R, Target&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.slice.html\">[Target]</a>&gt; for <a class=\"enum\" href=\"either/enum.Either.html\" title=\"enum either::Either\">Either</a>&lt;L, R&gt;<span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;L: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.slice.html\">[Target]</a>&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;R: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.slice.html\">[Target]</a>&gt;,</span>"]],
"glam":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f32.html\">f32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">9</a>]&gt; for <a class=\"struct\" href=\"glam/f32/struct.Mat3.html\" title=\"struct glam::f32::Mat3\">Mat3</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f32.html\">f32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">2</a>]&gt; for <a class=\"struct\" href=\"glam/f32/struct.Vec2.html\" title=\"struct glam::f32::Vec2\">Vec2</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f32.html\">f32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">3</a>]&gt; for <a class=\"struct\" href=\"glam/f32/struct.Vec3.html\" title=\"struct glam::f32::Vec3\">Vec3</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f32.html\">f32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">4</a>]&gt; for <a class=\"struct\" href=\"glam/f32/struct.Mat2.html\" title=\"struct glam::f32::Mat2\">Mat2</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f32.html\">f32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">16</a>]&gt; for <a class=\"struct\" href=\"glam/f32/struct.Mat4.html\" title=\"struct glam::f32::Mat4\">Mat4</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f32.html\">f32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">4</a>]&gt; for <a class=\"struct\" href=\"glam/f32/struct.Quat.html\" title=\"struct glam::f32::Quat\">Quat</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f32.html\">f32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">3</a>]&gt; for <a class=\"struct\" href=\"glam/f32/struct.Vec3A.html\" title=\"struct glam::f32::Vec3A\">Vec3A</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f32.html\">f32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">4</a>]&gt; for <a class=\"struct\" href=\"glam/f32/struct.Vec4.html\" title=\"struct glam::f32::Vec4\">Vec4</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f64.html\">f64</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">4</a>]&gt; for <a class=\"struct\" href=\"glam/f64/struct.DMat2.html\" title=\"struct glam::f64::DMat2\">DMat2</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f64.html\">f64</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">9</a>]&gt; for <a class=\"struct\" href=\"glam/f64/struct.DMat3.html\" title=\"struct glam::f64::DMat3\">DMat3</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f64.html\">f64</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">16</a>]&gt; for <a class=\"struct\" href=\"glam/f64/struct.DMat4.html\" title=\"struct glam::f64::DMat4\">DMat4</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f64.html\">f64</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">4</a>]&gt; for <a class=\"struct\" href=\"glam/f64/struct.DQuat.html\" title=\"struct glam::f64::DQuat\">DQuat</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f64.html\">f64</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">2</a>]&gt; for <a class=\"struct\" href=\"glam/f64/struct.DVec2.html\" title=\"struct glam::f64::DVec2\">DVec2</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f64.html\">f64</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">3</a>]&gt; for <a class=\"struct\" href=\"glam/f64/struct.DVec3.html\" title=\"struct glam::f64::DVec3\">DVec3</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.f64.html\">f64</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">4</a>]&gt; for <a class=\"struct\" href=\"glam/f64/struct.DVec4.html\" title=\"struct glam::f64::DVec4\">DVec4</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.i32.html\">i32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">2</a>]&gt; for <a class=\"struct\" href=\"glam/i32/struct.IVec2.html\" title=\"struct glam::i32::IVec2\">IVec2</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.i32.html\">i32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">3</a>]&gt; for <a class=\"struct\" href=\"glam/i32/struct.IVec3.html\" title=\"struct glam::i32::IVec3\">IVec3</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.i32.html\">i32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">4</a>]&gt; for <a class=\"struct\" href=\"glam/i32/struct.IVec4.html\" title=\"struct glam::i32::IVec4\">IVec4</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.u32.html\">u32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">2</a>]&gt; for <a class=\"struct\" href=\"glam/u32/struct.UVec2.html\" title=\"struct glam::u32::UVec2\">UVec2</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.u32.html\">u32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">3</a>]&gt; for <a class=\"struct\" href=\"glam/u32/struct.UVec3.html\" title=\"struct glam::u32::UVec3\">UVec3</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.u32.html\">u32</a>; <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/core/primitive.array.html\">4</a>]&gt; for <a class=\"struct\" href=\"glam/u32/struct.UVec4.html\" title=\"struct glam::u32::UVec4\">UVec4</a>"]],
"raw_string":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"raw_string/struct.RawStr.html\" title=\"struct raw_string::RawStr\">RawStr</a>&gt; for <a class=\"struct\" href=\"raw_string/struct.RawStr.html\" title=\"struct raw_string::RawStr\">RawStr</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"raw_string/struct.RawStr.html\" title=\"struct raw_string::RawStr\">RawStr</a>&gt; for [<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u8.html\">u8</a>]"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"raw_string/struct.RawStr.html\" title=\"struct raw_string::RawStr\">RawStr</a>&gt; for <a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.str.html\">str</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u8.html\">u8</a>]&gt; for <a class=\"struct\" href=\"raw_string/struct.RawStr.html\" title=\"struct raw_string::RawStr\">RawStr</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"raw_string/struct.RawStr.html\" title=\"struct raw_string::RawStr\">RawStr</a>&gt; for <a class=\"struct\" href=\"raw_string/struct.RawString.html\" title=\"struct raw_string::RawString\">RawString</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u8.html\">u8</a>]&gt; for <a class=\"struct\" href=\"raw_string/struct.RawString.html\" title=\"struct raw_string::RawString\">RawString</a>"]],
"regex_syntax":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u8.html\">u8</a>]&gt; for <a class=\"struct\" href=\"regex_syntax/hir/literal/struct.Literal.html\" title=\"struct regex_syntax::hir::literal::Literal\">Literal</a>"]],
"smallvec":[["impl&lt;A:&nbsp;<a class=\"trait\" href=\"smallvec/trait.Array.html\" title=\"trait smallvec::Array\">Array</a>&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[&lt;A as <a class=\"trait\" href=\"smallvec/trait.Array.html\" title=\"trait smallvec::Array\">Array</a>&gt;::<a class=\"associatedtype\" href=\"smallvec/trait.Array.html#associatedtype.Item\" title=\"type smallvec::Array::Item\">Item</a>]&gt; for <a class=\"struct\" href=\"smallvec/struct.SmallVec.html\" title=\"struct smallvec::SmallVec\">SmallVec</a>&lt;A&gt;"]],
"spirt":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"spirt/struct.AttrSetDef.html\" title=\"struct spirt::AttrSetDef\">AttrSetDef</a>&gt; for <a class=\"struct\" href=\"spirt/struct.AttrSetDef.html\" title=\"struct spirt::AttrSetDef\">AttrSetDef</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"spirt/struct.TypeDef.html\" title=\"struct spirt::TypeDef\">TypeDef</a>&gt; for <a class=\"struct\" href=\"spirt/struct.TypeDef.html\" title=\"struct spirt::TypeDef\">TypeDef</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"spirt/struct.ConstDef.html\" title=\"struct spirt::ConstDef\">ConstDef</a>&gt; for <a class=\"struct\" href=\"spirt/struct.ConstDef.html\" title=\"struct spirt::ConstDef\">ConstDef</a>"]],
"spirv_tools":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u32.html\">u32</a>]&gt; for <a class=\"enum\" href=\"spirv_tools/binary/enum.Binary.html\" title=\"enum spirv_tools::binary::Binary\">Binary</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;[<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u8.html\">u8</a>]&gt; for <a class=\"enum\" href=\"spirv_tools/binary/enum.Binary.html\" title=\"enum spirv_tools::binary::Binary\">Binary</a>"]],
"tempfile":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/path/struct.Path.html\" title=\"struct std::path::Path\">Path</a>&gt; for <a class=\"struct\" href=\"tempfile/struct.TempDir.html\" title=\"struct tempfile::TempDir\">TempDir</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/path/struct.Path.html\" title=\"struct std::path::Path\">Path</a>&gt; for <a class=\"struct\" href=\"tempfile/struct.TempPath.html\" title=\"struct tempfile::TempPath\">TempPath</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/ffi/os_str/struct.OsStr.html\" title=\"struct std::ffi::os_str::OsStr\">OsStr</a>&gt; for <a class=\"struct\" href=\"tempfile/struct.TempPath.html\" title=\"struct tempfile::TempPath\">TempPath</a>"],["impl&lt;F&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.AsRef.html\" title=\"trait core::convert::AsRef\">AsRef</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/path/struct.Path.html\" title=\"struct std::path::Path\">Path</a>&gt; for <a class=\"struct\" href=\"tempfile/struct.NamedTempFile.html\" title=\"struct tempfile::NamedTempFile\">NamedTempFile</a>&lt;F&gt;"]]
};if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()