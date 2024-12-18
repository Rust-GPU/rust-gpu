searchState.loadedDescShard("bytemuck", 0, "This crate gives small utilities for casting between plain …\nFor this type of cast the alignments must be exactly the …\nMarker trait for “plain old data” types that are valid …\nDerive the <code>AnyBitPattern</code> trait for a struct\nDerive the <code>PartialEq</code> and <code>Eq</code> trait for a type\nDerive the <code>Hash</code> trait for a type\nDerive the <code>CheckedBitPattern</code> trait for a struct or enum.\nA trait indicating that:\nDerive the <code>Contiguous</code> trait for an enum\nThe primitive integer type with an identical …\nThe upper <em>inclusive</em> bound for valid instances of this type.\nThe lower <em>inclusive</em> bound for valid instances of this type.\nMarker trait for “plain old data” types with no uninit …\nDerive the <code>NoUninit</code> trait for a struct or enum\nIf the element size of a slice changes, then the output …\nMarker trait for “plain old data”.\nDerive the <code>Pod</code> trait for a struct\nThe things that can go wrong when casting between <code>Pod</code> data …\nTrait for types which are Pod when wrapped in Option.\nWhen casting an individual <code>T</code>, <code>&amp;T</code>, or <code>&amp;mut T</code> value the …\nYou tried to cast a reference into a reference to a type …\nA trait which indicates that a type is a …\nDerive the <code>TransparentWrapper</code> trait for a struct\nTrait for types that can be safely created with <code>zeroed</code>.\nDerive the <code>Zeroable</code> trait for a type.\nTrait for types which are Zeroable when wrapped in Option.\nRe-interprets <code>&amp;T</code> as <code>&amp;[u8]</code>.\nRe-interprets <code>&amp;mut T</code> as <code>&amp;mut [u8]</code>.\nCast <code>A</code> into <code>B</code>\nCast <code>&amp;mut A</code> into <code>&amp;mut B</code>.\nCast <code>&amp;A</code> into <code>&amp;B</code>.\nCast <code>&amp;[A]</code> into <code>&amp;[B]</code>.\nCast <code>&amp;mut [A]</code> into <code>&amp;mut [B]</code>.\nChecked versions of the casting functions exposed in crate …\nFill all bytes of <code>slice</code> with zeroes (see <code>Zeroable</code>).\nReturns the argument unchanged.\nRe-interprets <code>&amp;[u8]</code> as <code>&amp;T</code>.\nRe-interprets <code>&amp;mut [u8]</code> as <code>&amp;mut T</code>.\nIf <code>value</code> is within the range for valid instances of this …\nIf <code>value</code> is within the range for valid instances of this …\nCalls <code>U::from(self)</code>.\nPerform the conversion from <code>C</code> into the underlying integral …\nPerform the conversion from <code>C</code> into the underlying integral …\nFind the offset in bytes of the given <code>$field</code> of <code>$Type</code>. …\nConvert the wrapper type into the inner type.\nConvert the wrapper type into the inner type.\nConvert a mutable reference to the wrapper type into a …\nConvert a mutable reference to the wrapper type into a …\nConvert a reference to the wrapper type into a reference …\nConvert a reference to the wrapper type into a reference …\nConvert a slice to the wrapped type into a slice to the …\nConvert a slice to the wrapped type into a slice to the …\nConvert a mutable slice to the wrapped type into a mutable …\nConvert a mutable slice to the wrapped type into a mutable …\nAs <code>align_to</code>, but safe because of the <code>Pod</code> bound.\nAs <code>align_to_mut</code>, but safe because of the <code>Pod</code> bound.\nReads the slice into a <code>T</code> value.\nTry to cast <code>A</code> into <code>B</code>.\nTry to convert a <code>&amp;mut A</code> into <code>&amp;mut B</code>.\nTry to convert a <code>&amp;A</code> into <code>&amp;B</code>.\nTry to convert <code>&amp;[A]</code> into <code>&amp;[B]</code> (possibly with a change in …\nTry to convert <code>&amp;mut [A]</code> into <code>&amp;mut [B]</code> (possibly with a …\nRe-interprets <code>&amp;[u8]</code> as <code>&amp;T</code>.\nRe-interprets <code>&amp;mut [u8]</code> as <code>&amp;mut T</code>.\nReads from the bytes as if they were a <code>T</code>.\nConvert the inner type into the wrapper type.\nConvert the inner type into the wrapper type.\nConvert a mutable reference to the inner type into a …\nConvert a mutable reference to the inner type into a …\nConvert a reference to the inner type into a reference to …\nConvert a reference to the inner type into a reference to …\nConvert a slice to the inner type into a slice to the …\nConvert a slice to the inner type into a slice to the …\nConvert a mutable slice to the inner type into a mutable …\nConvert a mutable slice to the inner type into a mutable …\nFill all bytes of <code>target</code> with zeroes (see <code>Zeroable</code>).\nCalls <code>zeroed</code>.\nCalls <code>zeroed</code>.\n<code>Self</code> <em>must</em> have the same layout as the specified <code>Bits</code> …\nA marker trait that allows types that have some invalid …\nThe things that can go wrong when casting between …\nWhen casting to a <code>CheckedBitPattern</code> type, it is possible …\nAn error occurred during a true-<code>Pod</code> cast\nCast <code>A</code> into <code>B</code>\nCast <code>&amp;mut A</code> into <code>&amp;mut B</code>.\nCast <code>&amp;A</code> into <code>&amp;B</code>.\nCast <code>&amp;[A]</code> into <code>&amp;[B]</code>.\nCast <code>&amp;mut [A]</code> into <code>&amp;mut [B]</code>.\nReturns the argument unchanged.\nRe-interprets <code>&amp;[u8]</code> as <code>&amp;T</code>.\nRe-interprets <code>&amp;mut [u8]</code> as <code>&amp;mut T</code>.\nCalls <code>U::from(self)</code>.\nIf this function returns true, then it must be valid to …\nReads the slice into a <code>T</code> value.\nTry to cast <code>A</code> into <code>B</code>.\nTry to convert a <code>&amp;mut A</code> into <code>&amp;mut B</code>.\nTry to convert a <code>&amp;A</code> into <code>&amp;B</code>.\nTry to convert <code>&amp;[A]</code> into <code>&amp;[B]</code> (possibly with a change in …\nTry to convert <code>&amp;mut [A]</code> into <code>&amp;mut [B]</code> (possibly with a …\nRe-interprets <code>&amp;[u8]</code> as <code>&amp;T</code>.\nRe-interprets <code>&amp;mut [u8]</code> as <code>&amp;mut T</code>.\nReads from the bytes as if they were a <code>T</code>.")