initSidebarItems({"enum":[["BinaryOptions",""],["DisassembleOptions",""]],"fn":[["assemble","Encodes the given SPIR-V assembly text to its binary representation. The length parameter specifies the number of bytes for text. Encoded binary will be stored into *binary. Any error will be written into *diagnostic if diagnostic is non-null, otherwise the context’s message consumer will be used. The generated binary is independent of the context and may outlive it. The SPIR-V binary version is set to the highest version of SPIR-V supported by the context’s target environment."],["disassemble","Decodes the given SPIR-V binary representation to its assembly text. The word_count parameter specifies the number of words for binary. The options parameter is a bit field of spv_binary_to_text_options_t. Decoded text will be stored into *text. Any error will be written into *diagnostic if diagnostic is non-null, otherwise the context’s message consumer will be used."],["text_destroy","Frees an allocated text stream. This is a no-op if the text parameter is a null pointer."]],"struct":[["Text",""]]});