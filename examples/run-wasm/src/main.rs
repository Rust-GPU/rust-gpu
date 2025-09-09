fn main() {
    cargo_run_wasm::run_wasm_with_css(
        "
        html, body, canvas {
            outline: none;
            overflow: clip;
            margin: 0px;
            width: 100%;
            height: 100%;
        }
    ",
    );
}
