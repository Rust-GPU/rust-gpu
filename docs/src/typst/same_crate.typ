#import "@preview/cetz:0.4.2": canvas, draw
#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge
#import fletcher.shapes: diamond
#import draw: *

#set page(width: auto, height: auto, margin: .5cm)

#let color_shader = rgb("#f9c5c5");
// #let color_shader = white;
// #let color_cpu = rgb("#b1e2d8");
#let color_cpu = white;
#let color_build_script = orange.darken(20%);
#diagram(
	node-stroke: 1pt,
	mark-scale: 150%,
	node((0,0), [myapp], name: "app", corner-radius: 2pt, fill: gradient.linear(color_cpu, color_cpu, color_shader, color_shader)),
	edge("-|>", label("appsh"), [_optional_]),
	edge("-|>", stroke: (dash: "dashed"), [_optional_], label-pos: 0.65),
	node((-0.5,1), [std], name: "std", corner-radius: 2pt, fill: color_cpu),

	node((1.5,0), [myapp-shader], name: "appsh", corner-radius: 2pt, fill: color_shader, stroke: color_build_script),
	node((0.1,1), [core], name: "core", corner-radius: 2pt, fill: color_shader, stroke: color_build_script),
	node((0.77,1), [spirv-std], name: "spirv", corner-radius: 2pt, fill: color_shader, stroke: color_build_script),
	edge(label("app"), "-|>", label("core")),
	edge(label("app"), "-|>", label("spirv")),

	edge(label("app.north"), label("appsh.north"), "<|-", bend: 60deg, label: text(color_build_script)[build shaders], stroke: color_build_script),
)
