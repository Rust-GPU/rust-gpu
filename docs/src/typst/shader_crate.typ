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
	node((0,0), [myapp], name: "app", corner-radius: 2pt, fill: color_cpu),
	edge("r", "-|>"),
	edge("-|>"),
	node((0,1), [std], name: "std", corner-radius: 2pt, fill: color_cpu),

	node((1.2,0), [myapp-shader], name: "appsh", corner-radius: 2pt, fill: color_shader, stroke: color_build_script),
	node((0.8,1), [core], name: "core", corner-radius: 2pt, fill: color_shader, stroke: color_build_script),
	node((1.5,1), [spirv-std], name: "spirv", corner-radius: 2pt, fill: color_shader, stroke: color_build_script),
	edge(label("appsh"), "-|>", label("core")),
	edge(label("appsh"), "-|>", label("spirv")),

	edge(label("app.north"), label("appsh.north"), "-|>", bend: 60deg, label: text(color_build_script)[build shaders], stroke: color_build_script),
)
