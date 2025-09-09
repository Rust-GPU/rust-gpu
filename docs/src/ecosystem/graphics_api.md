# Graphics APIs

While there are many graphics APIs in the wild, and you may be unsure on which one to pick, I assume you they are all surprisingly similar to each other. Generally speaking, you can sort all graphics APIs in one of three categories, which all share a common set of properties. Learning one API will allow you to pick up the others of the same category almost immediately, since you'll already be familiar with all the concepts. Switching between categories may take a bit longer. While the general concepts are often the exact same, the way they are exposed in the API can differ quite dramatically. But still, learning any of these makes it a lot easier to pick up the next.

### High Level

The high level graphics APIs were the first to be invested and focus on being simple to use. But as GPUs evolved, their API got further and further away from how modern GPUs actually work. Any of these is still very much usable nowadays, and we still see many newly released applications make use of them, so they won't be deprecated anytime soon. 

| API       | Platform                        | Note                      |
|-----------|---------------------------------|---------------------------|
| OpenGL    | Windows, Linux, MacOS*          |                           |
| OpenGL ES | Windows, Linux, MacOS*, Android | for embedded systems (ES) |
| DX11      | Windows                         |                           |
| WebGL     | browser                         |                           |

*drivers severely outdated

* **State machine**: The graphics API is a giant global state machine. You have a bunch of functions that let you adjust each knob of this state machine, and another set of functions that allows you to flush the state and send it off to the GPU for processing. Easy to use, but also easy to misuse and accidentally forget to reset some state. 
* **Immediately executed**: From an API perspective, each flushing command is immediately executed, and you're free to immediately ask for its result. However, GPUs have very deep pipelines which you have to feed with lots of data, and it can take a while until whatever you're enqueued has finished processing. But as the API hides the asynchronous nature of the GPU, you can't know when commands have finished processing. When data is either processed on the GPU or uploaded from the CPU, and you're using it in some operation, you implicitly wait for the previous operation to finish. Which leads to your CPU or GPU idling, waiting for the other component to finish processing.
* **Automated Synchronization**: With the GPU having so deep pipelines, it needs to know which command must wait for which other command to finish. Due to the immediately executed nature of the APIs, you don't need to worry about this, the graphics driver will do it for you. But this is not just expensive to evaluate (on the CPU), but may need to be overly cautious on which operations may race (on the GPU), which generally speaking, *may* reduce performance. 
* **Driver inconsistencies**: These APIs are often not super well specified, making it difficult to reason exactly what a driver might do. And since the driver needs to do a lot of the heavy lifting, you may see vastly different outcomes between different platforms. This is especially an issue with OpenGL, where apart from the main desktop GPU vendors, you can face some wild inconsistencies between drivers.

### Low Level

Low level APIs expose everything to you. You will have to deal with every single detail of the GPU to even render your very first triangle. Which is why you often see people being celebrated for making their first colored triangle. With all the detail front-loaded, by the time you've made your first triangle, you have seen 90% of the API already. Which allows you to immediately start on more complicated problems.

| API    | Platform                        |
|--------|---------------------------------|
| Vulkan | Windows, Linux, Android, MacOS* |
| DX12   | Windows                         |
| Metal  | MacOS, iOS                      |

*via MoltenVK, see [Translation Layers#MoltenVK](translation.md#moltenvk)

* **pre-defined state**: When you define a graphics or compute pipeline, you define all the state ahead-of-time when you create it. And when you bind a pipeline, it will set all the state to what you've defined it to be. You don't need to worry about resetting the global state and the graphics driver can run its heavy compilation and optimization passes on creation, not when you first enter said state. Optionally, you can also define which state you would like to be dynamic.
* **Asynchronous operations**: The asynchronous nature of GPUs is directly exposed to you. Instead of enqueuing individual commands, you create chains of commands (CommandEncoder or CommandBuffer) and submit them to the GPU. And you can then ask the graphics API whether some submission finished or enqueue a new submission that must wait on the previous one.
* **Manual Synchronization**: Even within chains of commands, the GPU must know which caches to flush and invalidate between two commands. You need to manually tell the GPU the resources that need flushing or invalidation, which gives you a lot more precise control. 
* **Manual Memory management**: You can do manual memory management, though most often you'll just delegate this task to a library.
* **A good spec**: A good specification of the graphics API is surprisingly useful, as it lets you reason about how things are supposed to work. And when you do encounter a supposed driver bug, it allows you to compare what you're doing with the spec to figure out what's wrong.
* **Validation layers**: In low level graphics APIs, validation is optional. You can choose to turn it on and face the overhead they have, or turn them off in production and run at full speed. And with a good specification declaring the exact failure modes, these validation layers can point you exactly at the item in the spec you're violating. 

### WebGPU in the middle

A quick clarification of definitions:
* [WebGPU](https://www.w3.org/TR/webgpu/) is the API specification used to communicate with the browser
* [wgpu](https://github.com/gfx-rs/wgpu) is an implementation of said API in rust, used in Firefox
* [dawn](https://github.com/google/dawn) is another implementation in C++, used in Chrome

WebGPU is a new graphics API set to replace the high-level WebGL with a new low-level API. But as we need to sandbox the code executed in the web browser, it can't just expose a pure low level API. This places WebGPU in the middle of these two, providing all the bells and whistles of a modern graphics API, but not exposing everything to you since it needs to work in a browser sandbox. 

From the low level API, it inherits much of the *pre-defined state*, *a good spec* and always-on *validation layers*. But it can't expose manual synchronization or memory management, as that could be used to escape the sandbox. Instead, it draws inspiration from the *automated synchronization* and the *immediately executed* nature of the high level APIs. This mix positions WebGPU well to both provide a modern graphics API while at the same time being easier to learn than low level APIs.

TODO wgpu-native, dawn on native
TODO API multiplexing
