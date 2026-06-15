# Compute shaders

Unlike vertex or fragment shaders, compute shaders need to declare the size of their workgroup:

```rust
#[spirv(compute(threads(8, 8)))]
pub fn main() {}
```

This one declares a workgroup of the size `(8, 8, 1)`. Any dimensions not declared default to 1.

Here's a full example of a compute shader that computes `c = a + b`:

```rust
#[spirv(compute(threads(32)))]
pub fn main(
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] a: &[u32],
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] b: &[u32],
    #[spirv(descriptor_set = 0, binding = 2, storage_buffer)] c: &mut [u32],
    #[spirv(global_invocation_id)] global_index: UVec3,
) {
    let i = global_index.x as usize;
    c[i] = a[i] + b[i];
}
```

> [!CAUTION]
> I want to direct your attention to the `c: &mut [u32]` arg. In Rust, mutable references don't just allow you to mutate the referenced memory, but also gives you *exclusive* access to it. Since this mutable references is *shared* between all the threads, it can't be exclusive.
> 
> This is a mistake on rust-gpu's entry point API design, not sure when I'll get around to fix it.

Here's the most interesting built-ins and intrinsics for compute shaders, put together into a rather silly shader that sum up 128 values. The shared memory is also a mutable reference that is not actually exclusive, like the mutable buffers.

```rust
// assumption: subgroup size == 32
#[spirv(compute(threads(128)))]
pub fn sum(
    #[spirv(global_invocation_id)] global_invocation_id: UVec3,
    #[spirv(local_invocation_id)] local_invocation_id: UVec3,
    #[spirv(num_workgroups)] num_workgroups: UVec3,
    #[spirv(workgroup_id)] workgroup_id: UVec3,
    #[spirv(workgroup)] shared_memory: &mut [f32; 4],
    #[spirv(spec_constant(id = 0, default = 42))] spec_constant: u32,
    #[spirv(descriptor_set = 0, binding = 0, storage_buffer)] values: &[f32],
    #[spirv(descriptor_set = 0, binding = 1, storage_buffer)] output: &mut f32,
) {
    let value = values[global_invocation_id.x as usize];
    let sum_per_subgroup = spirv_std::arch::subgroup_f_add(value);

    let local_idx = local_invocation_id.x as usize;
    if local_idx % 32 == 0 {
        shared_memory[local_idx / 32] = sum_per_subgroup;
    }

    spirv_std::arch::workgroup_memory_barrier_with_group_sync();
    if local_idx == 0 {
        let mut accum = 0.;
        for i in 0..4 {
            shared_memory[i] += accum;
        }
        *output = accum;
    }
}
```

> [!Note]
> You currently can't declare the workgroup size as a specialization constant, open an issue if you need this.
