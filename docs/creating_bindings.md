# Creating bindings for kobj interfaces

The KPI crate has traits for `device_if.m` and `pic_if.m` and the `virtio_snd` branch has a sound
crate with a trait for `channel_if.m`, but developers will need to use other kobj interfaces. Not
gonna lie, creating bindings is currently a little rough around the edges. Many developers shouldn't
need to do this, but the main tools for this are `define_dev_interface!`, `define_c_function!` and
the `AsRustType`/`AsCType` traits.

To create bindings for `foo_if.m` first define a trait for it (the convention is `trait FooIf {}`).
If this is a device interface trait, add `DeviceIf` as a bound (i.e. `trait FooIf: DeviceIf {}`).

Then add a trait method for each kobj method you want to allow defining in rust. As a first pass,
make the arguments the same as in the C versions. If this is a device interface the first argument
must be `ArcRef<<Self as DeviceIf>::Softc>` followed by the original arguments.

## Basic type-safety and ergonomics

Let's now start changing argument types in the trait methods to make the interface harder to use
incorrectly. `AsRustType`/`AsCType` define how the rust method argument/return types must be related
to the types in the kobj method, respectively. For example if I know some void pointer arguments
must all have the same type for a given implementation I can add an associated type to the trait to
constrain them. This gives each implementation the flexibility to choose a type for the void
pointers while making sure the types match where expected.

```
trait FooIf {
    // Each implementation of FooIf has to specify a type for Bar
    type Bar;

    // Allowed even if kobj interface defines these as `void method_1(void *ptr)` and
    // `void method_2(uint32_t x, void *ptr)`.

    // If the kobj interface defines these as
    //
    // void method_1(void *ptr);
    // void method_2(uint32_t x, void *ptr);
    //
    // and expects each implementation to use the same type for the two void pointers I can define
    // the trait methods like this
    fn method_1(ptr: *mut Self::Bar);
    fn method_2(x: u32, ptr: *mut Self::Bar);
}
```

This type-safety is nice, but dereferencing pointers require unsafe blocks. However, if we know how
the pointers will be accessed on both sides of the interface we might be able to change pointers to
references. Rust has shared references `&T` and unique references `&mut T`. If I know a pointer
passed to a method will not be dereferenced by C code for the duration of that method I can change
the pointer to a shared reference. Furthermore if I know the pointer is only passed to one method at
a time and the method is not called concurrently multiple times on the same kobj I can change it to
a `&mut T`.

## Subclasses

Sometimes we know part of the pointee is accessed by C code and another part is not (e.g.
subclasses). Subclasses are not the preferred way of code reuse in rust, but there's too much
working C code that relies on them so let's make it work.

The KPI crate provides the `SubClass<B, F>` type and `base!` macro for this. `SubClass<B, F>` has
the same layout as a C struct starting with a base type `B` followed by a type `F` with the extra
subclass fields. Its API is designed with the assumption that the base class is always shared with C
code while the extra fields `F` are only used from rust. A `SubClass<B, F>` may be used
transparently as if it were an instance of `F` (e.g. by accessing the fields and methods of `F`
directly). To access the base class fields the `base!` macro can be used with C-like syntax (e.g.
`base!(&subclass->some_base_field)` to reference a field and `base!(subclass->another_base_field)`
to read/write to a field). Since C code may be concurrently reading/writing to the base class the
latter operations require `unsafe` blocks that should be justified by stating why the usage can't
create a data race.

A base class shared with C must have a specified memory layout (i.e. a `#[repr(C)]` annotation on
the base struct definition). The base class will typically be a C type defined in `kpi::bindings`
which all have specified memory layouts. If you find yourself needing more than one level of
subclassing please reconsider your approach (in particular when the rust code would be what accesses
both subclass levels).

## Pointer lifecycles

Another scenario is one where a kobj method returns a void pointer as some kind of "register"
operation, a second method is called repeatedly with that same pointer as an argument and at some
point a third method is called once with the pointer as an "unregister" operation. If we know the C
code pipes around the pointer like this and does nothing else with it we can define the following
trait for it.

```
trait BarIf {
    type SomeType;

    // This method has to create an Arc<Self::SomeType> somehow and conceptually gives ownership of it
    // to the C code for bar_if.m
    fn bar_register() -> Arc<Self::SomeType>;

    // The C code only calls this after bar_register so it owns an Arc<Self::SomeType> which it can
    // let bar_do_thing borrow
    fn bar_do_thing(ptr: ArcRef<Self::SomeType>);

    // The C code only calls this after all calls to bar_do_thing have returned so it gives
    // ownership of its Arc<Self::SomeType> to bar_unregister. If this method doesn't hand it off to
    // something else the Arc's refcount gets released when the function returns.
    fn bar_unregister(ptr: Arc<Self::SomeType>);
}
```

This is very similar to the softc lifecycle pattern (`device_attach` is "register", most other
device interface methods borrow that `Arc<Softc>` and `device_detach` is "unregister").

If implementations shouldn't require refcounting this example could've also been handled by
replacing `Arc` with `Box` and `ArcRef<Self::SomeType>` with `&Self::SomeType`. Furthermore if I
know the C code does not call `bar_do_thing` multiple times concurrently then
`ArcRef<Self::SomeType>` could be replaced with `&mut Self::SomeType` since it's the only thing
borrowing the pointer. Either way passing `Arc`/`Box` to `bar_unregister` ensures that the pointee
gets its reference released/is freed automatically.

That last example would be the most ergonomic to work with, but it is up to the developer creating
the interface trait to ensure the conversions are allowed given the ownership semantics implicit in
the C code. Defining kobj interface traits that do conversions that break the
[requirements for references](https://doc.rust-lang.org/nomicon/references.html) enables rustc and
LLVM to make invalid optimizations. Defining a good interface that is hard to mis-use requires an
understanding of what code logically "owns" pointers that cross the interface and how the pointee
may be accessed on both sides of it. A safe first step though is to just keep all pointer types the
same and evolve the interface as necessary.
