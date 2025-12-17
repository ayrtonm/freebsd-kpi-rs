# Creating bindings for kobj interfaces

The KPI crate currently has traits for `device_if.m` and `pic_if.m` and the `virtio_snd` branch has
a sound crate with traits for `channel_if.m` and `mixer_if.m`, but developers will need to use other
kobj interfaces. Not gonna lie, creating bindings is currently a little rough around the edges. Many
developers shouldn't need to do this, but the main tools for this are `define_dev_interface!`,
`define_interface!` and the `AsRustType`/`AsCType` traits.

To create bindings for `foo_if.m` first define a trait for it (the convention is `trait FooIf {}`).
If this is a device interface trait, it must have `DeviceIf` as a bound
(i.e. `trait FooIf: DeviceIf {}`).

Then add a trait method for each kobj method you want to allow defining in rust. As a first pass,
make the arguments the same as in the C versions. If this is a device interface the first argument
must be `ArcRef<<Self as DeviceIf>::Softc>` followed by the original arguments.

## Basic type-safety and ergonomics

Let's now start changing the trait method argument types so the interface is harder to mis-use.
`AsRustType`/`AsCType` define how the rust method argument/return types must be related to the types
in the kobj method, respectively. For example if we know some void pointers in an interface must all
have the same type for a given implementation we can add an associated type to the trait to
constrain them. This gives each implementation the flexibility to choose a type for the void
pointers without requiring any runtime-type info.

```
trait FooIf {
    // Each implementation of FooIf has to specify a type for Bar
    type Bar;

    // If the kobj interface defines these as
    //
    // void method_1(void *ptr);
    // void method_2(uint32_t x, void *ptr);
    //
    // and expects each implementation to use the same type for the two void pointers we can define
    // the trait methods like this
    fn method_1(ptr: *mut Self::Bar);
    fn method_2(x: u32, ptr: *mut Self::Bar);
}
```

This type-safety is a nice first step, but if we know how the pointers will be accessed on both
sides of the interface we might be able to change pointers to references. The benefit is that the
compiler will ensure that implementations don't use references in ways that they shouldn't. Rust has
shared references `&T` and unique references `&mut T`. If we know a pointer passed to a method will
not be dereferenced by C code for the duration of that method we can change the pointer to a shared
reference. Alternatively if we know the pointer is only passed to one rust method at a time and the
method is not called concurrently multiple times on the same kobj we can change it to a `&mut T`.

## Subclasses

Sometimes we know part of the pointee is accessed by C code and another part is not, like in
subclasses. Subclasses are not the suggested way of reusing code in rust, but there's too much
working C code that relies on them so let's just make it work.

The KPI crate provides the `SubClass<B, F>` type and `base!` macro for this. `SubClass<B, F>` has
the same layout as a C struct starting with a base type `B` followed by a type `F` with the extra
subclass fields.

```
struct SubClass {
    struct B base;
    struct F extra_fields
};
```

Its API assumes that the base class is always shared with C code while the extra fields `F` are only
used from rust. A `SubClass<B, F>` may be used transparently as if it were an instance of `F` (e.g.
by accessing the fields and methods of `F` directly). To access the base class fields the `base!`
macro can be used with C-like syntax (e.g.  `base!(&subclass->some_base_field)` to reference a field
and `base!(subclass->another_base_field)` to read/write to a field). Since C code may be
concurrently reading/writing to the base class the latter operations require `unsafe` blocks that
can be justified by stating why the usage won't create a data race.

A base class shared with C must have a specified memory layout (e.g. a `#[repr(C)]` annotation on
the base struct definition). The base class will typically be a type defined in `kpi::bindings`
which all have specified memory layouts. If you find yourself needing more than one level of
subclassing please reconsider your approach (in particular when the rust code would be what accesses
both subclass levels).

## Pointer lifecycles

Another common scenario is one where a kobj interface with 3 methods:

- one method returns a void pointer as some kind of "register" operation
- a second method which is called repeatedly with the registered void pointer
- a third method is called once at the end with the same pointer as an "unregister" operation

If we know the C code pipes around a pointer like this and does nothing else with it we can define
the following trait for it.

```
trait BarIf {
    type MyType;

    // This method has to create an Arc<Self::MyType> and gives ownership of it to the bar_if C code
    fn bar_register() -> Arc<Self::MyType>;

    // The C code only calls this after bar_register so it owns an Arc<Self::MyType> at that point
    // which it can let bar_do_thing borrow
    fn bar_do_thing(ptr: ArcRef<Self::MyType>);

    // The C code only calls this after all calls to bar_do_thing have returned so it gives
    // ownership of its Arc<Self::MyType> to bar_unregister. If this method doesn't hand it off to
    // something else the Arc's refcount gets released when the function returns.
    fn bar_unregister(ptr: Arc<Self::MyType>);
}
```

This is very similar to the softc lifecycle pattern (`device_attach` is "register", most other
device interface methods borrow that `Arc<Softc>` and `device_detach` is "unregister").

If implementations shouldn't use refcounting this example could've also been handled by replacing
`Arc` with `Box` and `ArcRef<Self::MyType>` with `&Self::MyType`. Like we saw above, if we know the
C code doesn't call bar_do_thing multiple times concurrently we could alternatively replace
`ArcRef<Self::MyType>` with `&mut Self::MyType`.

Mutable references are the most ergonomic to work with, but it is up to the developer creating the
interface trait to ensure the conversions are allowed given the ownership semantics implicit in the
C code. If you define a kobj interface trait that does conversions that break the basic
[requirements for references](https://doc.rust-lang.org/nomicon/references.html) that enables rustc
and LLVM to make invalid optimizations. Defining a good interface that is hard to mis-use requires
an understanding of what code logically "owns" pointers that cross the interface and how the pointee
may be accessed on both sides of it. A reasonable first step though is to initially keep all pointer
types the same and evolve the rust interface as the access patterns on the C side become more clear.
