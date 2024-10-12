use crate::bindings;
use crate::bindings::{malloc_type, M_DEVBUF, M_NOWAIT, M_WAITOK, M_ZERO};
use core::alloc::{AllocError, Allocator, GlobalAlloc, Layout};
use core::ffi::c_int;
use core::ptr::{addr_of_mut, NonNull};

#[derive(Debug, Copy, Clone)]
pub struct KernelAllocator<const F: c_int> {
    ty: *mut malloc_type,
}

impl<const F: c_int> KernelAllocator<F> {
    pub const fn new(ty: *mut malloc_type) -> Self {
        Self { ty }
    }
}

impl<const F: c_int> KernelAllocator<F> {
    unsafe fn malloc(&self, layout: Layout, flags: c_int) -> *mut u8 {
        bindings::malloc(layout.size(), self.ty, F).cast()
    }
}

#[global_allocator]
pub static WAITOK: KernelAllocator<{ M_WAITOK }> =
    KernelAllocator::new(addr_of_mut!(M_DEVBUF) as *mut malloc_type);

pub static NOWAIT: KernelAllocator<{ M_NOWAIT }> =
    KernelAllocator::new(addr_of_mut!(M_DEVBUF) as *mut malloc_type);

unsafe impl<const F: c_int> Sync for KernelAllocator<F> {}

unsafe impl GlobalAlloc for KernelAllocator<{ M_WAITOK }> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.malloc(layout, M_WAITOK)
    }
    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        bindings::free(ptr.cast(), self.ty);
    }
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        self.malloc(layout, M_WAITOK | M_ZERO)
    }
    unsafe fn realloc(&self, ptr: *mut u8, _layout: Layout, new_size: usize) -> *mut u8 {
        bindings::realloc(ptr.cast(), new_size, self.ty, M_WAITOK).cast()
    }
}

unsafe impl Allocator for KernelAllocator<{ M_NOWAIT }> {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = unsafe { self.malloc(layout, M_NOWAIT) };
        match NonNull::new(ptr) {
            Some(non_null_ptr) => Ok(NonNull::slice_from_raw_parts(non_null_ptr, layout.size())),
            None => Err(AllocError),
        }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        bindings::free(ptr.as_ptr().cast(), self.ty)
    }
}
