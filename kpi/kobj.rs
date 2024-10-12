use crate::bindings;
use crate::bindings::{kobj_class_t, kobj_method_t, kobj_ops_t, kobj_t, kobjop_desc_t};
use core::ptr::null_mut;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct KObj(pub(crate) kobj_t);

impl KObj {
    pub fn ops(&self) -> KObjOps {
        KObjOps(unsafe { (*self.0).ops })
    }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct KObjOps(kobj_ops_t);

impl KObjOps {
    pub fn cache(&self, idx: usize) -> KObjMethod {
        KObjMethod(unsafe { (*self.0).cache[idx] })
    }
    pub fn cls(&self) -> KObjClass {
        let ptr = self.0;
        KObjClass(unsafe { (*ptr).cls })
    }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct KObjMethod(*const kobj_method_t);

impl KObjMethod {
    pub fn desc(&self) -> KObjOpDesc {
        let ptr = self.0;
        KObjOpDesc(unsafe { (*ptr).desc })
    }
    pub fn func(&self) -> Option<unsafe extern "C" fn()> {
        let ptr = self.0;
        unsafe { (*ptr).func }
    }
}

#[repr(C)]
#[derive(PartialEq, Eq)]
pub struct KObjOpDesc(pub(crate) kobjop_desc_t);

impl KObjOpDesc {
    pub fn id(&self) -> usize {
        let ptr = self.0;
        unsafe { (*ptr).id as usize }
    }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct KObjClass(kobj_class_t);

impl KObjClass {
    pub fn lookup_method(&self, desc: KObjOpDesc) -> KObjMethod {
        // TODO: this doesn't cache the result yet
        let res = unsafe { bindings::kobj_lookup_method(self.0, null_mut(), desc.0) };
        KObjMethod(res)
    }
}

#[macro_export]
macro_rules! kobj_lookup {
    ($dev:expr, $method:ident) => {{
        use core::mem::transmute;
        use core::ptr::addr_of_mut;
        use $crate::kobj::KObjOpDesc;

        let kobj = $dev.base_class();
        let ops = kobj.ops();
        let desc = {
            use $crate::bindings::*;
            KObjOpDesc(addr_of_mut!(concat_idents!($method, _desc)))
        };
        let mut method = ops.cache(desc.id());
        if method.desc() != desc {
            method = ops.cls().lookup_method(desc)
        }
        let untyped_f = method.func();
        unsafe {
            use $crate::bindings::*;
            transmute::<_, concat_idents!($method, _t)>(untyped_f).unwrap()
        }
    }};
}
