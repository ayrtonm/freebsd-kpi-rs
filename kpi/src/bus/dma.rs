/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2025 Ayrton Muñoz
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

//use crate::bindings::bus_dma_segment_t;
use crate::prelude::*;
use core::ffi::{c_int, c_void};
use core::ops::BitOr;

//#[derive(Debug)]
//pub struct DmaMem(*mut c_void, bus_size_t);
//pub type DmaMapFn<T> = fn(CRef<T>, &[bus_dma_segment_t], c_int) -> Result<()>;

#[derive(Copy, Clone, Debug)]
pub struct BusDmaFlags(c_int);

impl BitOr<BusDmaFlags> for BusDmaFlags {
    type Output = BusDmaFlags;

    fn bitor(self, rhs: BusDmaFlags) -> BusDmaFlags {
        Self(self.0 | rhs.0)
    }
}

pub mod wrappers {
    use super::*;
    use crate::ErrCode;
    use bindings::{
        bus_addr_t, bus_dma_lock_t, bus_dma_tag_t, bus_dmamap_callback_t, bus_dmamap_t, bus_size_t,
        device_t,
    };
    use core::ops::Range;
    use core::ptr::null_mut;

    gen_newtype! {
        BusDmaFlags,
        BUS_DMA_ALLOCNOW,
        BUS_DMA_WAITOK,
        BUS_DMA_NOWAIT,
        BUS_DMA_COHERENT,
        BUS_DMA_ZERO,
        BUS_DMA_NOCACHE,
    }

    pub fn bus_get_dma_tag(dev: device_t) -> bus_dma_tag_t {
        unsafe { bindings::bus_get_dma_tag(dev) }
    }

    /// Allocates a DMA tag and initializes it according to the arguments.
    pub fn bus_dma_tag_create(
        parent: bus_dma_tag_t,
        alignment: bus_size_t,
        bounds: bus_addr_t,
        addr: Range<bus_addr_t>,
        maxsize: bus_size_t,
        nsegments: usize,
        maxsegsz: bus_size_t,
        flags: BusDmaFlags,
        lockfunc: bus_dma_lock_t,
        lockfunc_arg: *mut c_void,
    ) -> Result<bus_dma_tag_t> {
        let mut tag = bus_dma_tag_t::default();
        let res = unsafe {
            bindings::bus_dma_tag_create(
                parent,
                alignment,
                bounds,
                addr.start,
                addr.end,
                None,       /* filtfunc */
                null_mut(), /* filtfunc_arg */
                maxsize,
                nsegments as c_int,
                maxsegsz,
                flags.0,
                lockfunc,
                lockfunc_arg,
                &mut tag,
            )
        };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(tag)
        }
    }

    /// Allocates memory that is mapped into KVA at the address returned in vaddr
    pub fn bus_dmamem_alloc(
        dmat: bus_dma_tag_t,
        //vaddr: &mut DmaMem,
        vaddr: *mut *mut c_void,
        flags: BusDmaFlags,
    ) -> Result<bus_dmamap_t> {
        let mut map = bus_dmamap_t::default();
        let res = unsafe { bindings::bus_dmamem_alloc(dmat, vaddr, flags.0, &mut map) };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(map)
        }
    }

    /// Creates a mapping in device visible address space of buflen bytes of buf, associated with the DMA map map.
    pub fn bus_dmamap_load(
        dmat: bus_dma_tag_t,
        map: bus_dmamap_t,
        //buf: DmaMem,
        buf: *mut c_void,
        buflen: bus_size_t,
        callback: bus_dmamap_callback_t,
        callback_arg: *mut c_void,
        //callback: DmaMapFn<T>,
        //callback_arg: CRef<T>,
        flags: Option<BusDmaFlags>,
    ) -> Result<()> {
        //unsafe extern "C" fn invoke_rust_fn<T>(
        //    callback_c_arg: *mut c_void,
        //    segs: *mut bus_dma_segment_t,
        //    nsegs: c_int,
        //    error: c_int,
        //) {
        //    let callback_rust_arg = unsafe { callback_c_arg.cast::<T>().as_ref().unwrap() };
        //}
        let c_flags = match flags {
            Some(flags) => flags.0,
            None => 0,
        };
        // TODO: don't leak the arg
        //let leaked_arg = unsafe { callback_arg.leak_ref() };
        let res = unsafe {
            bindings::bus_dmamap_load(dmat, map, buf, buflen, callback, callback_arg, c_flags)
        };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }
}
