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

use crate::ErrCode;
use crate::bindings::{
    bus_addr_t, bus_dma_lock_t, bus_dma_segment_t, bus_dma_tag_t, bus_dmamap_t, bus_size_t,
};
use crate::ffi::{Ext, SyncPtr};
use crate::prelude::*;
use core::ffi::{c_int, c_void};
use core::mem::transmute;
use core::ops::{BitOr, Range};
use core::ptr::null_mut;

pub type BusDmaMapFn<T> = extern "C" fn(Ext<T>, &bus_dma_segment_t, i32, i32);
type _RawBusDmaMapFn = extern "C" fn(*mut c_void, *mut bus_dma_segment_t, i32, i32);

#[must_use]
#[derive(Debug)]
pub struct BusDmaTagBuilder {
    parent: BusDmaTag,
    alignment: Option<bus_size_t>,
    bounds: Option<bus_addr_t>,
    addr: Option<Range<bus_addr_t>>,
    maxsize: Option<bus_size_t>,
    nsegments: Option<usize>,
    maxsegsz: Option<bus_size_t>,
    flags: Option<BusDmaFlags>,
    lockfunc: Option<bus_dma_lock_t>,
    lockfunc_arg: *mut c_void,
}

impl BusDmaTagBuilder {
    pub fn alignment(mut self, alignment: bus_size_t) -> Self {
        self.alignment = Some(alignment);
        self
    }
    pub fn bounds(mut self, bounds: bus_addr_t) -> Self {
        self.bounds = Some(bounds);
        self
    }
    pub fn addr(mut self, addr: Range<bus_addr_t>) -> Self {
        self.addr = Some(addr);
        self
    }
    pub fn max_size(mut self, maxsize: bus_size_t) -> Self {
        self.maxsize = Some(maxsize);
        self
    }
    pub fn num_segments(mut self, nsegments: usize) -> Self {
        self.nsegments = Some(nsegments);
        self
    }
    pub fn max_seg_size(mut self, maxsegsz: bus_size_t) -> Self {
        self.maxsegsz = Some(maxsegsz);
        self
    }
    pub fn flags(mut self, flags: Option<BusDmaFlags>) -> Self {
        self.flags = flags;
        self
    }
    pub fn lock_func(mut self, func: bus_dma_lock_t, arg: *mut c_void) -> Self {
        self.lockfunc = Some(func);
        self.lockfunc_arg = arg;
        self
    }
    /// Allocates a DMA tag and initializes it according to the arguments.
    pub fn build(self) -> Result<BusDmaTag> {
        let mut tag: bus_dma_tag_t = null_mut();
        let addr = self.addr.unwrap_or(BUS_SPACE_MAXADDR..BUS_SPACE_MAXADDR);
        let flags = match self.flags {
            Some(flags) => flags.0,
            None => 0,
        };
        let res = unsafe {
            bindings::bus_dma_tag_create(
                self.parent.0,
                self.alignment.unwrap_or(1),
                self.bounds.unwrap_or(0),
                addr.start,
                addr.end,
                None,       /* filtfunc */
                null_mut(), /* filtfunc_arg */
                self.maxsize.unwrap_or(bindings::maxphys),
                self.nsegments.unwrap_or(1).try_into().unwrap(),
                self.maxsegsz.unwrap_or(bindings::maxphys),
                flags,
                self.lockfunc.unwrap_or(None),
                self.lockfunc_arg,
                &raw mut tag,
            )
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(BusDmaTag(tag))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct BusDmaTag(bus_dma_tag_t);

unsafe impl Sync for BusDmaTag {}
unsafe impl Send for BusDmaTag {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct BusDmaFlags(c_int);

impl BitOr<BusDmaFlags> for BusDmaFlags {
    type Output = BusDmaFlags;

    fn bitor(self, rhs: BusDmaFlags) -> BusDmaFlags {
        Self(self.0 | rhs.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct BusDmaMap(bus_dmamap_t);

unsafe impl Sync for BusDmaMap {}
unsafe impl Send for BusDmaMap {}

pub mod wrappers {
    use super::*;
    use crate::ErrCode;
    use bindings::{bus_dmamap_callback_t, bus_dmamap_t, device_t};

    gen_newtype! {
        BusDmaFlags as i32,
        BUS_DMA_ALLOCNOW,
        BUS_DMA_WAITOK,
        BUS_DMA_NOWAIT,
        BUS_DMA_COHERENT,
        BUS_DMA_ZERO,
        BUS_DMA_NOCACHE,
    }

    pub fn bus_get_dma_tag(dev: device_t) -> BusDmaTag {
        BusDmaTag(unsafe { bindings::bus_get_dma_tag(dev) })
    }

    pub fn bus_dma_tag_create(parent: BusDmaTag) -> BusDmaTagBuilder {
        BusDmaTagBuilder {
            parent,
            alignment: None,
            bounds: None,
            addr: None,
            maxsize: None,
            nsegments: None,
            maxsegsz: None,
            flags: None,
            lockfunc: None,
            lockfunc_arg: null_mut(),
        }
    }

    pub fn bus_dmamap_create(tag: BusDmaTag, flags: Option<BusDmaFlags>) -> Result<BusDmaMap> {
        let flags = match flags {
            Some(flags) => flags.0,
            None => 0,
        };
        let mut map: bus_dmamap_t = null_mut();
        let res = unsafe { bindings::bus_dmamap_create(tag.0, flags, &raw mut map) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(BusDmaMap(map))
    }

    /// Creates a mapping in device visible address space of buflen bytes of buf, associated with the DMA map map.
    pub fn bus_dmamap_load<T>(
        dmat: BusDmaTag,
        map: BusDmaMap,
        //buf: &mut [u8],
        ptr: SyncPtr<c_void>,
        len: bus_size_t,
        callback: Option<BusDmaMapFn<T>>,
        arg: Ext<T>,
        flags: Option<BusDmaFlags>,
    ) -> Result<()> {
        let flags = match flags {
            Some(flags) => flags.0,
            None => 0,
        };
        let callback =
            unsafe { transmute::<Option<BusDmaMapFn<T>>, bus_dmamap_callback_t>(callback) };
        let arg = Ext::into_raw(arg).cast::<c_void>();
        let res = unsafe {
            bindings::bus_dmamap_load(
                dmat.0,
                map.0,
                //buf.as_mut_ptr().cast::<c_void>(),
                //buf.len().try_into().unwrap(),
                ptr.as_ptr(),
                len,
                callback,
                arg,
                flags,
            )
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    /// Allocates memory that is mapped into KVA at the address returned in vaddr
    pub fn bus_dmamem_alloc(
        dmat: BusDmaTag,
        //vaddr: &mut DmaMem,
        vaddr: *mut SyncPtr<c_void>,
        flags: BusDmaFlags,
    ) -> Result<BusDmaMap> {
        let mut map: bus_dmamap_t = null_mut();
        let res =
            unsafe { bindings::bus_dmamem_alloc(dmat.0, &raw mut (*vaddr).0, flags.0, &mut map) };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(BusDmaMap(map))
        }
    }
}
