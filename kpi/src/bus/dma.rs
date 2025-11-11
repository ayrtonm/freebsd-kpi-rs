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

use crate::bindings::{bus_addr_t, bus_dma_lock_t, bus_dma_tag_t, bus_dmamap_t, bus_size_t};
use crate::prelude::*;
use core::ffi::{c_int, c_void};
use core::ops::{BitOr, Range};
use core::ptr::null_mut;

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
    pub fn maxsize(mut self, maxsize: bus_size_t) -> Self {
        self.maxsize = Some(maxsize);
        self
    }
    pub fn nsegments(mut self, nsegments: usize) -> Self {
        self.nsegments = Some(nsegments);
        self
    }
    pub fn maxsegsz(mut self, maxsegsz: bus_size_t) -> Self {
        self.maxsegsz = Some(maxsegsz);
        self
    }
    pub fn flags(mut self, flags: Option<BusDmaFlags>) -> Self {
        self.flags = flags;
        self
    }
    pub fn lockfunc(mut self, func: bus_dma_lock_t, arg: *mut c_void) -> Self {
        self.lockfunc = Some(func);
        self.lockfunc_arg = arg;
        self
    }
    /// Allocates a DMA tag and initializes it according to the arguments.
    pub fn build(self) -> Result<BusDmaTag> {
        let mut tag = bus_dma_tag_t::default();
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
                self.maxsize.unwrap_or(unsafe { bindings::maxphys }),
                self.nsegments.unwrap_or(1).try_into().unwrap(),
                self.maxsegsz.unwrap_or(unsafe { bindings::maxphys }),
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

#[derive(Debug)]
pub struct BusDmaTag(bus_dma_tag_t);

#[derive(Copy, Clone, Debug)]
pub struct BusDmaFlags(c_int);

impl BitOr<BusDmaFlags> for BusDmaFlags {
    type Output = BusDmaFlags;

    fn bitor(self, rhs: BusDmaFlags) -> BusDmaFlags {
        Self(self.0 | rhs.0)
    }
}

#[derive(Debug)]
pub struct BusDmaMap(bus_dmamap_t);

pub mod wrappers {
    use super::*;
    use crate::ErrCode;
    use bindings::{bus_dmamap_callback_t, bus_dmamap_t, device_t};

    gen_newtype! {
        BusDmaFlags,
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
        let mut map = bus_dmamap_t::default();
        let res = unsafe { bindings::bus_dmamap_create(tag.0, flags, &raw mut map) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(BusDmaMap(map))
    }

    /// Creates a mapping in device visible address space of buflen bytes of buf, associated with the DMA map map.
    pub fn bus_dmamap_load(
        dmat: BusDmaTag,
        map: BusDmaMap,
        buf: &mut [u8],
        callback: bus_dmamap_callback_t,
        arg: *mut c_void,
        flags: Option<BusDmaFlags>,
    ) -> Result<()> {
        let flags = match flags {
            Some(flags) => flags.0,
            None => 0,
        };
        let res = unsafe {
            bindings::bus_dmamap_load(
                dmat.0,
                map.0,
                buf.as_mut_ptr().cast::<c_void>(),
                buf.len().try_into().unwrap(),
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
}
