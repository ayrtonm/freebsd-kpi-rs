/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2024 Ayrton Muñoz
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

#include "opt_platform.h"

#include <sys/cdefs.h>

#include <sys/param.h>
#include <sys/bus.h>
#ifdef INTRNG
#include <sys/intr.h>
#endif
#include <sys/kernel.h>
#include <sys/lock.h>
#include <sys/malloc.h>
#include <sys/module.h>
#include <sys/mutex.h>
#include <sys/proc.h>
#include <sys/refcount.h>
#include <sys/rman.h>
#include <sys/smp.h>
#include <sys/systm.h>
#include <sys/taskqueue.h>

#include <machine/_inttypes.h>
#include <machine/bus.h>

#if !defined(__x86_64__)
#include <machine/machdep.h>
#endif

#include <machine/resource.h>
#ifdef SMP
#include <machine/smp.h>
#endif

#ifdef FDT
#include <dev/fdt/fdt_intr.h>

#include <dev/ofw/openfirm.h>
#include <dev/ofw/ofw_bus.h>
#include <dev/ofw/ofw_bus_subr.h>

#include "ofw_bus_if.h"
#endif

#include "device_if.h"

#if !defined(__x86_64__)
#include "pic_if.h"
#endif


/*
 * inline functions defined in headers are not supported well by bindgen yet. We work around this by
 * instantiating them once in this file. These functions cannot be static since they're referenced
 * by other translation units but they only need to be called from rust so we just add declarations
 * here for simplicity.
 */
#define INLINE(...) __VA_ARGS__;__VA_ARGS__
#ifdef FDT
INLINE(
phandle_t
rust_bindings_ofw_bus_get_node(device_t dev))
{
	return ofw_bus_get_node(dev);
}
#endif

INLINE(
void
rust_bindings_refcount_init(volatile u_int *count, u_int value)) {
    return refcount_init(count, value);
}

INLINE(
u_int
rust_bindings_refcount_load(volatile u_int *count)) {
    return refcount_load(count);
}

INLINE(
u_int
rust_bindings_refcount_acquire(volatile u_int *count)) {
    return refcount_acquire(count);
}
INLINE(
bool
rust_bindings_refcount_release(volatile u_int *count)) {
    return refcount_release(count);
}

#define BUS_N(n, ty) \
    ty rust_bindings_bus_read_##n(struct resource *res, bus_size_t offset); \
    ty \
    rust_bindings_bus_read_##n(struct resource *res, bus_size_t offset) { \
        return bus_read_##n(res, offset); \
    } \
    void rust_bindings_bus_write_##n(struct resource *res, bus_size_t offset, ty value); \
    void \
    rust_bindings_bus_write_##n(struct resource *res, bus_size_t offset, ty value) { \
        return bus_write_##n(res, offset, value); \
    }

BUS_N(1, uint8_t);
BUS_N(2, uint16_t);
BUS_N(4, uint32_t);
BUS_N(8, uint64_t);

#if 1
#include <dt-bindings/interrupt-controller/apple-aic.h>
#include <arm64/apple/apple_mbox.h>
#include <arm64/apple/rtkit.h>

#endif
