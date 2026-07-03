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

#if !defined(__has_include)
#error "Please preprocess this with a compiler with support for __has_include"
#endif

#include "opt_platform.h"

#include <sys/cdefs.h>

#include <sys/param.h>
#include <sys/conf.h>
#include <sys/bus.h>
#include <sys/cpuset.h>
#include <sys/fcntl.h>
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
#include <sys/sglist.h>
#include <sys/smp.h>
#include <sys/sx.h>
#include <sys/systm.h>
#include <sys/taskqueue.h>
#include <sys/uio.h>

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
#include <dev/fdt/simplebus.h>

#include <dev/ofw/openfirm.h>
#include <dev/ofw/ofw_bus.h>
#include <dev/ofw/ofw_bus_subr.h>

#include "ofw_bus_if.h"
#endif // FDT

#if defined(WITH_SOUND)
#include <dev/sound/pcm/sound.h>
#include "channel_if.h"
#include "mixer_if.h"
#endif

// FIXME: The second part is a workaround for what is arguably a bug in the APPLE KERNCONF
// i.e. the build has -I flags that pick up virtio headers, but omits the virtio sources leading
// to an undefined symbol ld error. This wouldn't be a problem except that bindgen generates a
// inlines.o which references the symbols declared by the virtio headers
#if __has_include(<dev/virtio/virtio.h>) && !defined(KERNCONF_APPLE)
#include <dev/virtio/virtio.h>
#include <dev/virtio/virtqueue.h>
#endif

#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/socketvar.h>

#if __has_include(<dev/virtio/socket/virtio_socket.h>)
#include <dev/virtio/socket/virtio_socket.h>
#endif

#if __has_include(<dev/virtio/sound/virtio_snd.h>)
#include <dev/virtio/sound/virtio_snd.h>
#endif

#if __has_include(<dev/virtio/fs/virtio_fs.h>)
#include <dev/virtio/fs/virtio_fs.h>
#endif

#if defined(__aarch64__)
#include <dev/gpio/gpiobusvar.h>
device_t apple_smc_get_bus(device_t dev);
int apple_smc_pin_set(device_t dev, uint32_t pin, uint32_t value);
#endif

#if defined(KERNCONF_APPLE)
#include <dev/nvme/nvme_private.h>
#include <dev/nvme/nvme_ans.h>

#include <dev/evdev/input.h>
#include <dev/hid/hid.h>
#include <dev/hid/hidquirk.h>
#include "hid_if.h"

#include <arm64/apple/apple_bindings.h>
#include <dt-bindings/interrupt-controller/apple-aic.h>
#include "nvme_if.h"
#endif

#include "device_if.h"

#if defined(__aarch64__)
#include "pic_if.h"
#endif

#if defined(__aarch64__)
extern struct bus_space memmap_bus;
#endif

/*
 * This header is preprocessed by bindgen to create bindings.rs and inlines.c then preprocessed
 * again when compiling inlines.c. I'd like to ignore -Wmissing-prototypes since the code generated
 * by bindgen does not fit the FreeBSD style guidelines and attempting to do so would require
 * invasive changes to bindgen for little benefit.
 */
#pragma clang diagnostic ignored "-Wmissing-prototypes"

void
rust_bindings_CPU_SET(u_int cpu, cpuset_t *set) {
    CPU_SET(cpu, set);
}

bool
rust_bindings_CPU_ISSET(u_int cpu, cpuset_t *set) {
    return CPU_ISSET(cpu, set);
}

#if defined(__aarch64__)
uint64_t
rust_bindings_CPU_AFFINITY(u_int cpu) {
    return CPU_AFFINITY(cpu);
}

uint64_t
rust_bindings_CPU_AFF0(uint64_t mpidr) {
    return CPU_AFF0(mpidr);
}

uint64_t
rust_bindings_CPU_AFF1(uint64_t mpidr) {
    return CPU_AFF1(mpidr);
}
#endif

#define FN1(macro, ret, ty1) \
    ret fn_##macro(ty1 a) { \
        return macro(a); \
    }

#define FN2(macro, ret, ty1, ty2) \
    ret fn_##macro(ty1 a, ty2 b) { \
        return macro(a, b); \
    }

#define FN3(macro, ret, ty1, ty2, ty3) \
    ret fn_##macro(ty1 a, ty2 b, ty3 c) { \
        return macro(a, b, c); \
    }

#define FN4(macro, ret, ty1, ty2, ty3, ty4) \
    ret fn_##macro(ty1 a, ty2 b, ty3 c, ty4 d) { \
        return macro(a, b, c, d); \
    }

#define FN5(macro, ret, ty1, ty2, ty3, ty4, ty5) \
    ret fn_##macro(ty1 a, ty2 b, ty3 c, ty4 d, ty5 e) { \
        return macro(a, b, c, d, e); \
    }

FN5(bus_space_barrier, void, bus_space_tag_t, bus_space_handle_t, bus_size_t, bus_size_t, int)
FN4(bus_barrier, void, struct resource*, bus_size_t, bus_size_t, int)
FN5(bus_space_map, int, bus_space_tag_t, bus_addr_t, bus_size_t, int, bus_space_handle_t*)

FN4(mtx_init, void, struct mtx*, const char*, const char*, int)
FN1(mtx_initialized, size_t, const struct mtx*)
FN1(mtx_lock, void, struct mtx*)
FN1(mtx_lock_spin, void, struct mtx*)
FN1(mtx_unlock, void, struct mtx*)
FN1(mtx_unlock_spin, void, struct mtx*)

FN4(tsleep, int, const void*, int, const char*, int)

#define BUS_N(n, ty) \
    ty fn_bus_read_##n(struct resource *res, bus_size_t offset) { \
        return bus_read_##n(res, offset); \
    } \
    void fn_bus_write_region_##n(struct resource *res, bus_size_t o, ty *p, bus_size_t count) { \
        return bus_write_region_##n(res, o, p, count); \
    } \
    void fn_bus_write_##n(struct resource *res, bus_size_t offset, ty value) { \
        return bus_write_##n(res, offset, value); \
    }

BUS_N(1, uint8_t);
BUS_N(2, uint16_t);
BUS_N(4, uint32_t);
#if defined(__aarch64__)
BUS_N(8, uint64_t);
#endif
