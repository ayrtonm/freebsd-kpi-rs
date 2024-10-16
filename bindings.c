#include "opt_platform.h"

#include <sys/cdefs.h>

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/bus.h>
#include <sys/kernel.h>
#include <sys/lock.h>
#include <sys/malloc.h>
#include <sys/module.h>
#include <sys/mutex.h>
#include <sys/proc.h>
#include <sys/rman.h>
#include <sys/smp.h>
#include <sys/taskqueue.h>

#include <machine/_inttypes.h>
#include <machine/bus.h>
#include <machine/intr.h>
#include <machine/machdep.h>
#include <machine/resource.h>
#ifdef SMP
#include <machine/smp.h>
#endif

#include <dev/fdt/fdt_intr.h>

#include <dev/ofw/openfirm.h>
#include <dev/ofw/ofw_bus.h>
#include <dev/ofw/ofw_bus_subr.h>

#include <dt-bindings/interrupt-controller/apple-aic.h>
#include <arm64/apple/apple_mbox.h>
#include <arm64/apple/rtkit.h>

#include "device_if.h"
#include "ofw_bus_if.h"
#include "pic_if.h"

#if !defined(BINDINGS_AS_HEADER)
phandle_t rust_bindings_ofw_bus_get_node(device_t dev);

phandle_t
rust_bindings_ofw_bus_get_node(device_t dev)
{
    return ofw_bus_get_node(dev);
}
#endif

struct rtkit_task;

int rtkit_set_iop_pwrstate(struct rtkit_state *state, uint16_t pwrstate);
int rtkit_rx_callback(void *cookie, struct apple_mbox_msg msg);
void rtkit_rx_task(struct rtkit_task *context, int pending);

struct rtkit_buffer {
	bus_addr_t			addr;
	bus_size_t			size;
	void				*kva;

	bus_dma_tag_t		tag;
	bus_dmamap_t		map;

	// TODO: this is a pretty ugly workaround for making handle_buffer_req
	// endpoint oblivious. I should probably find a better way to dedup code
	struct rtkit_state	*state;
};

struct rtkit_task {
	//struct task				task;
	struct apple_mbox_msg	msg;
	struct rtkit_state		*state;
};

struct rtkit_state {
	device_t			dev;
	struct apple_mbox	mbox;

	uint16_t			iop_pwrstate;
	uint16_t			ap_pwrstate;

	uint64_t			epmap;
	void (*callbacks[32])(void *, uint64_t);
	void				*args[32];

	struct rtkit_buffer crashlog_buffer;
	struct rtkit_buffer syslog_buffer;
	struct rtkit_buffer ioreport_buffer;
	struct rtkit_buffer oslog_buffer;

	rtkit_map			map_fn;
	void				*map_arg;

	uint8_t				syslog_entries;
	uint8_t				syslog_msg_size;
	char				*syslog_msg;

	bool				verbose;
	bool				noalloc;
};
int rtkit_handle_mgmt(struct rtkit_state *state, struct apple_mbox_msg *msg);
