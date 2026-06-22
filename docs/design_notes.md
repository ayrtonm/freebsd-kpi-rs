# Rust KPI design notes

## Callbacks

Making callback KPIs safe is a challenge because we need to ensure that the callback context pointer
won't be freed as long as the callback may still be called. This is a very general statement so
let's narrow down to the only case I care about for now which is when the softc is the context.
Basically the challenge boils down to make sure the callback can't be invoked after device_detach.

There are three options here:

1. make the callback register function unsafe

2. refcount the softc

3. use an out-of-band method to "drain" the callback invocation queue

2 and 3 accomplish the same goal but do different things with the softc lifetime. 2 extends it as
long as necessary while 3 ensures it doesn't need to be extended past its natural end
(device_detach). Warner recommended against refcounting the softc because newbus may reuse the
device with other drivers so if the callback were to get invoked you would essentially have a
detached driver trying to co-manage a device. There are also more things that get "disconnected" (to
revoke access to hardware) after device_detach so the softc might contain pointers to freed memory
if one isn't careful/aware of what still works after detach. This means 3 is the more viable option.
The downside is that it doesn't provide a uniform solution for different callback KPIs like 2 so we
need to look at them individually.

config_intrhook_establish + config_intrhook_disestablish

callout_reset + callout_schedule + callout_drain

intr_pic_claim_root (no unregister)

bus_setup_intr + bus_teardown_intr

taskqueue_enqueue + taskqueue_drain/taskqueue_cancel


Use-after-frees seem like a relatively big problem so it would be nice to try to avoid unsafe. If
refcounting is not an option, then the next best thing would be to use out-of-band methods to at
least panic if there will be a future callback that takes the thing being freed. For callout this
means

callout_reset<T: CalloutArg>(c: &mut Callout, ticks: sbintime_t, func: CalloutFn<T>, arg: Ref<T>) -> Result<()>;

|----------------------|----------------------------|---------------X
device_attach          callout_reset/schedule       device_detach

device attach leads to...
drop softc
drop Ref<T> that was passed in to callout_reset

must ensure callout_pending(c) is false

need to map Ref<T> where T: CalloutArg to the callout

Don't think I can do this

impl<T: CalloutArg> Drop for T {
    fn drop(&mut self) {
        assert!(!callout_pending(self.get_callout()));
    }
}

trait CalloutArg {
    fn get_callout(&self) -> *mut callout;
}

impl Drop for Callout {
    fn drop(&mut self) {
        assert!(!callout_pending(self.0))
    }
}

Instead put the callout_pending(c) assertion in drop glue for Callout
this means that the lifetime of the callout must have the same lifetime as its arg
if Callout in softc and softc is arg all is fine
if Callout in softc and thing with shorter lifetime than softc is arg all is fine, but need to check that thing with shorter lifetime is actually related to interfaces used by the driver

## Self-references
