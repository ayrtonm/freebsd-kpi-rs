# Running Demos

The easiest way to run demos is in QEMU. Get either qemu-system-x86_64 or qemu-system-aarch64.
Next grab a FreeBSD 15.1 image from https://download.freebsd.org/releases/VM-IMAGES/15.1-RELEASE/

x86_64 is easier to boot
https://wiki.freebsd.org/arm64/QEMU

```
qemu-system-x86_64 -m 4G \
    -serial telnet::4444,server \
    -nographic \
    -drive file=path/to/FreeBSD-15.1-RELEASE-amd64-ufs.raw,format=raw \
    -enable-kvm \
    -smp cpus=4
```

In a separate terminal run `telnet localhost 4444`

# Hello world module

# Echodev module

# Virtio sound driver

QEMU flag for virtio sound PCI device `-device virtio-sound-pci,audiodev=myaudiodev -audiodev alsa,id=myaudiodev`

The easiest way to build virtio_snd.ko is on the host.

Go to the output directory

python3 -m http.server 8080

In the FreeBSD guest run
fetch http://10.0.2.2:8080/path/to/virtio_snd.ko

kldload ./virtio_snd.ko
sndctl
pkg install moc
