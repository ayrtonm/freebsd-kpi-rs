#include <sys/param.h>
#include <sys/conf.h>
#include <sys/kernel.h>
#include <sys/module.h>

int hello_world_modevent(module_t mod, int type, void *data);

DEV_MODULE(hello_world, hello_world_modevent, NULL);
