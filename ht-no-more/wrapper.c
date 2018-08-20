#define _GNU_SOURCE
#include <sched.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "Entry_stub.h"
#include "Rts.h"
#endif

int setcpus() {
   cpu_set_t set;
   CPU_ZERO(&set);
   FILE *cpuinfo = fopen("/proc/cpuinfo", "rb");
   char *arg = 0;
   size_t size = 0;
   int current_cpu = -1;
   int current_core = -1;
   while(getdelim(&arg, &size, '\n', cpuinfo) != -1)
   {
      if (strstr(arg, "core id") != NULL) {
	current_core++;
	char * found = strchr(arg, ':');
	if (found) {
	   int cpu = atoi(found+1);
	   if (current_cpu != cpu) {
              current_cpu++;
	      CPU_SET(current_core, &set);
           }
        } else {
	   return 1;
	}
      }
   }
   sched_setaffinity(0, sizeof(cpu_set_t), &set);
   free(arg);
   fclose(cpuinfo);
   return 0;
}

int main(int argc, char * argv[]) {
  setcpus();
  #if __GLASGOW_HASKELL__ >= 703
  {
     RtsConfig conf = defaultRtsConfig;
     conf.rts_opts_enabled = RtsOptsAll;
     hs_init_ghc(&argc, &argv, conf);
  }
  #else
     hs_init(&argc, &argv);
  #endif
  hs_init(&argc, &argv);
  entry();
  hs_exit();
  return 0;
}
