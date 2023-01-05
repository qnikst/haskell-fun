#define _GNU_SOURCE
#include <sched.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
//#include "Entry_stub.h"
#include "Rts.h"
#endif

static uint32_t procno = 0;

StgClosure Entry_entry_closure;
uint32_t __real_getNumberOfProcessors(void);

uint32_t __wrap_getNumberOfProcessors(void)
{
   if (procno==0) {
      return __real_getNumberOfProcessors();
   } else {
     return procno;
   }
}

int setcpus() {
   cpu_set_t set;
   CPU_ZERO(&set);
   FILE *cpuinfo = fopen("/proc/cpuinfo", "rb");
   char *arg = 0;
   size_t size = 0;
   int ret = 0;
   int current_cpu = -1;
   int current_core = -1;
   int cpu_count = 0;

   ret = sched_getaffinity(0, sizeof(cpu_set_t), &set);
   if (ret == -1) {
     fprintf(stderr, "Error: failed to get cpu affinity");
     return 0;
   }

   while(getdelim(&arg, &size, '\n', cpuinfo) != -1)
   {
      if (strstr(arg, "core id") != NULL) {
        current_core++;
        char * found = strchr(arg, ':');
        if (found) {
           int cpu = atoi(found+1);
           if (current_cpu != cpu) {
              current_cpu++;
              if (CPU_ISSET(current_core, &set)) {
                 CPU_SET(current_core, &set);  // XXX: this is noop.
                 fprintf(stderr, "%i real core - enabling\n", current_core);
                 cpu_count++;
              } else {
                 fprintf(stderr, "%i was disabled - skipping\n", current_core);
              }
           } else {
              fprintf(stderr, "%i is virual - skipping\n", current_core);
              CPU_CLR(current_core, &set);
           }
        } else {
           return 1;
        }
      }
   }
   ret = sched_setaffinity(0, sizeof(cpu_set_t), &set);
   if (ret == -1) {
     fprintf(stderr, "Error: failed to set affinities - falling back to default procedure\n");
     procno = 0;
   } else {
     fprintf(stderr, "Define number of affinities as %i\n", cpu_count);
     procno = cpu_count;
   }
   free(arg);
   fclose(cpuinfo);
   return 0;
}

int main(int argc, char * argv[]) {
  setcpus();
  int exit_status;
  SchedulerStatus status;
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
  {
     Capability *cap = rts_lock();
     rts_evalLazyIO(&cap, &Entry_entry_closure, NULL);
     status = rts_getSchedStatus(cap);
     rts_unlock(cap);
  }
  // check the status of the entire Haskell computation
 switch (status) {
    case Killed:
        errorBelch("main thread exited (uncaught exception)");
        exit_status = EXIT_KILLED;
        break;
    case Interrupted:
        errorBelch("interrupted");
        exit_status = EXIT_INTERRUPTED;
        break;
    case HeapExhausted:
        exit_status = EXIT_HEAPOVERFLOW;
        break;
    case Success:
        exit_status = EXIT_SUCCESS;
        break;
    default:
        barf("main thread completed with invalid status");
    }
  hs_exit();
  return 0;
}
