#include <unistd.h>

int bogus() {
  return usleep(100000000);
}
