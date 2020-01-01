#include <stdint.h>
#include <stdio.h>

extern int32_t nox_rt_write(int32_t i) {
  printf("%d\n", i);
  return 0;
}

extern int32_t nox_rt_read() {
  int32_t val = -1;
  scanf("%d", &val);
  return val;
}
