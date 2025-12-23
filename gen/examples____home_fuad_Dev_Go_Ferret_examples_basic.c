// Generated C code from Ferret
// Module: examples/../home/fuad/Dev/Go/Ferret/examples/basic

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

#include "io.h"
#include "interface.h"
#include "map.h"
#include "bigint.h"
#include "optional.h"
#include "examples____home_fuad_Dev_Go_Ferret_examples_basic.h"

int main() {
  int32_t x = 10;
  int32_t y = 20;
  int32_t sum = x + y;
  int32_t diff = x - y;
  int32_t prod = x * y;
  if (sum > 25) {
    ferret_io_Println("Sum is greater than 25");
  } else {
    ferret_io_Println("Sum is 25 or less");
  }
  print_result(sum);
  return 0;
}

void print_result(int32_t value) {
  ferret_io_Println("Result calculated");
}

