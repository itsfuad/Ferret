// Generated C code from Ferret
// Module: Ferret/test_map

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

#include "io.h"
#include "interface.h"
#include "optional.h"
#include "Ferret_test_map.h"

int main() {
  ferret_map_t* ages;
  const char* __map_keys_0[3];
  int32_t __map_values_0[3];
  __map_keys_0[0] = "alice";
  __map_values_0[0] = 25;
  __map_keys_0[1] = "bob";
  __map_values_0[1] = 30;
  __map_keys_0[2] = "charlie";
  __map_values_0[2] = 35;
  ages = ferret_map_from_pairs(sizeof(const char*), sizeof(int32_t), __map_keys_0, __map_values_0, 3, ferret_map_hash_str, ferret_map_equals_str);
  ferret_optional_int32_t aliceAge = (((int32_t*)(ferret_map_get(ages, &((const char*){"alice"}))) ? ((ferret_optional_int32_t){.value = *((int32_t*)(ferret_map_get(ages, &((const char*){"alice"})))), .is_some = 1}) : ((ferret_optional_int32_t){.value = 0, .is_some = 0}));
  if (aliceAge.is_some != 0) {
        char __print_buf_1[1024]; snprintf(__print_buf_1, sizeof(__print_buf_1), "%s%d", "Alice's age: ", aliceAge.value); ferret_io_Println(__print_buf_1);
;
  } else {
        ferret_io_Println("Alice not found");
;
  }
  ferret_optional_int32_t daveAge = (((int32_t*)(ferret_map_get(ages, &((const char*){"dave"}))) ? ((ferret_optional_int32_t){.value = *((int32_t*)(ferret_map_get(ages, &((const char*){"dave"})))), .is_some = 1}) : ((ferret_optional_int32_t){.value = 0, .is_some = 0}));
  if (daveAge.is_some == 0) {
        ferret_io_Println("Dave not found (as expected)");
;
  } else {
        char __print_buf_2[1024]; snprintf(__print_buf_2, sizeof(__print_buf_2), "%s%d", "Dave's age: ", daveAge.value); ferret_io_Println(__print_buf_2);
;
  }
    const char* __map_key_2 = "dave";
  int32_t __map_value_2 = 40;
  ferret_map_set(ages, &__map_key_2, &__map_value_2);
  ferret_optional_int32_t daveAgeAfter = (((int32_t*)(ferret_map_get(ages, &((const char*){"dave"}))) ? ((ferret_optional_int32_t){.value = *((int32_t*)(ferret_map_get(ages, &((const char*){"dave"})))), .is_some = 1}) : ((ferret_optional_int32_t){.value = 0, .is_some = 0}));
  if (daveAgeAfter.is_some != 0) {
        char __print_buf_3[1024]; snprintf(__print_buf_3, sizeof(__print_buf_3), "%s%d", "Dave's age after assignment: ", daveAgeAfter.value); ferret_io_Println(__print_buf_3);
;
  }
  ferret_map_t* emptyMap = (ferret_map_t*)ferret_map_new(sizeof(const char*), sizeof(int32_t), ferret_map_hash_str, ferret_map_equals_str);
  ferret_optional_int32_t test = (((int32_t*)(ferret_map_get(emptyMap, &((const char*){"nonexistent"}))) ? ((ferret_optional_int32_t){.value = *((int32_t*)(ferret_map_get(emptyMap, &((const char*){"nonexistent"})))), .is_some = 1}) : ((ferret_optional_int32_t){.value = 0, .is_some = 0}));
  if (test.is_some == 0) {
        ferret_io_Println("Empty map test passed");
;
  }
  return 0;
}

