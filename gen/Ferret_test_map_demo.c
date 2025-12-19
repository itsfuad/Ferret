// Generated C code from Ferret
// Module: Ferret/test_map_demo

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

#include "io.h"
#include "interface.h"
#include "optional.h"
#include "map.h"
#include "Ferret_test_map_demo.h"

int main() {
  ferret_map_t* capitals;
  const char* __map_keys_0[3];
  const char* __map_values_0[3];
  __map_keys_0[0] = "France";
  __map_values_0[0] = "Paris";
  __map_keys_0[1] = "Japan";
  __map_values_0[1] = "Tokyo";
  __map_keys_0[2] = "Bangladesh";
  __map_values_0[2] = "Dhaka";
  capitals = ferret_map_from_pairs(sizeof(const char*), sizeof(const char*), __map_keys_0, __map_values_0, 3, ferret_map_hash_str, ferret_map_equals_str);
    char __print_buf_1[1024]; snprintf(__print_buf_1, sizeof(__print_buf_1), "%s%s", "Capital of France is: ", ((ferret_optional_const_char_ptr __coalesce_temp_2 = (((const char**)(ferret_map_get(capitals, &((const char*){"France"}))) ? ((ferret_optional_const_char_ptr){.value = *((const char**)(ferret_map_get(capitals, &((const char*){"France"})))), .is_some = 1}) : ((ferret_optional_const_char_ptr){.value = 0, .is_some = 0}))), __coalesce_temp_2.is_some ? __coalesce_temp_2.value : "Unknown")); ferret_io_Println(__print_buf_1);
;
  ferret_optional_const_char_ptr japanCapital = (((const char**)(ferret_map_get(capitals, &((const char*){"Japan"}))) ? ((ferret_optional_const_char_ptr){.value = *((const char**)(ferret_map_get(capitals, &((const char*){"Japan"})))), .is_some = 1}) : ((ferret_optional_const_char_ptr){.value = 0, .is_some = 0}));
  if (japanCapital.is_some != 0) {
        char __print_buf_3[1024]; snprintf(__print_buf_3, sizeof(__print_buf_3), "%s%s", "Capital of Japan: ", japanCapital.value); ferret_io_Println(__print_buf_3);
;
  }
    const char* __map_key_3 = "Germany";
  const char* __map_value_3 = "Berlin";
  ferret_map_set(capitals, &__map_key_3, &__map_value_3);
    char __print_buf_4[1024]; snprintf(__print_buf_4, sizeof(__print_buf_4), "%s%s", "Capital of Germany: ", ((ferret_optional_const_char_ptr __coalesce_temp_5 = (((const char**)(ferret_map_get(capitals, &((const char*){"Germany"}))) ? ((ferret_optional_const_char_ptr){.value = *((const char**)(ferret_map_get(capitals, &((const char*){"Germany"})))), .is_some = 1}) : ((ferret_optional_const_char_ptr){.value = 0, .is_some = 0}))), __coalesce_temp_5.is_some ? __coalesce_temp_5.value : "Not found")); ferret_io_Println(__print_buf_4);
;
  return 0;
}

