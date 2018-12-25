#include "fc_debug.h"
program test_fc_debug
  use fc_debug
  implicit none

  write(*, '(a)') compile_info()

  assert(1==1)

  assert_equal(1.0,1.0)

  info(1,*) 'this is an information'
  assert_equal(1.0,2.0)



end program test_fc_debug
