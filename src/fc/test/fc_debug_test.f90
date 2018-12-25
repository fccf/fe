#include "fc_debug.h"
program fc_debug_test
  use fc_debug
  implicit none

  write(*, '(a)') compile_info()

  write(*, '(a, i0)') 'debug level: ', debug_level_
  debug_info(-1, '(a)') 'this is an level -1 information.'
  debug_info(1,'(a)') 'this is an level 1 information.'

  !> true
  assert_equal(1.0,1.0)
  !> false
  assert_true(1==2)

  print*, assert_info()

  assert(1==1)
  error('print an error.')

end program fc_debug_test
