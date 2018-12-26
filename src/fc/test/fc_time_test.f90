#include "fc_debug.h"
program fc_time_test
  use fc_time
  use fc_debug
  use fc_string
  implicit none

  real :: t1
  print*, now()

  t1 = wtime()
  call sleep(1)
  t1 = wtime()
  assert_equal(t1, 1.0)
  assert_true(s2hms(t1)=='0h:0m:1s')

  block
    type(timer_t) :: timer

    call timer%start('A')
    call timer%start('B')
    call sleep(1)
    call timer%start('D')
    call sleep(1)
    call timer%stop()
    call timer%stop()
    call timer%start('C')
    call sleep(1)
    call timer%stop()
    call timer%start('B')
    call sleep(1)
    call timer%stop()
    call timer%stop()

    call timer%report()
  endblock


end program fc_time_test
