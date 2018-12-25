#include "fc_debug.h"
program fc_string_test
  use fc_string
  use fc_debug
  implicit none

  assert_true(lower('HELLO WORLD!')=='hello world!')
  assert_true(upper('hello world!')=='HELLO WORLD!')
  assert_true(all(is_logical(['true ', 'false', 'no   ', 'yes  ', 'T    ', 'N    '])))
  assert_true(all(is_int(['1234', '10  ', '0058'])))
  assert_true(all(is_real(['1234', '3.14', '5.0 ',' 12 '])))
  assert_true(to_logical('true'))
  assert_true(to_int('123') == 123)
  assert_equal(to_real('3.14'), 3.14)
  assert_true(start_with('start_with', 'start'))
  assert_true(end_with('end_with', 'with'))
  assert_true(to_str(1)=='1')
  assert_true(to_str([1,2,3])=='[1,2,3]')
  assert_true(to_str(reshape([1,2,3,4],[2,2]))=='[[1,2];[3,4]]')

  write(*, '(a)') assert_info()

end program fc_string_test
