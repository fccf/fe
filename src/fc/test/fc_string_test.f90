
#include "fc_debug.h"
program fc_string_test
  use fc_string
  use fc_debug
  implicit none

  character(:), allocatable :: s_lower, s_upper, s_file

  s_lower = 'hello world!'
  s_upper = 'HELLO WORLD!'
  s_file  = '/root/fc/fc.f90'

  assert_true(lower(s_upper)==s_lower)
  assert_true(upper(s_lower)==s_upper)
  assert_true(all(is_logical(['true ', 'false', 'no   ', 'yes  ', 'T    ', 'N    '])))
  assert_true(all(is_int(['1234', '10  ', '0058'])))
  assert_true(all(is_real(['1234', '3.14', '5.0 ',' 12 '])))
  assert_true(to_logical('true'))
  assert_true(to_int('123') == 123)
  assert_equal(to_real('3.14'), 3.14)
  assert_true(file_dir(s_file)=='/root/fc')
  assert_true(file_name(s_file)=='fc.f90')
  assert_true(file_base(s_file)=='fc')
  assert_true(file_ext(s_file)=='f90')
  assert_true(start_with('start_with', 'art'))
  assert_true(end_with('end_with', 'wi'))

  write(*, '(a)') assert_info()


  print*, 'to_str(s) = '//to_str(3.1415)
  print*, 'to_str(v) = '//to_str([1,2,3,4,5], vsep=',', shell=.TRUE.)
  print*, 'to_str(m) = ...'//new_line('a')//to_str(reshape([1.,2.,3.,4.],[2,2]), vsep=' ', &
                             & msep= new_line('a'), shell=.FALSE.)


 block
   character(:), allocatable :: file_string, filename, line
   integer :: unit

   filename = 'test_read_file.txt'
   file_string = file_content(filename)
   print*, file_string

   open(newunit=unit, file=filename)

   line = next_line_content(unit)
   print*, 'line>>1:'//line
   line = next_line_content(unit)
   print*, 'line>>2:'//line
   close(unit)

 end block

end program fc_string_test
