#include "fc_debug.h"
program fc_file_test
  use fc_debug
  use fc_file
  implicit none
  character(:), allocatable :: s_file

  s_file  = '/root/fc/fc.f90'

  assert_true(file_dir(s_file)=='/root/fc')
  assert_true(file_name(s_file)=='fc.f90')
  assert_true(file_base(s_file)=='fc')
  assert_true(file_ext(s_file)=='f90')

  write(*, '(a)') assert_info()
  
end program fc_file_test
