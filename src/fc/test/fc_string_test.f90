program fc_string_test
  use fc_string
  implicit none

  character(:), allocatable :: s_lower, s_upper, s_file
  logical, allocatable :: passed(:)
  logical :: pass


  passed = [logical ::]

  s_lower = 'hello world!'
  s_upper = 'HELLO WORLD!'
  s_file  = '/root/fc/fc.f90'


  pass = lower(s_upper)==s_lower; passed = [passed, pass]
  print*, '>> lower:', pass

  pass = lower(s_upper)==s_lower; passed = [passed, pass]
  print*, '>> upper:', pass

  pass = all(is_logical(['true ', 'false', 'no   ', 'yes  ', 'T    ', 'N    ']))
  passed = [passed, pass]
  print*, ">> is_logical:", pass

  pass = all(is_int(['1234', '10  ', '0058']))
  passed = [passed, pass]
  print*, ">> is_int:", pass

  pass = all(is_real(['1234', '3.14', '5.0 ',' 12 ']))
  passed = [passed, pass]
  print*, ">> is_real:", pass

  pass = to_logical('true')
  passed = [passed, pass]
  print*, ">> to_logical:", pass

  pass = to_int('123') == 123
  passed = [passed, pass]
  print*, ">> to_int:", pass

  pass = to_real('3.14') == 3.14
  passed = [passed, pass]
  print*, ">> to_real:", pass

  pass = file_dir(s_file)=='/root/fc'; passed = [passed, pass]
  print*, ">> file_dir:", pass

  pass = file_name(s_file)=='fc.f90'; passed = [passed, pass]
  print*, ">> file_name:", pass

  pass = file_base(s_file)=='fc'; passed = [passed, pass]
  print*, ">> file_base:", pass

  pass = file_ext(s_file)=='f90'; passed = [passed, pass]
  print*, ">> file_ext:", pass

  pass = start_with('start_with', 'start'); passed = [passed, pass]
  print*, ">> start_with", pass

  pass = end_with('end_with', 'with'); passed = [passed, pass]
  print*, ">> end_with", pass

  print*, "all passed?", all(passed)

  print*, 'to_str(s) = '//to_str(3.1415)
  print*, 'to_str(v) = '//to_str([1,2,3,4,5], vsep=',', shell=.TRUE.)
  print*, 'to_str(m) = ...'//new_line('a')//to_str(reshape([1.,2.,3.,4.],[2,2]), vsep=' ', &
                             & msep= new_line('a'), shell=.FALSE.)


 block
   character(:), allocatable :: file_string, filename, line
   integer :: unit, ios, n

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
