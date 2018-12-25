program fc_test_assert
  use fc_assert
  implicit none

  call assert(1.0==1.000)

  call assert(1.0,1.000)

  call assert(1==2)


end program fc_test_assert
