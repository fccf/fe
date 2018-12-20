program fc_env_test
  use fc_env
  implicit none

  print*, "==> fortran common environment parameter:"
  print*, 'i4p = ', i4p
  print*, 'i8p = ', i8p
  print*, 'r4p = ', r4p
  print*, 'r8p = ', r8p
  print*, 'ip_ = ', ip_
  print*, 'rp_ = ', rp_
  print*, 'iu_ = ', iu_
  print*, 'ou_ = ', ou_
  print*, 'eu_ = ', eu_


end program fc_env_test
