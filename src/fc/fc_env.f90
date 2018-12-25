module fc_env
  !< fortran common environment parameter
  use iso_fortran_env, &
  only: int32, int64, real32, real64, input_unit, output_unit, error_unit, &
      & compiler_version, compiler_options
  implicit none

  integer, parameter :: i4p = int32
  integer, parameter :: i8p = int64
  integer, parameter :: r4p = real32
  integer, parameter :: r8p = real64
  integer, parameter :: ip_ = i4p          !< default integer kind
  integer, parameter :: rp_ = r8p          !< default real kind
  integer, parameter :: iu_ = input_unit   !< default input unit
  integer, parameter :: ou_ = output_unit  !< default output unit
  integer, parameter :: eu_ = error_unit   !< default error unit

end module fc_env
