module fc_assert

  implicit none

  public :: assert

  private

  interface assert
    module procedure assert_true
    module procedure assert_equal
  end interface assert

contains
  !=============================================================================
  subroutine assert_true(this, msg)
    logical, intent(in)      :: this
    character(*), intent(in) :: msg
    optional :: msg

    if(.not.this) error stop "Faild assertation"

  end subroutine assert_true
  !=============================================================================
  subroutine assert_equal(this, that, tol, msg)
    real, intent(in) :: this, that, tol
    character(*), intent(in) :: msg
    optional :: tol, msg

    real, parameter :: default_tol = 1.0e-6
    real :: tol_

    tol_ = default_tol; if(present(tol)) tol_ = tol
    if(.not.abs(this-that)<tol_)  error stop "Faild assertation"

  end subroutine assert_equal

end module fc_assert
