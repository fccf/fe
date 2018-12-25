module fc_utest
  !< unit test module for fortrna
  implicit none

  type utest_t
    integer :: counter   = 0
    real    :: tolerance = 1.0e-6
    logical, allocatable      :: passed(:)
    character(:), allocatable :: fail_msg
  contains
    generic :: assert => assert_true, assert_equal

    procedure, private :: assert_true  => utest_assert_true
    procedure, private :: assert_equal => utest_assert_equal_tol
  end type utest_t

contains
  !=============================================================================
  subroutine utest_assert_true(this, condition, message)
    class(utest_t), intent(inout) :: this
    logical, intent(in) :: condition
    character(*), intent(in), optional :: message

    if(this%counter == 0) passed = [logical ::]
    this%counter = this%counter + 1

    if(condition) then
      passed = [passed, .TRUE.]
    else
      write(unit=*, fmt=*) 'assertation faild.'
    endif

  end subroutine utest_assert_true
  !=============================================================================
  subroutine utest_assert_equal_tol(this, lhs, rhs, tol, message)
    class(utest_t), intent(inout) :: this
    real, intent(in) :: lhs, rhs
    real, intent(in), optional :: tol
    character(*), intent(in), optional :: message

    if(present(tol)) this%tolerance = tol

    if(this%counter == 0) passed = [logical ::]
    this%counter = this%counter + 1

    if(abs(rhs-lhs)<this%tolerance) then
      passed = [passed, .TRUE.]
    else
      write(unit=*, fmt=*) 'assertation faild.'
    endif

  end subroutine utest_assert_equal_tol

end module fc_utest
