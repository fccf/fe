module fc_debug
  use iso_fortran_env, only: error_unit, output_unit, &
                           & compiler_version, compiler_options
  use fc_string
  implicit none

  public :: set_debug_level, debug_unit, debug_error, debug_level_
  public :: debug_assert_true, debug_assert_equal
  public :: set_asser_tolerance
  public :: assert_info, assert_clear
  public :: compile_info

  private

  integer, save :: debug_level_  = 1
  real, save    :: default_tolerance = 1.0e-6
  logical, allocatable      :: passed(:)
  character(:), allocatable :: fail_msg

contains
  !=============================================================================
  subroutine set_debug_level(level)
    !< set debug_level_, only print `level < debug_level_` information
    integer,intent(in) :: level

    debug_level_ = level

  end subroutine set_debug_level
  !=============================================================================
  function debug_unit(level)
    !< choose where to print, level<1',print to `ERROR_UNIT`, else print to `OUTPUT_UNIT`
    integer, intent(in) :: level
    integer :: debug_unit

    if(level<1) then
       debug_unit = error_unit
    else
       debug_unit = output_unit
    end if

  end function debug_unit
  !=============================================================================
  function debug_location(file, line) result(loc)
    !< return the file name and line as a string
    character(*), intent(in) :: file
    integer, intent(in) :: line
    character(:), allocatable :: loc

    loc = '('//file//', '//to_str(line)//' )'

  end function debug_location
  !=============================================================================
  subroutine debug_error(msg, file, line)
    !< print error message, contains which file and line
    character(*), intent(in) :: msg, file
    integer, intent(in) :: line

    error stop msg//new_line('a')//'Location:'//debug_location(file, line)

  end subroutine debug_error
  !=============================================================================
  subroutine debug_assert_true(this, file, line)
    !< assert true
    logical, intent(in)      :: this
    character(*), intent(in) :: file
    integer, intent(in)      :: line

    logical :: pass

    if(.not.allocated(passed)) passed = [logical ::]
    if(.not.allocated(fail_msg)) fail_msg = 'Faild assertation location:'

    pass = this; passed = [passed,pass]
    if(.not.pass) fail_msg = fail_msg//new_line('a')//debug_location(file,line)

  end subroutine debug_assert_true
  !=============================================================================
  subroutine debug_assert_equal(lhs, rhs, file, line)
    !< assert real equal
    real, intent(in) :: lhs, rhs
    character(*), intent(in) :: file
    integer, intent(in)      :: line

    logical :: pass

    if(.not.allocated(passed)) passed = [logical ::]
    if(.not.allocated(fail_msg)) fail_msg = 'Faild assertation location:'

    pass = abs(rhs-lhs)<default_tolerance; passed = [passed,pass]
    if(.not.pass) fail_msg = fail_msg//new_line('a')//debug_location(file,line)

  end subroutine debug_assert_equal
  !=============================================================================
  subroutine set_asser_tolerance(tol)
    !< set real equal tolerence
    real, intent(in) :: tol
    default_tolerance = tol
  end subroutine set_asser_tolerance
  !=============================================================================
  function assert_info()
    !< assert information string
    character(:), allocatable :: assert_info
    assert_info = "Total assertation number is "//to_str(size(passed))//', '
    if(all(passed)) then
      assert_info =assert_info//'All passed.'
    else
      assert_info = assert_info//to_str(size(passed)-count(passed))//&
                  & ' not passed. '//fail_msg
    endif

  end function assert_info
  !=============================================================================
  subroutine assert_clear()
    !< clear current assertation
    if(allocated(passed)) deallocate(passed)
    if(allocated(fail_msg)) deallocate(fail_msg)
  end subroutine assert_clear
  !=============================================================================
  function compile_info()
    !< compile information
    character(:), allocatable :: compile_info

    compile_info = "Compile with: "//compiler_version()//new_line('a')//&
                 & "Compile options: "//compiler_options()

#ifdef __DATE__ && __TIME__
    compile_info = compile_info//new_line('a')//"Compile time: "//__DATE__//' '//__TIME__
#endif

  end function compile_info

end module fc_debug
