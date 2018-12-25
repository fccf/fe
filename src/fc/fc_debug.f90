module fc_debug
  use iso_fortran_env, only: error_unit, output_unit, &
                           & compiler_version, compiler_options
  implicit none

  public :: set_debug_level, debug_unit, debug_error, debug_level_
  public :: compile_info, assert_equal

  private

  integer, save :: debug_level_  = 1

contains
  !=============================================================================
  function compile_info()
    character(:), allocatable :: compile_info

    compile_info = "Compile with: "//compiler_version()//new_line('a')//&
                 & "Compile options: "//compiler_options()//new_line('a')//&
                 & "Compile time: "//__DATE__//' '//__TIME__

  end function compile_info
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
  function location(file, line) result(loc)
    !< return the file name and line as a string
    character(*), intent(in) :: file
    integer, intent(in) :: line
    character(:), allocatable :: loc

    character(16) :: line_str

    write(line_str,'(i0)') line

    loc = '('//file//', '//trim(adjustl(line_str))//' )'

  end function location
  !=============================================================================
  subroutine debug_error(msg, file, line)
    !< print error message, contains which file and line
    character(*), intent(in) :: msg, file
    integer, intent(in) :: line

    error stop msg//new_line('a')//location(file, line)

  end subroutine debug_error
  !=============================================================================
  subroutine assert_equal(this, that, tol, msg)
    real, intent(in) :: this, that, tol
    character(*), intent(in) :: msg
    optional :: tol, msg

    real, parameter :: default_tol = 1.0e-6
    real :: tol_

    tol_ = default_tol; if(present(tol)) tol_ = tol
    if(.not.abs(this-that)<tol_)  error stop "Faild assertation. "//msg

  end subroutine assert_equal

end module fc_debug
