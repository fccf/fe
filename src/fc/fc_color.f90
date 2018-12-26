module fc_color
  use fc_string, only: to_str, lower
  implicit none

  public :: colorize, color_map

  private

  interface colorize
    module procedure colorize_rgb, colorize_default
  end interface colorize

  interface color_map
    module procedure color_map_range
  end interface color_map

contains
  !=============================================================================
  function colorize_default(s, color) result(o)
    !< Bracket a string with text to change its color on a terminal
    character(*),intent(in) :: s !< String to colorize
    character(*),intent(in) :: color !< color
    character(:),allocatable:: o
    select case (lower(color))
    case('red');   o = colorize(s,[5,0,0])
    case('green'); o = colorize(s,[0,5,0])
    case('blue');  o = colorize(s,[0,0,5])
    case('yello'); o = colorize(s,[5,5,0])
    case('cyan');  o = colorize(s,[5,0,5])
    case('purple');o = colorize(s,[0,5,5])
    case('black'); o = colorize(s,[0,0,0])
    case('white'); o = colorize(s,[5,5,5])
    case default;  o = s
    end select
  end function colorize_default
  !=============================================================================
  function colorize_rgb(s,c) result(o)
    !< Bracket a string with text to change its color on a terminal
    character(*),intent(in)::s !< String to colorize
    integer,dimension(3),intent(in)::c ! c in [0,5]
    !< Color to use in [r,g,b] format, where \(r,b,g \in [0,5]\)
    character(:),allocatable::o

    ! character(1),parameter::CR  = achar(13)
    character(1),parameter::ESC = achar(27)
    character(3),parameter::post = '[0m'

    character(:),allocatable::pre

    pre = ESC//'[38;5;'//to_str(36*c(1)+6*c(2)+c(3)+16)//'m'
    o = trim(pre)//s//ESC//post

  end function colorize_rgb
  !=============================================================================
  function color_map_range(v,r) result(c)
    !< Return the color code for colorize based on the coolwarm color map
    real,intent(in)::v
    !! Value to map
    real,dimension(2),intent(in)::r
    !! Range over which to scale the colors
    integer,dimension(3)::c

    integer::s

    if(v<sum(r)/2.0) then
      s = nint((v-r(1))/(sum(r)/2.0-r(1))*5.0)
      c = [s,s,5]
    else
      s = 5-nint((v-sum(r)/2.0)/(r(2)-sum(r)/2.0)*5.0)
      c = [5,s,s]
    end if

  end function color_map_range

end module fc_color
