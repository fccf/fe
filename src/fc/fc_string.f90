module fc_string
  use iso_fortran_env, only: iostat_eor
  implicit none

  public :: upper, lower

  public :: start_with, end_with

  public :: is_logical, is_int, is_real
  public :: to_logical, to_int, to_real
  public :: to_str

  public :: file_dir, file_base, file_ext, file_name
  public :: file_content, next_line_content

  private

  type str_t
    character(:), allocatable :: raw
  end type str_t

  interface to_str
    module procedure :: scalar_to_string, vector_to_string, matrix_to_string
  end interface to_str

  character(*), parameter :: linux_path_separator_ = '/'

contains
  !=============================================================================
  elemental function upper(this)
    !< Returns string in uppercase
    character(*), intent(in)  :: this
    character(len(this))      :: upper

    integer :: i, dis

    upper = this
    dis = ichar('A')-ichar('a')
    do i = 1, len(upper)
       if ( ichar(upper(i:i)) >= ichar('a') .and. &
          & ichar(upper(i:i)) <= ichar('z')) then
          upper(i:i) = char(ichar(upper(i:i)) + dis)
       end if
    end do

  end function upper
  !=============================================================================
  elemental function lower(this)
    !< Returns string in lowercase
    character(*), intent(in) :: this
    character(len(this))     :: lower

    integer :: i, dis

    lower = this
    dis = ichar('A')-ichar('a')
    do i = 1, len(lower)
       if ( ichar(lower(i:i)) >= ichar('A') .and.&
          & ichar(lower(i:i)) <= ichar('Z')) then
          lower(i:i) = char(ichar(lower(i:i)) - dis)
       end if
    end do

  end function lower
  !=============================================================================
  elemental function insert(this,sub,pos) result(new)
    !< insert string at pos (position)
    character(len=*), intent(in)  :: this
    character(len=*), intent(in)  :: sub
    integer, intent(in)           :: pos
    character(len(this)+len(sub)) :: new

    integer :: ipos

    ipos = min(max(1,pos),len(this))
    new  = this(1:ipos)//sub//this(ipos+1:)

  end function insert
  !=============================================================================
  elemental function replace(this, sub,rep) result(new)
    !< Return a string with first occurrences of substring sub replaced by rep.
    character(len=*), intent(in)          :: this
    character(len=*), intent(in)          :: sub
    character(len=*), intent(in)          :: rep
    character(len(this)-len(sub)+len(rep)):: new

    integer :: pos

    pos = index(string=this, substring=sub)

    if (pos>0) then

       if (pos==1) then

          new = rep//this(len(sub)+1:)
       else
          new = this(1:pos-1)//rep//this(pos+len(sub):)
       endif

    endif

  end function replace
  !=============================================================================
  function replace_all(this, sub, rep) result(new)
    !< Return a string with all occurrences of substring sub replaced by rep.
    character(len=*), intent(in) :: this
    character(len=*), intent(in) :: sub
    character(len=*), intent(in) :: rep
    character(len=:),allocatable :: new

    new = this

    do
       if(index(new,sub) > 0) then
          new = replace(new, sub, rep)
       else
          exit
       endif
    enddo

  end function replace_all
  !=============================================================================
  elemental function delete(this, sub) result(new)
    !< Return a string with first  substring sub deleted
    character(len=*), intent(in)  :: this
    character(len=*), intent(in)  :: sub
    character(len(this))           :: new

    integer :: pos

    pos = index(string=this, substring=sub)

    if (pos>0) then
       if (pos==1) then

          new = this(len(sub)+1:)
       else
          new = this(1:pos-1)//this(pos+len(sub):)
       endif
    endif
    new = trim(adjustl(new))

  end function delete
  !=============================================================================
  pure function count_words(this,sub) result(rst)
    !< count the word `sub` in `this`
    character(*), intent(in):: this
    character(*), intent(in):: sub

    integer :: rst
    integer :: n1,n2

    rst = 0
    if (len(sub)>len(this)) return

    n1 = 1
    do
       n2 = index(string=this(n1:), substring=sub)
       if (n2 > 0) then
          rst = rst + 1
          n1 = n1 + n2 + len(sub)
       else
          exit
       endif
    enddo

  endfunction count_words
  !=============================================================================
  subroutine split(this, words, nwords, sep)
    !< Return a list of words in the string
    character(len=*), intent(in)             :: this
    character(len=*),allocatable,intent(out) :: words(:)
    integer, intent(out), optional           :: nwords
    character(len=*), intent(in), optional   :: sep

    character(len =:),allocatable :: space, table
    character(len =:),allocatable :: str_tmp, sep_

    integer :: i,c,n

    space = ' '
    table = char(9)
    sep_  = space

    if(present(sep)) sep_ = sep

    str_tmp = replace_all(trim(adjustl(this)), table ,sep_)
    str_tmp = str_tmp//sep_

    n = 0
    do
       c = index(str_tmp,sep_)
       if(c == 1) exit
       str_tmp = trim(adjustl(str_tmp(c+1:)))//sep_
       n = n+1
    enddo

    allocate(words(n))

    str_tmp = replace_all(trim(adjustl(this)), table ,sep_)
    str_tmp = str_tmp//sep_
    do i = 1,n

       c = index(str_tmp,sep_)
       words(i) = str_tmp(1:c-1)
       str_tmp = trim(adjustl(str_tmp(c+1:)))//sep_

    end do

    if(allocated(str_tmp))  deallocate(str_tmp)
    if(present(nwords)) nwords = n

  end subroutine split
  !=============================================================================
  function join(words, sep) result(this)
    !< Return the string that is the join of an array of characters.
    character(len=*), intent(in)           :: words(1:)
    character(len=*), intent(in), optional :: sep
    character(len=:), allocatable          :: this
    character(len=:), allocatable          :: sep_

    integer :: i

    sep_ = ' '
    if (present(sep)) sep_ = sep

    this = ''

    do i=2, size(words,1)
       if (trim(words(i))/='') this = this//sep_//trim(adjustl(words(i)))
    enddo

    if (words(1)/='') then
       this = trim(adjustl(words(1)))//this
    else
       this = this(len(sep_)+1:len(this))

    endif

  end function join
  !=============================================================================
  elemental function is_logical(this)
    !< Return true if the string is a logical variable
    character(*),intent(in):: this
    logical :: is_logical

    is_logical = .false.
    select case (lower(trim(adjustl(this))))
    case ('y', 'yes','t', 'true', 'on', 'n', 'no', 'f', 'false', 'off')
      is_logical = .true.
    end select

  endfunction is_logical
  !=============================================================================
  elemental function is_int(this)
    !< Return true if the string is a integer
    character(*),intent(in)   :: this
    logical                   :: is_int

    integer :: ios  !< The iostat of I/O
    integer :: int

    is_int = .false.
    read(this, * ,iostat= ios) int
    if(ios== 0) is_int = .true.

  endfunction is_int
  !=============================================================================
  elemental function is_real(this)
    !< Return true if the string is a real.
    character(*), intent(in) :: this
    logical                  :: is_real

    integer :: ios  !< The iostat of I/O
    real    :: r

    is_real = .false.
    read(this, * ,iostat= ios) r
    if(ios== 0) is_real = .true.

  endfunction is_real
  !=============================================================================
  elemental function to_logical(this)
     !< transform string to logical
    character(*),intent(in) :: this
    logical :: to_logical

    to_logical = .false.
    select case (lower(trim(adjustl(this))))
    case ('y','yes', 't', 'true', 'on')
      to_logical = .true.
    case ('n','no', 'f', 'false', 'off')
      to_logical = .false.
    case default
      to_logical = .false.
      ! error stop 'string "'//trim(adjustl(this))//'" is not a logical identification!'
    end select

  endfunction to_logical

  !=============================================================================
  elemental function to_int(this)
    !< transform string to integer
    character(*),intent(in) :: this
    integer :: to_int
    integer :: ios

    read(this, * ,iostat= ios) to_int
    ! if(ios /= 0) error stop "string "//this//" is not a integer number"

  endfunction to_int
  !=============================================================================
  elemental function to_real(this)
    !< transform string to real
    character(*),intent(in) :: this
    real    :: to_real
    integer :: ios

    read(this, * , iostat= ios) to_real
    ! if(ios /= 0) error stop 'string "'//str//'" is not a real number'

  endfunction to_real

  !=============================================================================
  pure function start_with(this, prefix)
    !< Return true if a string starts with a specified prefix.
    character(len=*), intent(in) :: this
    character(len=*), intent(in) :: prefix
    logical :: start_with

    start_with= .false.
    if (len(prefix)<=len(this)) then
       start_with = index(this, prefix)==1
    endif

  end function start_with
  !=============================================================================
  pure function end_with(this, suffix)
    !< Return true if a string end with a specified suffix.
    character(len=*), intent(in) :: this
    character(len=*), intent(in) :: suffix
    logical :: end_with

    end_with= .false.
    if (len(suffix)<=len(this)) then
       end_with = index(this, suffix, back = .true.)==(len(this) - len(suffix) + 1)
    endif

  end function end_with

  !=============================================================================
  pure function scalar_to_string(value) result(s)
    !< convert any scalar type (integer, real, logical, character(*)) to string
    class(*), intent(in)      :: value
    character(:), allocatable :: s

    integer, parameter        :: max_num_len_ = 32
    character(max_num_len_)   :: ls

    select type(v_p => value)
    type is(integer)
      write(ls,'(i0)') v_p
      s = trim(adjustl(ls))
    type is(real)
      write(ls,fmt=*) v_p
      s = trim(adjustl(ls))
    type is(logical)
      if (v_p) then
        write(ls,'(a)') 'true'
      else
        write(ls,'(a)') 'false'
      end if
      s = trim(adjustl(ls))
    type is(character(*))
      s = trim(adjustl(v_p))
    class default
      write(ls,'(a)') '***'
      s = trim(adjustl(ls))
    end select
  end function scalar_to_string
  !=============================================================================
  pure function vector_to_string(value, vsep, shell) result(s)
    !< convert any vector type (integer, real, logical, character(*)) to string
    class(*), intent(in) :: value(:)
    character(*), intent(in), optional :: vsep   !< vector separator ',', ' '
    logical,intent(in), optional :: shell        !< if have the shell []
    character(:), allocatable :: s

    character(:),allocatable :: lsep
    logical :: lshell
    integer :: n

    lsep = ','      !< default vector separator
    lshell = .TRUE. !< default shell = true
    if(present(vsep)) lsep = vsep !< local optional argument
    if(present(shell)) lshell = shell !< another local argument

    s = ''
    if(lshell) s = '['
    do n = 1, size(value)
      if (n > 1) s = s//lsep
      s = s // to_str(value(n))    !< str => scalar_to_string
    end do
    if(lshell) s = s// ']'

  end function vector_to_string
  !=============================================================================
  pure function matrix_to_string(value, vsep, msep, shell) result(s)
    class(*), intent(in) :: value(:,:)
    character(*), intent(in), optional :: vsep  !< vector separator ',', ' '
    character(*), intent(in), optional :: msep  !< matrix separator ';', new_line('a')
    logical,intent(in), optional :: shell       !< if have the shell []

    character(:), allocatable :: s

    character(:),allocatable :: lvsep, lmsep
    logical :: lshell
    integer   :: n, m
    lvsep = ','
    lmsep = ';'
    lshell = .TRUE.
    if(present(vsep)) lvsep = vsep
    if(present(msep)) lmsep = msep
    if(present(shell)) lshell = shell

    s = ''
    if(lshell) s = '['
    do n = 1, size(value,2)
      if (n > 1) s = s//lmsep
      if(lshell) s = s//'['
      do m = 1, size(value, 1)
        if (m > 1) s = s//lvsep
        s = s // to_str(value(m,n))    !< str => scalar_to_string
      enddo
      if(lshell) s = s//']'
    enddo
    if(lshell) s = s//']'

  end function matrix_to_string
  !=============================================================================
  !!> filename string operation
  !=============================================================================
  function file_dir(this, sep)
    !< Return the directory name of a string containing a file name.
    character(*), intent(in)  :: this     !< The string.
    character(*), intent(in)  :: sep      !< Directory separator.
    character(:), allocatable :: file_dir !< Directory name.
    optional :: sep

    character(:), allocatable :: sep_     !< Separator, default value.
    integer                   :: pos      !< Character position.

    sep_ = linux_path_separator_
    if (present(sep)) sep_ = sep
    pos = index(this, sep_, back=.true.)
    if (pos>0) file_dir = this(1:pos-1)

  endfunction file_dir
  !=============================================================================
  function file_name(this, sep)
    !< Return file name
    character(*), intent(in)  :: this      !< The string.
    character(*), intent(in)  :: sep       !< Directory separator.
    character(:), allocatable :: file_name !< file name.
    optional :: sep

    character(:), allocatable :: sep_     !< Separator, default value.
    integer                   :: pos      !< Character position.

    sep_ = linux_path_separator_
    if (present(sep)) sep_ = sep
    pos  = index(this, sep_, back = .true.)
    file_name = this(pos+1:)

  end function file_name
  !=============================================================================
  function file_base(this, sep)
    !< Return file base name
    character(*), intent(in)  :: this      !< The string.
    character(*), intent(in)  :: sep       !< Directory separator.
    character(:), allocatable :: file_base !< Base file name.
    optional :: sep

    character(:), allocatable :: sep_      !< Separator, default value.
    integer :: st,ed

    sep_ = linux_path_separator_
    if (present(sep)) sep_ = sep
    st  = index(this, sep_, back = .true.)
    ed = index(this, '.', back = .true.)
    file_base = this(st+1:ed-1)

  end function file_base
  !=============================================================================
  function file_ext(this)
    character(*), intent(in)  :: this     !< The string.
    character(:), allocatable :: file_ext !< Base file name.

    integer :: pos      !< Character position.

    pos  = index(this, '.', back = .true.)
    file_ext = this(pos+1:)

  end function file_ext
  !=============================================================================
  function file_content(file) result(string)
    !< Read a file as a single string stream.
    character(len=*), intent(in) :: file       !< File name.
    character(:),allocatable     :: string     !< The string.

    integer   :: unit       !< Logical unit.
    logical   :: exist      !< Check if file exist.
    integer   :: size   !< Size of the file.

    inquire(file=file, exist=exist)
    if (exist) then
      open(newunit=unit, file=file, access='stream', form='unformatted')
      inquire(file=file, size=size)
      allocate(character(len=size) :: string)
      read(unit=unit) string
      close(unit)
    else
      error stop "file not found"
    endif

  end function file_content
  !=============================================================================
  function next_line_content(unit, form, ios) result(line)
    !< Read line from a connected unit.
    integer,      intent(in)  :: unit  !< Logical unit.
    character(*), intent(in)  :: form  !< Format of unit.
    character(:), allocatable :: line  !< Line storage.
    integer, intent(out)      :: ios
    optional :: form, ios

    character(:), allocatable :: form_
    integer    :: ios_ !< IO status code, local variable.
    character  :: ch   !< Character storage.

    form_ = 'formatted' ; if (present(form)) form_ = lower(form)
    line = ''
    select case(form_)
    case('formatted')
       do
          read(unit, "(a)", advance='no', iostat=ios_) ch
          if(is_iostat_eor(ios_)) exit
          line = line//ch
       enddo
    case('unformatted')
       do
          read(unit, iostat=ios_) ch
          if (ch==new_line('a')) then
             ios_ = iostat_eor
             exit
          endif
          line = line//ch
       enddo
    endselect
    if(present(ios)) ios = ios_

  endfunction next_line_content

end module fc_string
