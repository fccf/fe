module fc_file
  use iso_fortran_env, only: iostat_eor, iostat_end
  use fc_string
  implicit none

  public :: file_dir, file_base, file_ext, file_name
  public :: file_is_exist, file_is_open, file_delete

  public :: file_content, file_line_content

  private

  character(*), parameter :: linux_path_separator_ = '/'

contains
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
  function file_is_exist(this) result(is_exist)
    !< confirm if the file is exist
    character(*),intent(in) :: this
    logical :: is_exist
    inquire(file=this, exist=is_exist)
  end function file_is_exist
  !=============================================================================
  function file_is_open(this) result(is_open)
    !< confirm if the file is opened
    character(*),intent(in) :: this
    logical :: is_open
    inquire(file=this, opened=is_open)
  end function file_is_open
  !=============================================================================
  subroutine file_delete(this)
    character(*), intent(in) :: this
    integer :: unit
    logical :: is_exist, is_open

    inquire(file=this, exist=is_exist)
    if (is_exist) then
      inquire(file=this, opened = is_open)
      if (is_open) then
        inquire(file=this, number=unit)
      else
        open(newunit=unit, file=this)
      end if
      close(unit=unit, status='delete')
    end if

  end subroutine file_delete
  !=============================================================================
  function file_content(this) result(string)
    !< Read a file as a single string stream.
    character(len=*), intent(in) :: this       !< File name.
    character(:),allocatable     :: string     !< The string.

    integer   :: unit       !< Logical unit.
    logical   :: exist      !< Check if file exist.
    integer   :: size   !< Size of the file.

    inquire(file=this, exist=exist)
    if (exist) then
      open(newunit=unit, file=this, access='stream', form='unformatted')
      inquire(file=this, size=size)
      allocate(character(len=size) :: string)
      read(unit=unit) string
      close(unit)
    else
      error stop "file "//this//" not found"
    endif

  end function file_content
  !=============================================================================
  function file_line_content(unit, form, ios) result(line)
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

  endfunction file_line_content

end module fc_file
