! module fc_file
!
!   implicit none
!
! contains
!   !=============================================================================
!   function file_lines(file) result(n)
!     character(*), intent(in) :: file
!     integer :: n
!
!     integer :: unit       !< Logical unit.
!     logical :: exist      !< Check if file exist.
!
!     inquire(file=file, exist=exist)
!
!     if (exist) then
!       open(newunit=unit, file=file)
!       do
!          read(unit, *, err=999, end=999)
!          n = n + 1
!       enddo
!       999 close(unit)
!     else
!       error stop "file not found"
!     endif
!
!   end function file_lines
!   !=============================================================================
!   function read_line(unit, form, ios) result(line)
!     !< Read line from a connected unit.
!     integer,      intent(in)  :: unit  !< Logical unit.
!     character(*), intent(in)  :: form  !< Format of unit.
!     character(:), allocatable :: line  !< Line storage.
!     integer, intent(out)      :: ios
!     optional :: form, ios
!
!     character(:), allocatable :: form_
!     integer    :: ios_ !< IO status code, local variable.
!     character  :: ch   !< Character storage.
!
!     form_ = 'formatted' ; if (present(form)) form_ = lower(form)
!     line = ''
!     select case(form_)
!     case('formatted')
!        do
!           read(unit, "(a)", advance='no', iostat=ios_) ch
!           if(is_iostat_eor(ios_)) exit
!           line = line//ch
!        enddo
!     case('unformatted')
!        do
!           read(unit, iostat=ios_, err=10, end=10) ch
!           if (ch==new_line('a')) then
!              ios_ = iostat_eor
!              exit
!           endif
!           line = line//ch
!        enddo
!     endselect
!     if(present(ios)) ios = ios_
!
!   endfunction read_line
!   !=============================================================================
!   subroutine read_lines(unit, lines, form, iostat, iomsg)
!     !< Read lines (records) from a connected-formatted unit.
!     !<
!     !< @note The connected unit is rewinded. At a successful exit current record is at eof, at the beginning otherwise.
!     !<
!     !< The lines are returned as an array of strings that are read until the eof is reached.
!     !< The line is read as an ascii stream read until the eor is reached.
!     !<
!     !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
!     !<
!     !< @note There is no doctests, this being tested by means of [[read_file]] doctests.
!     integer,          intent(in)               :: unit     !< Logical unit.
!     type(string),     intent(out), allocatable :: lines(:) !< The lines.
!     character(len=*), intent(in),    optional  :: form     !< Format of unit.
!     integer,          intent(out),   optional  :: iostat   !< IO status code.
!     character(len=*), intent(inout), optional  :: iomsg    !< IO status message.
!     type(string)                               :: form_    !< Format of unit, local variable.
!     integer                                    :: iostat_  !< IO status code, local variable.
!     character(len=:), allocatable              :: iomsg_   !< IO status message, local variable.
!     character(kind=CK, len=1)                  :: ch       !< Character storage.
!     integer                                    :: l        !< Counter.
!
!     form_ = 'FORMATTED' ; if (present(form)) form_ = form ; form_ = form_%upper()
!     iomsg_ = repeat(' ', 99) ; if (present(iomsg)) iomsg_ = iomsg
!     rewind(unit)
!     select case(form_%chars())
!     case('FORMATTED')
!        l = 0
!        do
!           read(unit, *, err=10, end=10)
!           l = l + 1
!        enddo
!     case('UNFORMATTED')
!        l = 0
!        do
!           read(unit, err=10, end=10) ch
!           if (ch==new_line('a')) l = l + 1
!        enddo
!     endselect
!     10 rewind(unit)
!     if (l>0) then
!        allocate(lines(1:l))
!        l = 1
!        iostat_ = 0
!        do
!           call lines(l)%read_line(unit=unit, form=form, iostat=iostat_, iomsg=iomsg_)
!           if ((iostat_/=0.and..not.is_iostat_eor(iostat_)).or.(l>=size(lines, dim=1))) then
!              exit
!           endif
!           l = l + 1
!        enddo
!     endif
!     if (present(iostat)) iostat = iostat_
!     if (present(iomsg)) iomsg = iomsg_
!   endsubroutine read_lines
!
! end module fc_file
