module element_factory
  use line
  use triangle
  use quadrilateral
  use tetrahedron
  use prism
  use hexahedron
  use element_interface
  implicit none

  public :: choose_element

  private

  !=============================================================================
  !>  Parameter giving the number of different element type available.
  integer, parameter :: num_element_type_= 6
  integer, parameter :: lin_type_ = 1
  integer, parameter :: tri_type_ = 2
  integer, parameter :: qua_type_ = 3
  integer, parameter :: tet_type_ = 4
  integer, parameter :: pri_type_ = 5
  integer, parameter :: hex_type_ = 6


  !=============================================================================
  !> Overload the element factory routines
  interface choose_element
    module procedure :: choose_element_by_int
    module procedure :: choose_element_by_name
  end interface choose_element

contains
  !=============================================================================
  subroutine choose_element_by_name(this, type, degree)

    class(ele_t), intent(inout), pointer :: this
    character(*), intent(in) :: type
    integer, intent(in) :: degree

    nullify(this)

    select case(trim(adjustl(type)))
    case('lin', 'line')
      allocate(lin_t::this)

    case('tri', 'triangle')
      allocate(tri_t::this)

    case('qua', 'quadrilateral')
      allocate(qua_t::this)

    case('tet', 'tetrahedron')
      allocate(tet_t::this)

    case('pri', 'prism')
      allocate(pri_t::this)

    case('hex', 'hexahedron')
      allocate(hex_t::this)

    case default
      error stop 'Unsupported element type.'

    end select

    call this%build(degree)

  end subroutine choose_element_by_name
  !=============================================================================
  subroutine choose_element_by_int(this, type, degree)
    class(ele_t), intent(inout), pointer :: this
    integer, intent(in) :: type
    integer, intent(in) :: degree

    nullify(this)

    select case(type)
    case(lin_type_)
      allocate(lin_t::this)

    case(tri_type_)
      allocate(tri_t::this)

    case(qua_type_)
      allocate(qua_t::this)

    case(tet_type_)
      allocate(tet_t::this)

    case(pri_type_)
      allocate(pri_t::this)

    case(hex_type_)
      allocate(hex_t::this)

    case default
      error stop 'Unsupported element type.'

    end select

    call this%build(degree)

  end subroutine choose_element_by_int


end module element_factory
