module prism
  use element_interface, only: ele_t
  use fc, only: dp_
  implicit none

  public :: pri_t

  private

  !=============================================================================
  !> parameter definition:
  integer, parameter :: pri_md_ = 3
  !< max accuracy of degree for prirahedron type.
  integer, parameter :: pri_nq_degree_(1:3) = [1,4,5]
  !< number of quadratures for degree [i] in prirahedron type.
  integer, parameter :: pri_nn_degree_(1:3) = [4,10,20]
  !< number of local points for degree [i] in prirahedron type.

  !=============================================================================
  !> type definition:
  type, extends(ele_t) :: pri_t

  contains
    !--------------
    ! Constructors
    !--------------
    procedure :: init => pri_init
    procedure :: make_shape => pri_make_shape
    procedure :: make_quadrature => pri_make_quadrature
    procedure :: build => pri_build

  end type pri_t

contains
  !=============================================================================
  subroutine pri_build(this, degree)

    class(pri_t), intent(inout) :: this
    integer, intent(in) :: degree

    call this%init(degree)
    call this%make_quadrature()
    call this%make_shape()

  end subroutine pri_build
  !=============================================================================
  subroutine pri_init(this, degree)

    class(pri_t), intent(inout) :: this
    integer, intent(in) :: degree

    this%nd = 3
    this%nv = 6
    this%ne = 9
    this%ns = 5
    this%md = pri_md_

    this%name   = 'prism'
    this%degree = degree
    if(this%degree < 1 .or. this%degree > pri_md_) error stop &
    "Unsupported degree for prism element"
    this%nq = pri_nq_degree_(degree)
    this%nn = pri_nn_degree_(degree)

    call this%allocate()

  end subroutine pri_init
  !=============================================================================
  subroutine pri_make_quadrature(this)

    class(pri_t), intent(inout) :: this

    select case(this%nq)
    case(1)

    case default
      error stop "Unsupported quadrature points number for a prism."
    end select

  end subroutine pri_make_quadrature
  !=============================================================================
  subroutine pri_make_shape(this)

    class(pri_t), intent(inout) :: this

    real, pointer :: qp(:) => null()
    real :: c1,c2,c3
    integer :: i

    do i = 1,this%nq
      qp => this%qp(i,:)
      c1 = qp(1)
      c2 = qp(2)
      c3 = qp(3)

      select case(this%nn)
      case(6)

      case default
        error stop 'Unsupported shape functions for prism.'
      end select

    enddo

    nullify(qp)

  end subroutine pri_make_shape

end module prism
