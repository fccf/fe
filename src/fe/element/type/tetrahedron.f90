module tetrahedron
  use element_interface, only: ele_t
  implicit none

  public :: tet_t

  private

  !=============================================================================
  !> parameter definition:
  integer, parameter :: tet_md_ = 3
  !< max accuracy of degree for tetrahedron type.
  integer, parameter :: tet_nq_degree_(1:3) = [1,4,5]
  !< number of quadratures for degree [i] in tetrahedron type.
  integer, parameter :: tet_nn_degree_(1:3) = [4,10,20]
  !< number of local points for degree [i] in tetrahedron type.

  integer, parameter :: dp_ = selected_real_kind(14)
  !=============================================================================
  !> type definition:
  type, extends(ele_t) :: tet_t

  contains
    !--------------
    ! Constructors
    !--------------
    procedure :: init => tet_init
    procedure :: make_shape => tet_make_shape
    procedure :: make_quadrature => tet_make_quadrature
    procedure :: build => tet_build

  end type tet_t

contains
  !=============================================================================
  subroutine tet_build(this, degree)

    class(tet_t), intent(inout) :: this
    integer, intent(in) :: degree

    call this%init(degree)
    call this%make_quadrature()
    call this%make_shape()

  end subroutine tet_build
  !=============================================================================
  subroutine tet_init(this, degree)

    class(tet_t), intent(inout) :: this
    integer, intent(in) :: degree

    this%nd = 3
    this%nv = 4
    this%ne = 6
    this%ns = 4
    this%md = tet_md_

    this%name   = 'tetrahedron'
    this%degree = degree
    if(this%degree < 1 .or. this%degree > tet_md_) error stop &
    "Unsupported degree for tet_t element"
    this%nq = tet_nq_degree_(degree)
    this%nn = tet_nn_degree_(degree)

    call this%allocate()

  end subroutine tet_init
  !=============================================================================
  subroutine tet_make_quadrature(this)

    class(tet_t), intent(inout) :: this

    select case(this%nq)
    case(1)
      this%qp(1,1)=0.25_dp_
      this%qp(1,2)=0.25_dp_
      this%qp(1,3)=0.25_dp_
      this%qw(1)=1.0_dp_/6.0_dp_
    case(4)
      this%qp(1,1)=0.58541020_dp_
      this%qp(1,2)=0.13819660_dp_
      this%qp(1,3)=this%qp(1,2)
      this%qp(2,2)=this%qp(1,1)
      this%qp(2,3)=this%qp(1,2)
      this%qp(2,1)=this%qp(1,2)
      this%qp(3,3)=this%qp(1,1)
      this%qp(3,1)=this%qp(1,2)
      this%qp(3,2)=this%qp(1,2)
      this%qp(4,1)=this%qp(1,2)
      this%qp(4,2)=this%qp(1,2)
      this%qp(4,3)=this%qp(1,2)
      this%qw(1:4)=0.25_dp_/6.0_dp_
    case(5)
      this%qp(1,1)=0.25_dp_
      this%qp(1,2)=0.25_dp_
      this%qp(1,3)=0.25_dp_
      this%qp(2,1)=0.5_dp_
      this%qp(2,2)=1.0_dp_/6.0_dp_
      this%qp(2,3)=this%qp(2,2)
      this%qp(3,2)=0.5_dp_
      this%qp(3,3)=1.0_dp_/6.0_dp_
      this%qp(3,1)=this%qp(3,3)
      this%qp(4,3)=0.5_dp_
      this%qp(4,1)=1.0_dp_/6.0_dp_
      this%qp(4,2)=this%qp(4,1)
      this%qp(5,1)=1.0_dp_/6.0_dp_
      this%qp(5,2)=this%qp(5,1)
      this%qp(5,3)=this%qp(5,1)
      this%qw(1)=-0.8_dp_
      this%qw(2)=9.0_dp_/20.0_dp_
      this%qw(3:5)=this%qw(2)
      this%qw = this%qw/6.0_dp_
    case default
      error stop "Unsupported quadrature points number for a tetrahedron."
    end select

  end subroutine tet_make_quadrature
  !=============================================================================
  subroutine tet_make_shape(this)

    class(tet_t), intent(inout) :: this

    real, pointer :: qp(:) => null()
    real :: c1,c2,c3
    integer :: i

    do i = 1,this%nq
      qp => this%qp(i,:)
      c1 = qp(1)
      c2 = qp(2)
      c3 = qp(3)

      select case(this%nn)
      case(4)
        this%n(1,i)=c1
        this%n(2,i)=c2
        this%n(3,i)=c3
        this%n(4,i)=1.0_dp_-c1-c2-c3

        this%dn(1:3,1:4,i)=0.0_dp_
        this%dn(1,1,i)=1.0_dp_
        this%dn(2,2,i)=1.0_dp_
        this%dn(3,3,i)=1.0_dp_
        this%dn(1,4,i)=-1.0_dp_
        this%dn(2,4,i)=-1.0_dp_
        this%dn(3,4,i)=-1.0_dp_

      case(10)

      case(20)

      case default
        error stop 'Unsupported shape functions for tetrahedron.'
      end select

    enddo

    nullify(qp)

  end subroutine tet_make_shape


end module tetrahedron
