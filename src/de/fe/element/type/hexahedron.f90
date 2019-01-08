module hexahedron
  use element_interface, only: ele_t
  use fc, only: dp_
  implicit none

  public :: hex_t

  private

  !=============================================================================
  !> parameter definition:
  integer, parameter :: hex_md_ = 2
  !< max accuracy of degree for hexahedron type.
  integer, parameter :: hex_nq_degree_(1:2) = [1,8]
  !< number of quadratures for degree [i] in hexahedron type.
  integer, parameter :: hex_nn_degree_(1:2) = [8,14]
  !< number of local points for degree [i] in hexahedron type.


  !=============================================================================
  type, extends(ele_t) :: hex_t

  contains
    !--------------
    ! Constructors
    !-------------
    procedure :: init => hex_init
    procedure :: make_shape => hex_make_shape
    procedure :: make_quadrature => hex_make_quadrature
    procedure :: build => hex_build

  end type hex_t

contains
  !=============================================================================
  subroutine hex_build(this, degree)
    class(hex_t), intent(inout) :: this
    integer, intent(in) :: degree

    call this%init(degree)
    call this%make_quadrature()
    call this%make_shape()

  end subroutine hex_build
  !=============================================================================
  subroutine hex_init(this, degree)
    class(hex_t), intent(inout) :: this
    integer, intent(in) :: degree

    this%nd = 3
    this%nv = 8
    this%ne = 12
    this%ns = 6
    this%md = hex_md_

    this%name   = 'hexahedron'
    this%degree = degree
    if(this%degree < 1 .or. this%degree > hex_md_) error stop &
    "Unsupported degree for hex_t element"
    this%nq = hex_nq_degree_(degree)
    this%nn = hex_nn_degree_(degree)

    call this%allocate()

  end subroutine hex_init
  !=============================================================================
  subroutine hex_make_quadrature(this)
    class(hex_t), intent(inout) :: this

    real, parameter :: invroot3 = 1.0_dp_/sqrt(3.0_dp_)
    real, parameter :: c1=0.795822426_dp_, c2=0.758786911_dp_

    select case(this%nq)
    case(1)
      this%qp(1,1:3)=0.0_dp_
      this%qw(1)=8.0_dp_

    case(8)
      this%qp(1,1)= invroot3
      this%qp(1,2)= invroot3
      this%qp(1,3)= invroot3
      this%qp(2,1)= invroot3
      this%qp(2,2)= invroot3
      this%qp(2,3)=-invroot3
      this%qp(3,1)= invroot3
      this%qp(3,2)=-invroot3
      this%qp(3,3)= invroot3
      this%qp(4,1)= invroot3
      this%qp(4,2)=-invroot3
      this%qp(4,3)=-invroot3
      this%qp(5,1)=-invroot3
      this%qp(5,2)= invroot3
      this%qp(5,3)= invroot3
      this%qp(6,1)=-invroot3
      this%qp(6,2)=-invroot3
      this%qp(6,3)= invroot3
      this%qp(7,1)=-invroot3
      this%qp(7,2)= invroot3
      this%qp(7,3)=-invroot3
      this%qp(8,1)=-invroot3
      this%qp(8,2)=-invroot3
      this%qp(8,3)=-invroot3
      this%qw=1.0_dp_

    case(14)
      this%qw(1:6)=0.886426593_dp_
      this%qw(7:14)=0.335180055_dp_
      this%qp(1,1)=-c1
      this%qp(2,1)=c1
      this%qp(3,2)=-c1
      this%qp(4,2)=c1
      this%qp(5,3)=-c1
      this%qp(6,3)=c1
      this%qp(7:,:)=c2
      this%qp(7,1)=-c2
      this%qp(7,2)=-c2
      this%qp(7,3)=-c2
      this%qp(8,2)=-c2
      this%qp(8,3)=-c2
      this%qp(9,1)=-c2
      this%qp(9,3)=-c2
      this%qp(10,3)=-c2
      this%qp(11,1)=-c2
      this%qp(11,2)=-c2
      this%qp(12,2)=-c2
      this%qp(13,1)=-c2

    case default
      error stop "Unsupported quadrature points number for a hexahedron"
    end select

  end subroutine hex_make_quadrature
  !=============================================================================
  subroutine hex_make_shape(this)
    class(hex_t), intent(inout) :: this
    real, pointer :: qp(:) => null()
    real :: c1,c2,c3,c1m,c2m,c3m,c1p,c2p,c3p
    integer :: i

    do i = 1,this%nq
      qp => this%qp(i,:)
      c1 = qp(1)
      c2 = qp(2)
      c3 = qp(3)
      c1m = 1.0_dp_-c1
      c2m = 1.0_dp_-c2
      c3m = 1.0_dp_-c3
      c1p = 1.0_dp_+c1
      c2p = 1.0_dp_+c2
      c3p = 1.0_dp_+c3

      select case(this%nn)
      case(8) !< 8 vertices
        this%n(1,i)=0.125_dp_*c1m*c2m*c3m
        this%n(2,i)=0.125_dp_*c1m*c2m*c3p
        this%n(3,i)=0.125_dp_*c1p*c2m*c3p
        this%n(4,i)=0.125_dp_*c1p*c2m*c3m
        this%n(5,i)=0.125_dp_*c1m*c2p*c3m
        this%n(6,i)=0.125_dp_*c1m*c2p*c3p
        this%n(7,i)=0.125_dp_*c1p*c2p*c3p
        this%n(8,i)=0.125_dp_*c1p*c2p*c3m

        this%dn(1,1,i)=-0.125_dp_*c2m*c3m
        this%dn(1,2,i)=-0.125_dp_*c2m*c3p
        this%dn(1,3,i)= 0.125_dp_*c2m*c3p
        this%dn(1,4,i)= 0.125_dp_*c2m*c3m
        this%dn(1,5,i)=-0.125_dp_*c2p*c3m
        this%dn(1,6,i)=-0.125_dp_*c2p*c3p
        this%dn(1,7,i)= 0.125_dp_*c2p*c3p
        this%dn(1,8,i)= 0.125_dp_*c2p*c3m
        this%dn(2,1,i)=-0.125_dp_*c1m*c3m
        this%dn(2,2,i)=-0.125_dp_*c1m*c3p
        this%dn(2,3,i)=-0.125_dp_*c1p*c3p
        this%dn(2,4,i)=-0.125_dp_*c1p*c3m
        this%dn(2,5,i)= 0.125_dp_*c1m*c3m
        this%dn(2,6,i)= 0.125_dp_*c1m*c3p
        this%dn(2,7,i)= 0.125_dp_*c1p*c3p
        this%dn(2,8,i)= 0.125_dp_*c1p*c3m
        this%dn(3,1,i)=-0.125_dp_*c1m*c2m
        this%dn(3,2,i)= 0.125_dp_*c1m*c2m
        this%dn(3,3,i)= 0.125_dp_*c1p*c2m
        this%dn(3,4,i)=-0.125_dp_*c1p*c2m
        this%dn(3,5,i)=-0.125_dp_*c1m*c2p
        this%dn(3,6,i)= 0.125_dp_*c1m*c2p
        this%dn(3,7,i)= 0.125_dp_*c1p*c2p
        this%dn(3,8,i)=-0.125_dp_*c1p*c2p

      case(14) !< 8 vertices + 6 surface centre
        this%n(1,i) = (c1*c2+c1*c3+2.0_dp_*c1+c2*c3+2.0_dp_*c2+2.0_dp_*c3+2.0_dp_)*c1m*c2m*c3m/8.0_dp_
        this%n(2,i) =-(c1*c2-c1*c3+2.0_dp_*c1-c2*c3+2.0_dp_*c2-2.0_dp_*c3+2.0_dp_)*c1m*c2m*c3p/8.0_dp_
        this%n(3,i) =-(c1*c2-c1*c3+2.0_dp_*c1+c2*c3-2.0_dp_*c2+2.0_dp_*c3-2.0_dp_)*c1p*c2m*c3p/8.0_dp_
        this%n(4,i) = (c1*c2+c1*c3+2.0_dp_*c1-c2*c3-2.0_dp_*c2-2.0_dp_*c3-2.0_dp_)*c1p*c2m*c3m/8.0_dp_
        this%n(5,i) =-c1p*c1m*c2m*c3p*c3m/2.0_dp_
        this%n(6,i) =-c1m*c2p*c2m*c3p*c3m/2.0_dp_
        this%n(7,i) = c1p*c1m*c2p*c2m*c3p/2.0_dp_
        this%n(8,i) = c1p*c2p*c2m*c3p*c3m/2.0_dp_
        this%n(9,i) =-c1p*c1m*c2p*c2m*c3m/2.0_dp_
        this%n(10,i)= (c1*c2-c1*c3-2.0_dp_*c1+c2*c3+2.0_dp_*c2-2.0_dp_*c3-2.0_dp_)*c1m*c2p*c3m/8.0_dp_
        this%n(11,i)=-(c1*c2+c1*c3-2.0_dp_*c1-c2*c3+2.0_dp_*c2+2.0_dp_*c3-2.0_dp_)*c1m*c2p*c3p/8.0_dp_
        this%n(12,i)=-(c1*c2+c1*c3-2.0_dp_*c1+c2*c3-2.0_dp_*c2-2.0_dp_*c3+2.0_dp_)*c1p*c2p*c3p/8.0_dp_
        this%n(13,i)= (c1*c2-c1*c3-2.0_dp_*c1-c2*c3-2.0_dp_*c2+2.0_dp_*c3+2.0_dp_)*c1p*c2p*c3m/8.0_dp_
        this%n(14,i)= c1p*c1m*c2p*c3p*c3m/2.0_dp_

        this%dn(1,1,i)= (2.0_dp_*c1*c2+2.0_dp_*c1*c3+4.0_dp_*c1+c2*c3+c2+c3)*c2m*c3m/8.0_dp_
        this%dn(1,2,i)=-(2.0_dp_*c1*c2-2.0_dp_*c1*c3+4.0_dp_*c1-c2*c3+c2-c3)*c2m*c3p/8.0_dp_
        this%dn(1,3,i)=-(2.0_dp_*c1*c2-2.0_dp_*c1*c3+4.0_dp_*c1+c2*c3-c2+c3)*c2m*c3p/8.0_dp_
        this%dn(1,4,i)= (2.0_dp_*c1*c2+2.0_dp_*c1*c3+4.0_dp_*c1-c2*c3-c2-c3)*c2m*c3m/8.0_dp_
        this%dn(1,5,i)= -c2m*c3p*c3m*c1
        this%dn(1,6,i)=-c2p*c2m*c3p*c3m/2.0_dp_
        this%dn(1,7,i)=  c2p*c2m*c3p*c1
        this%dn(1,8,i)= c2p*c2m*c3p*c3m/2.0_dp_
        this%dn(1,9,i)= -c2p*c2m*c3m*c1
        this%dn(1,10,i)= (2.0_dp_*c1*c2-2.0_dp_*c1*c3-4.0_dp_*c1+c2*c3+c2-c3)*c2p*c3m/8.0_dp_
        this%dn(1,11,i)=-(2.0_dp_*c1*c2+2.0_dp_*c1*c3-4.0_dp_*c1-c2*c3+c2+c3)*c2p*c3p/8.0_dp_
        this%dn(1,12,i)=-(2.0_dp_*c1*c2+2.0_dp_*c1*c3-4.0_dp_*c1+c2*c3-c2-c3)*c2p*c3p/8.0_dp_
        this%dn(1,13,i)= (2.0_dp_*c1*c2-2.0_dp_*c1*c3-4.0_dp_*c1-c2*c3-c2+c3)*c2p*c3m/8.0_dp_
        this%dn(1,14,i)=  c2p*c3p*c3m*c1
        this%dn(2,1,i)= (2.0_dp_*c1*c2+c1*c3+c1+2.0_dp_*c2*c3+4.0_dp_*c2+c3)*c1m*c3m/8.0_dp_
        this%dn(2,2,i)=-(2.0_dp_*c1*c2-c1*c3+c1-2.0_dp_*c2*c3+4.0_dp_*c2-c3)*c1m*c3p/8.0_dp_
        this%dn(2,3,i)=-(2.0_dp_*c1*c2-c1*c3+c1+2.0_dp_*c2*c3-4.0_dp_*c2+c3)*c1p*c3p/8.0_dp_
        this%dn(2,4,i)= (2.0_dp_*c1*c2+c1*c3+c1-2.0_dp_*c2*c3-4.0_dp_*c2-c3)*c1p*c3m/8.0_dp_
        this%dn(2,5,i)=-c1p*c1m*c3p*c3m/2.0_dp_
        this%dn(2,6,i)= -c1m*c3p*c3m*c2
        this%dn(2,7,i)=  c1p*c1m*c3p*c2
        this%dn(2,8,i)=  c1p*c3p*c3m*c2
        this%dn(2,9,i)= -c1p*c1m*c3m*c2
        this%dn(2,10,i)= (2.0_dp_*c1*c2-c1*c3-c1+2.0_dp_*c2*c3+4.0_dp_*c2-c3)*c1m*c3m/8.0_dp_
        this%dn(2,11,i)=-(2.0_dp_*c1*c2+c1*c3-c1-2.0_dp_*c2*c3+4.0_dp_*c2+c3)*c1m*c3p/8.0_dp_
        this%dn(2,12,i)=-(2.0_dp_*c1*c2+c1*c3-c1+2.0_dp_*c2*c3-4.0_dp_*c2-c3)*c1p*c3p/8.0_dp_
        this%dn(2,13,i)= (2.0_dp_*c1*c2-c1*c3-c1-2.0_dp_*c2*c3-4.0_dp_*c2+c3)*c1p*c3m/8.0_dp_
        this%dn(2,14,i)= c1p*c1m*c3p*c3m/2.0_dp_
        this%dn(3,1,i)= (c1*c2+2.0_dp_*c1*c3+c1+2.0_dp_*c2*c3+c2+4.0_dp_*c3)*c1m*c2m/8.0_dp_
        this%dn(3,2,i)=-(c1*c2-2.0_dp_*c1*c3+c1-2.0_dp_*c2*c3+c2-4.0_dp_*c3)*c1m*c2m/8.0_dp_
        this%dn(3,3,i)=-(c1*c2-2.0_dp_*c1*c3+c1+2.0_dp_*c2*c3-c2+4.0_dp_*c3)*c1p*c2m/8.0_dp_
        this%dn(3,4,i)= (c1*c2+2.0_dp_*c1*c3+c1-2.0_dp_*c2*c3-c2-4.0_dp_*c3)*c1p*c2m/8.0_dp_
        this%dn(3,5,i)= -c1p*c1m*c2m*c3
        this%dn(3,6,i)= -c1m*c2p*c2m*c3
        this%dn(3,7,i)= c1p*c1m*c2p*c2m/2.0_dp_
        this%dn(3,8,i)=  c1p*c2p*c2m*c3
        this%dn(3,9,i)=-c1p*c1m*c2p*c2m/2.0_dp_
        this%dn(3,10,i)= (c1*c2-2.0_dp_*c1*c3-c1+2.0_dp_*c2*c3+c2-4.0_dp_*c3)*c1m*c2p/8.0_dp_
        this%dn(3,11,i)=-(c1*c2+2.0_dp_*c1*c3-c1-2.0_dp_*c2*c3+c2+4.0_dp_*c3)*c1m*c2p/8.0_dp_
        this%dn(3,12,i)=-(c1*c2+2.0_dp_*c1*c3-c1+2.0_dp_*c2*c3-c2-4.0_dp_*c3)*c1p*c2p/8.0_dp_
        this%dn(3,13,i)= (c1*c2-2.0_dp_*c1*c3-c1-2.0_dp_*c2*c3-c2+4.0_dp_*c3)*c1p*c2p/8.0_dp_
        this%dn(3,14,i)=  c1p*c1m*c2p*c3

      case(20) !< 8 vertices + 12 edge centre
        error stop 'Unsupported shape functions for hexahedron currently.'

      case default
        error stop 'Unsupported shape functions for hexahedron.'
      end select

    enddo

    nullify(qp)

  end subroutine hex_make_shape


end module hexahedron
