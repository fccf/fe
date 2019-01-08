module quadrilateral
  use element_interface, only: ele_t
  use fc, only: dp_
  implicit none

  public :: qua_t

  private

  !=============================================================================
  !> parameter definition:
  integer, parameter :: qua_md_ = 4
  !< max accuracy of degree for quadrilateral type.
  integer, parameter :: qua_nq_degree_(1:4) = [1,4,9,16]
  !< number of quadratures for degree [i] in quadrilateral type.
  integer, parameter :: qua_nn_degree_(1:4) = [4,5,8,9]
  !< number of local points for degree [i] in quadrilateral type.

  !=============================================================================
  type, extends(ele_t) :: qua_t

  contains
    !--------------
    ! Constructors
    !--------------
    procedure :: init => qua_init
    procedure :: make_shape => qua_make_shape
    procedure :: make_quadrature => qua_make_quadrature
    procedure :: build => qua_build

  end type qua_t

contains
  !=============================================================================
  subroutine qua_build(this, degree)

    class(qua_t), intent(inout) :: this
    integer, intent(in) :: degree

    call this%init(degree)
    call this%make_quadrature()
    call this%make_shape()

  end subroutine qua_build
  !=============================================================================
  subroutine qua_init(this, degree)

    class(qua_t), intent(inout) :: this
    integer, intent(in) :: degree

    this%nd = 2
    this%nv = 4
    this%ne = 4
    this%ns = 1
    this%md = qua_md_

    this%name   = 'quadrilateral'
    this%degree = degree
    if(this%degree < 1 .or. this%degree > qua_md_) error stop &
    "Unsupported degree for qua_t element"
    this%nq = qua_nq_degree_(degree)
    this%nn = qua_nn_degree_(degree)

    call this%allocate()

  end subroutine qua_init
  !=============================================================================
  subroutine qua_make_quadrature(this)

    class(qua_t), intent(inout) :: this

    real, parameter :: invroot3 = 1.0_dp_/sqrt(3.0_dp_)
    real, parameter :: pt2root15= 0.2_dp_*sqrt(15.0_dp_)

    select case(this%nq)
    case(1)
      this%qp(1,1)=0.0_dp_
      this%qp(1,2)=0.0_dp_
      this%qw(1)=4.0_dp_

    case(4)
      this%qp(1,1)=-invroot3
      this%qp(1,2)= invroot3
      this%qp(2,1)= invroot3
      this%qp(2,2)= invroot3
      this%qp(3,1)=-invroot3
      this%qp(3,2)=-invroot3
      this%qp(4,1)= invroot3
      this%qp(4,2)=-invroot3
      this%qw(1:4)= 1.0_dp_

    case(9)
      this%qp(1:7:3,1)=-pt2root15
      this%qp(2:8:3,1)=0.0_dp_
      this%qp(3:9:3,1)=pt2root15
      this%qp(1:3,2)  =pt2root15
      this%qp(4:6,2)  =0.0_dp_
      this%qp(7:9,2)  =-pt2root15
      this%qw(1) = 25.0_dp_/81.0_dp_
      this%qw(2) = 40.0_dp_/81.0_dp_
      this%qw(3) = 25.0_dp_/81.0_dp_
      this%qw(4) = 40.0_dp_/81.0_dp_
      this%qw(5) = 64.0_dp_/81.0_dp_
      this%qw(6) = 40.0_dp_/81.0_dp_
      this%qw(7) = 25.0_dp_/81.0_dp_
      this%qw(8) = 40.0_dp_/81.0_dp_
      this%qw(9) = 25.0_dp_/81.0_dp_

    case(16)
      this%qp(1:13:4,1)=-0.861136311594053_dp_
      this%qp(2:14:4,1)=-0.339981043584856_dp_
      this%qp(3:15:4,1)= 0.339981043584856_dp_
      this%qp(4:16:4,1)= 0.861136311594053_dp_
      this%qp(1:4,2)   = 0.861136311594053_dp_
      this%qp(5:8,2)   = 0.339981043584856_dp_
      this%qp(9:12,2)  =-0.339981043584856_dp_
      this%qp(13:16,2) =-0.861136311594053_dp_
      this%qw(1)       = 0.121002993285602_dp_
      this%qw(2)       = 0.226851851851852_dp_
      this%qw(3)       = this%qw(2)
      this%qw(4)       = this%qw(1)
      this%qw(5)       = this%qw(2)
      this%qw(6)       = 0.425293303010694_dp_
      this%qw(7)       = this%qw(6)
      this%qw(8)       = this%qw(2)
      this%qw(9)       = this%qw(2)
      this%qw(10)      = this%qw(6)
      this%qw(11)      = this%qw(6)
      this%qw(12)      = this%qw(2)
      this%qw(13)      = this%qw(1)
      this%qw(14)      = this%qw(2)
      this%qw(15)      = this%qw(2)
      this%qw(16)      = this%qw(1)

    case default
      error stop "Unsupported quadrature points number for a quadrilateral."
    end select

  end subroutine qua_make_quadrature
  !=============================================================================
  subroutine qua_make_shape(this)

    class(qua_t), intent(inout) :: this

    real, pointer :: qp(:) => null()
    real :: c1,c2,c1m,c2m,c1p,c2p
    integer :: i

    do i = 1,this%nq
      qp => this%qp(i,:)
      c1 = qp(1)
      c2 = qp(2)
      c1m = 0.25_dp_*(1.0_dp_-c1)
      c2m = 0.25_dp_*(1.0_dp_-c2)
      c1p = 0.25_dp_*(1.0_dp_+c1)
      c2p = 0.25_dp_*(1.0_dp_+c2)

      select case (this%nn)
      case(4) !< 4 vertices
        this%n(1,i)=4.0_dp_*c1m*c2m
        this%n(2,i)=4.0_dp_*c1m*c2p
        this%n(3,i)=4.0_dp_*c1p*c2p
        this%n(4,i)=4.0_dp_*c1p*c2m

        this%dn(1,1,i)=-c2m
        this%dn(1,2,i)=-c2p
        this%dn(1,3,i)=c2p
        this%dn(1,4,i)=c2m
        this%dn(2,1,i)=-c1m
        this%dn(2,2,i)=c1m
        this%dn(2,3,i)=c1p
        this%dn(2,4,i)=-c1p

      case(5) !< 4 vertices + 1 surface center
        this%n(1,i)=4.0_dp_*c1m*c2m-0.25_dp_*(1.0_dp_-c1*c1)*(1.0_dp_-c2*c2)
        this%n(2,i)=4.0_dp_*c1m*c2p-0.25_dp_*(1.0_dp_-c1*c1)*(1.0_dp_-c2*c2)
        this%n(3,i)=4.0_dp_*c1p*c2p-0.25_dp_*(1.0_dp_-c1*c1)*(1.0_dp_-c2*c2)
        this%n(4,i)=4.0_dp_*c1p*c2m-0.25_dp_*(1.0_dp_-c1*c1)*(1.0_dp_-c2*c2)
        this%n(5,i)=(1.0_dp_-c1*c1)*(1.0_dp_-c2*c2)

        this%dn(1,1,i)=-c2m+0.5_dp_*c1*(1.0_dp_-c2*c2)
        this%dn(1,2,i)=-c2p+0.5_dp_*c1*(1.0_dp_-c2*c2)
        this%dn(1,3,i)=c2p+0.5_dp_*c1*(1.0_dp_-c2*c2)
        this%dn(1,4,i)=c2m+0.5_dp_*c1*(1.0_dp_-c2*c2)
        this%dn(1,5,i)=-2.0_dp_*c1*(1.0_dp_-c2*c2)
        this%dn(2,1,i)=-c1m+0.5_dp_*c2*(1.0_dp_-c1*c1)
        this%dn(2,2,i)=c1m+0.5_dp_*c2*(1.0_dp_-c1*c1)
        this%dn(2,3,i)=c1p+0.5_dp_*c2*(1.0_dp_-c1*c1)
        this%dn(2,4,i)=-c1p+0.5_dp_*c2*(1.0_dp_-c1*c1)
        this%dn(2,5,i)=-2.0_dp_*c2*(1.0_dp_-c1*c1)

      case(8) !< 4 vertices + 4 edges center
        this%n(1,i)=4.0_dp_*c2m*c1m*(-c1-c2-1.0_dp_)
        this%n(2,i)=32.0_dp_*c2m*c1m*c2p
        this%n(3,i)=4.0_dp_*c2p*c1m*(-c1+c2-1.0_dp_)
        this%n(4,i)=32.0_dp_*c1m*c1p*c2p
        this%n(5,i)=4.0_dp_*c2p*c1p*(c1+c2-1.0_dp_)
        this%n(6,i)=32.0_dp_*c2p*c1p*c2m
        this%n(7,i)=4.0_dp_*c1p*c2m*(c1-c2-1.0_dp_)
        this%n(8,i)=32.0_dp_*c1m*c1p*c2m

        this%dn(1,1,i)=c2m*(2.0_dp_*c1+c2)
        this%dn(1,2,i)=-8.0_dp_*c2m*c2p
        this%dn(1,3,i)=c2p*(2.0_dp_*c1-c2)
        this%dn(1,4,i)=-4.0_dp_*c2p*c1
        this%dn(1,5,i)=c2p*(2.0_dp_*c1+c2)
        this%dn(1,6,i)=8.0_dp_*c2p*c2m
        this%dn(1,7,i)=c2m*(2.0_dp_*c1-c2)
        this%dn(1,8,i)=-4.0_dp_*c2m*c1
        this%dn(2,1,i)=c1m*(c1+2.0_dp_*c2)
        this%dn(2,2,i)=-4.0_dp_*c1m*c2
        this%dn(2,3,i)=c1m*(2.0_dp_*c2-c1)
        this%dn(2,4,i)=8.0_dp_*c1m*c1p
        this%dn(2,5,i)=c1p*(c1+2.0_dp_*c2)
        this%dn(2,6,i)=-4.0_dp_*c1p*c2
        this%dn(2,7,i)=c1p*(2.0_dp_*c2-c1)
        this%dn(2,8,i)=-8.0_dp_*c1m*c1p

      case(9) !< 4 vertices + 4 edges center + surface center
        c2m=c2-1.0_dp_
        c2p=c2+1.0_dp_
        c1m=c1-1.0_dp_
        c1p=c1+1.0_dp_
        this%n(1,i)=0.25_dp_*c1*c1m*c2*c2m
        this%n(2,i)=-0.5_dp_*c1*c1m*c2p*c2m
        this%n(3,i)=0.25_dp_*c1*c1m*c2*c2p
        this%n(4,i)=-0.5_dp_*c1p*c1m*c2*c2p
        this%n(5,i)=0.25_dp_*c1*c1p*c2*c2p
        this%n(6,i)=-0.5_dp_*c1*c1p*c2p*c2m
        this%n(7,i)=0.25_dp_*c1*c1p*c2*c2m
        this%n(8,i)=-0.5_dp_*c1p*c1m*c2*c2m
        this%n(9,i)=c1p*c1m*c2p*c2m

        this%dn(1,1,i)=0.25_dp_*(2.0_dp_*c1-1.0_dp_)*c2*c2m
        this%dn(1,2,i)=-0.5_dp_*(2.0_dp_*c1-1.0_dp_)*c2p*c2m
        this%dn(1,3,i)=0.25_dp_*(2.0_dp_*c1-1.0_dp_)*c2*c2p
        this%dn(1,4,i)=-c1*c2*c2p
        this%dn(1,5,i)=0.25_dp_*(2.0_dp_*c1+1.0_dp_)*c2*c2p
        this%dn(1,6,i)=-0.5_dp_*(2.0_dp_*c1+1.0_dp_)*c2p*c2m
        this%dn(1,7,i)=0.25_dp_*(2.0_dp_*c1+1.0_dp_)*c2*c2m
        this%dn(1,8,i)=-c1*c2*c2m
        this%dn(1,9,i)=2.0_dp_*c1*c2p*c2m
        this%dn(2,1,i)=0.25_dp_*c1*c1m*(2.0_dp_*c2-1.0_dp_)
        this%dn(2,2,i)=-c1*c1m*c2
        this%dn(2,3,i)=0.25_dp_*c1*c1m*(2.0_dp_*c2+1.0_dp_)
        this%dn(2,4,i)=-0.5_dp_*c1p*c1m*(2.0_dp_*c2+1.0_dp_)
        this%dn(2,5,i)=0.25_dp_*c1*c1p*(2.0_dp_*c2+1.0_dp_)
        this%dn(2,6,i)=-c1*c1p*c2
        this%dn(2,7,i)=0.25_dp_*c1*c1p*(2.0_dp_*c2-1.0_dp_)
        this%dn(2,8,i)=-0.5_dp_*c1p*c1m*(2.0_dp_*c2-1.0_dp_)
        this%dn(2,9,i)=2.0_dp_*c1p*c1m*c2

      case default
        error stop 'Unsupported shape this%nctions for quadrilateral'
      end select

    enddo

    nullify(qp)

  end subroutine qua_make_shape


end module quadrilateral
