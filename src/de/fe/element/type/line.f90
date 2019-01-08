module line
  use element_interface, only: ele_t
  use fc, only: dp_
  implicit none

  public :: lin_t

  private

  !=============================================================================
  !> parameter definition:
  integer, parameter :: lin_md_ = 4
  !< max accuracy of degree for line type.
  integer, parameter :: lin_nq_degree_(1:4) = [1,2,3,4]
  !< number of quadratures for degree [i] in line type.
  integer, parameter :: lin_nn_degree_(1:4) = [2,3,4,5]
  !< number of local points for degree [i] in line type.

  !=============================================================================
  !> type definition:
  type, extends(ele_t) :: lin_t
    !< line type, which contains 1-4 accuracy of degree.

  contains
    !--------------
    !> Constructors
    !--------------
    procedure :: init => lin_init
    procedure :: make_shape => lin_make_shape
    procedure :: make_quadrature => lin_make_quadrature
    procedure :: build => lin_build

  end type lin_t

contains

  !=============================================================================
  subroutine lin_build(this, degree)

    class(lin_t), intent(inout) :: this
    integer, intent(in) :: degree

    call this%init(degree)
    call this%make_quadrature()
    call this%make_shape()

  end subroutine lin_build
  !=============================================================================
  subroutine lin_init(this, degree)

    class(lin_t), intent(inout) :: this
    integer, intent(in) :: degree

    this%nd = 1
    this%nv = 2
    this%ne = 0
    this%ns = 0
    this%md = lin_md_

    this%name   = 'line'
    this%degree = degree
    if(this%degree < 1 .or. this%degree > lin_md_) error stop &
    "Unsupported degree for lin_t element"
    this%nq = lin_nq_degree_(degree)
    this%nn = lin_nn_degree_(degree)

    call this%allocate()

  end subroutine lin_init
  !=============================================================================
  subroutine lin_make_quadrature(this)

    class(lin_t), intent(inout) :: this

    select case(this%nq)
    case(1)
      this%qp(1,1)=0.0_dp_
      this%qw(1) =2.0_dp_
    case(2)
      this%qp(1,1)=-0.577350269189626_dp_
      this%qp(2,1)= 0.577350269189626_dp_
      this%qw(1) = 1.000000000000000_dp_
      this%qw(2) = 1.000000000000000_dp_
    case(3)
      this%qp(1,1)=-0.774596669241484_dp_
      this%qp(2,1)= 0.000000000000000_dp_
      this%qp(3,1)= 0.774596669241484_dp_
      this%qw(1) = 0.555555555555556_dp_
      this%qw(2) = 0.888888888888889_dp_
      this%qw(3) = 0.555555555555556_dp_
    case(4)
      this%qp(1,1)=-0.861136311594053_dp_
      this%qp(2,1)=-0.339981043584856_dp_
      this%qp(3,1)= 0.339981043584856_dp_
      this%qp(4,1)= 0.861136311594053_dp_
      this%qw(1) = 0.347854845137454_dp_
      this%qw(2) = 0.652145154862546_dp_
      this%qw(3) = 0.652145154862546_dp_
      this%qw(4) = 0.347854845137454_dp_
    case default
      error stop "Unsupported quadrature points number for a lin_t."
    end select

  end subroutine lin_make_quadrature
  !=============================================================================
  subroutine lin_make_shape(this)

    class(lin_t), intent(inout) :: this

    real, pointer :: qp(:) => null()
    real :: xi
    real :: t1,t2,t3,t4,t5
    integer :: i

    do i = 1,this%nq
      qp => this%qp(i,:)
      xi=qp(1)

      select case(this%nn)
      case(2)
        t1=-1.0_dp_-xi
        t2= 1.0_dp_-xi
        this%n(1,i)=t2/2.0_dp_
        this%n(2,i)=-t1/2.0_dp_

        this%dn(1,1,i)=-0.5_dp_
        this%dn(1,2,i)= 0.5_dp_

      case(3)
        t1=-1.0_dp_-xi
        t2=-xi
        t3=1.0_dp_-xi
        this%n(1,i)=t2*t3/2.0_dp_
        this%n(2,i)=-t1*t3
        this%n(3,i)=t1*t2/2.0_dp_

        this%dn(1,1,i)=-(t3+t2)/2.0_dp_
        this%dn(1,2,i)=(t3+t1)
        this%dn(1,3,i)=-(t2+t1)/2.0_dp_

      case(4)
        t1=-1.0_dp_-xi
        t2=-1.0_dp_/3.0_dp_-xi
        t3=1.0_dp_/3.0_dp_-xi
        t4=1.0_dp_-xi
        this%n(1,i)=t2*t3*t4*9.0_dp_/16.0_dp_
        this%n(2,i)=-t1*t3*t4*27.0_dp_/16.0_dp_
        this%n(3,i)=t1*t2*t4*27.0_dp_/16.0_dp_
        this%n(4,i)=-t1*t2*t3*9.0_dp_/16.0_dp_

        this%dn(1,1,i)=-(t3*t4+t2*t4+t2*t3)*9.0_dp_/16.0_dp_
        this%dn(1,2,i)=(t3*t4+t1*t4+t1*t3)*27.0_dp_/16.0_dp_
        this%dn(1,3,i)=-(t2*t4+t1*t4+t1*t2)*27.0_dp_/16.0_dp_
        this%dn(1,4,i)=(t2*t3+t1*t3+t1*t2)*9.0_dp_/16.0_dp_

      case(5)
        t1=-1.0_dp_ -xi
        t2=-0.5_dp_-xi
        t3=-xi
        t4=0.5_dp_-xi
        t5=1.0_dp_-xi
        this%n(1,i)=t2*t3*t4*t5*2.0_dp_/3.0_dp_
        this%n(2,i)=-t1*t3*t4*t5*8.0_dp_/3.0_dp_
        this%n(3,i)=t1*t2*t4*t5*4.0_dp_
        this%n(4,i)=-t1*t2*t3*t5*8.0_dp_/3.0_dp_
        this%n(5,i)=t1*t2*t3*t4*2.0_dp_/3.0_dp_

        this%dn(1,1,i)=-(t3*t4*t5+t2*t4*t5+t2*t3*t5+t2*t3*t4)*2.0_dp_/3.0_dp_
        this%dn(1,2,i)=(t3*t4*t5+t1*t4*t5+t1*t3*t5+t1*t3*t4)*8.0_dp_/3.0_dp_
        this%dn(1,3,i)=-(t2*t4*t5+t1*t4*t5+t1*t2*t5+t1*t2*t4)*4.0_dp_
        this%dn(1,4,i)=(t2*t3*t5+t1*t3*t5+t1*t2*t5+t1*t2*t3)*8.0_dp_/3.0_dp_
        this%dn(1,5,i)=-(t2*t3*t4+t1*t3*t4+t1*t2*t4+t1*t2*t3)*2.0_dp_/3.0_dp_
      case default
        error stop "Unsupported shape functions"
      end select

    enddo

    nullify(qp)

  end subroutine lin_make_shape


end module line
