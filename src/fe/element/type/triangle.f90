module triangle
  use element_interface, only: ele_t
  implicit none

  public :: tri_t

  private

  !=============================================================================
  !> parameter definition:
  integer, parameter :: tri_md_ = 3
  !< max accuracy of degree for triangle type.
  integer, parameter :: tri_nq_degree_(0:tri_md_) = [1,3,4,6]
  !< number of quadratures for degree [i] in triangle type.
  integer, parameter :: tri_nn_degree_(0:tri_md_) = [3,3,6,10]
  !< number of local points for degree [i] in triangle type.

  integer, parameter :: dp_ = selected_real_kind(14)
  !=============================================================================
  !> type definition:
  type, extends(ele_t) :: tri_t
    !< triangle type, which contains 1-4 accuracy of degree.

  contains
    !--------------
    !> Constructors
    !--------------
    procedure :: init => tri_init
    procedure :: make_shape => tri_make_shape
    procedure :: make_quadrature => tri_make_quadrature
    procedure :: build => tri_build

  end type tri_t

contains
  !=============================================================================
  subroutine tri_build(this, degree)

    class(tri_t), intent(inout) :: this
    integer, intent(in) :: degree

    call this%init(degree)
    call this%make_quadrature()
    call this%make_shape()

  end subroutine tri_build
  !=============================================================================
  subroutine tri_init(this, degree)

    class(tri_t), intent(inout) :: this
    integer, intent(in) :: degree

    this%nd = 2
    this%nv = 3
    this%ne = 3
    this%ns = 1
    this%md = tri_md_

    this%name   = 'triangle'
    this%degree = degree
    if(this%degree < 1 .or. this%degree > tri_md_) error stop &
    "Unsupported degree for tri_t element"
    this%nq = tri_nq_degree_(degree)
    this%nn = tri_nn_degree_(degree)

    call this%allocate()

  end subroutine tri_init
  !=============================================================================
  subroutine tri_make_quadrature(this)

    class(tri_t), intent(inout) :: this

    select case(this%nq)
    case(1)
      this%qp(1,1)= 0.333333333333333_dp_
      this%qp(1,2)= 0.333333333333333_dp_
      this%qw(1) = 0.500000000000000_dp_
    case(3)
      this%qp(1,1)= 0.500000000000000_dp_
      this%qp(1,2)= 0.500000000000000_dp_
      this%qp(2,1)= 0.500000000000000_dp_
      this%qp(2,2)= 0.000000000000000_dp_
      this%qp(3,1)= 0.000000000000000_dp_
      this%qp(3,2)= 0.500000000000000_dp_
      this%qw(1:3)=0.333333333333333_dp_
      this%qw=0.5_dp_*this%qw
    case(4)
      this%qp(1,1)= 0.6_dp_
      this%qp(1,2)= 0.2_dp_
      this%qp(2,1)= 0.2_dp_
      this%qp(2,2)= 0.6_dp_
      this%qp(3,1)= 0.2_dp_
      this%qp(3,2)= 0.2_dp_
      this%qp(4,1)= 0.333333333333333_dp_
      this%qp(4,2)= 0.333333333333333_dp_
      this%qw(1:3)= 0.520833333333333_dp_
      this%qw(4)=  -0.5625_dp_
      this%qw=0.5_dp_*this%qw
    case(6)
      this%qp(1,1)= 0.816847572980459_dp_
      this%qp(1,2)= 0.091576213509771_dp_
      this%qp(2,1)= 0.091576213509771_dp_
      this%qp(2,2)= 0.816847572980459_dp_
      this%qp(3,1)= 0.091576213509771_dp_
      this%qp(3,2)= 0.091576213509771_dp_
      this%qp(4,1)= 0.108103018168070_dp_
      this%qp(4,2)= 0.445948490915965_dp_
      this%qp(5,1)= 0.445948490915965_dp_
      this%qp(5,2)= 0.108103018168070_dp_
      this%qp(6,1)= 0.445948490915965_dp_
      this%qp(6,2)= 0.445948490915965_dp_
      this%qw(1:3)=0.109951743655322_dp_
      this%qw(4:6)=0.223381589678011_dp_
      this%qw=0.5_dp_*this%qw
    case default
      error stop "Unsupported quadrature points number for a triangle."
    end select

  end subroutine tri_make_quadrature
  !=============================================================================
  subroutine tri_make_shape(this)

    class(tri_t), intent(inout) :: this

    real, pointer :: qp(:) => null()
    real :: c1,c2,c3
    real :: t1,t2,t3,t4,t5,t6,t7,t8,t9
    integer :: i

    do i = 1,this%nq
      qp => this%qp(i,:)
      c1 = qp(1)
      c2 = qp(2)
      c3 = 1.0 - c1 - c2

      select case (this%nn)
      case (3)
        this%n(:,i) = [c1, c2, c3]
        this%dn(1,:,i) = [1.0_dp_, 0.0_dp_, -1.0_dp_]  !< dn/dc1
        this%dn(2,:,i) = [0.0_dp_, 1.0_dp_, -1.0_dp_]  !< dn/dc2

      case (6)
        this%n(1,i) = (2.0_dp_*c1 - 1.0_dp_)*c1
        this%n(2,i) = 4.0_dp_*c3*c1
        this%n(3,i) = (2.0_dp_*c3 - 1.0_dp_)*c3
        this%n(4,i) = 4.0_dp_*c2*c3
        this%n(5,i) = (2.0_dp_*c2 - 1.0_dp_)*c2
        this%n(6,i) = 4.0_dp_*c1*c2

        this%dn(1,1,i) = 4.0_dp_*c1 - 1.0_dp_
        this%dn(1,2,i) = 4.0_dp_*(c3-c1)
        this%dn(1,3,i) = -(4.0_dp_*c3 - 1.0_dp_)
        this%dn(1,4,i) = -4.0_dp_*c2
        this%dn(1,5,i) = 0.0_dp_
        this%dn(1,6,i) = 4.0_dp_*c2
        this%dn(2,1,i) = 0.0_dp_
        this%dn(2,2,i) = -4.0_dp_*c1
        this%dn(2,3,i) = -(4.0_dp_*c3 - 1.0_dp_)
        this%dn(2,4,i) = 4.0_dp_*(c3-c2)
        this%dn(2,5,i) = 4.0_dp_*c2 - 1.0_dp_
        this%dn(2,6,i) = 4.0_dp_*c1

      case (10)
        this%n(1,i)= ((3.0_dp_*c1-1.0_dp_)*(3.0_dp_*c1-2.0_dp_)*c1)/2.0_dp_
        this%n(2,i)= -(9.0_dp_*(3.0_dp_*c1-1.0_dp_)*(c1+c2-1.0_dp_)*c1)/2.0_dp_
        this%n(3,i)=  (9.0_dp_*(3.0_dp_*c1+3.0_dp_*c2-2.0_dp_)*(c1+c2-1.0_dp_)*c1)/2.0_dp_
        this%n(4,i)=-((3.0_dp_*c1+3.0_dp_*c2-1.0_dp_)*(3.0_dp_*c1+3.0_dp_*c2-2.0_dp_)*(c1+c2-1.0_dp_))/2.0_dp_
        this%n(5,i)=  (9.0_dp_*(3.0_dp_*c1+3.0_dp_*c2-2.0_dp_)*(c1+c2-1.0_dp_)*c2)/2.0_dp_
        this%n(6,i)= -(9.0_dp_*(c1+c2-1.0_dp_)*(3.0_dp_*c2-1.0_dp_)*c2)/2.0_dp_
        this%n(7,i)= ((3.0_dp_*c2-1.0_dp_)*(3.0_dp_*c2-2.0_dp_)*c2)/2.0_dp_
        this%n(8,i)=  (9.0_dp_*(3.0_dp_*c2-1.0_dp_)*c1*c2)/2.0_dp_
        this%n(9,i)=  (9.0_dp_*(3.0_dp_*c1-1.0_dp_)*c1*c2)/2.0_dp_
        this%n(10,i)=-27.0_dp_*((c2-1.0_dp_)+c1)*c1*c2

        this%dn(1,1,i)=(27.0_dp_*c1**2-18.0_dp_*c1+2.0_dp_)/2.0_dp_
        this%dn(1,2,i)=-(9.0_dp_*(9.0_dp_*c1**2+6.0_dp_*c1*c2-8.0_dp_*c1-c2+1.0_dp_))/2.0_dp_
        this%dn(1,3,i)= (9.0_dp_*(9.0_dp_*c1**2+12.0_dp_*c1*c2-10.0_dp_*c1+3.0_dp_*c2**2-5.0_dp_*c2+2.0_dp_))/2.0_dp_
        this%dn(1,4,i)=-(27.0_dp_*c1**2+54.0_dp_*c1*c2-36.0_dp_*c1+27.0_dp_*c2**2-36.0_dp_*c2+11.0_dp_)/2.0_dp_
        this%dn(1,5,i)= (9.0_dp_*(6.0_dp_*c1+6.0_dp_*c2-5.0_dp_)*c2)/2.0_dp_
        this%dn(1,6,i)=-(9.0_dp_*(3.0_dp_*c2-1.0_dp_)*c2)/2.0_dp_
        this%dn(1,7,i)=0.0_dp_
        this%dn(1,8,i)=(9.0_dp_*(3.0_dp_*c2-1.0_dp_)*c2)/2.0_dp_
        this%dn(1,9,i)=(9.0_dp_*(6.0_dp_*c1-1.0_dp_)*c2)/2.0_dp_
        this%dn(1,10,i)=-27.0_dp_*(((c2-1.0_dp_)+c1)+c1)*c2

        this%dn(2,1,i)=0.0_dp_
        this%dn(2,2,i)=-(9.0_dp_*(3.0_dp_*c1-1.0_dp_)*c1)/2.0_dp_
        this%dn(2,3,i)= (9.0_dp_*(6.0_dp_*c1+6.0_dp_*c2-5.0_dp_)*c1)/2.0_dp_
        this%dn(2,4,i)=-(27.0_dp_*c1**2+54.0_dp_*c1*c2-36.0_dp_*c1+27.0_dp_*c2**2-36.0_dp_*c2+11.0_dp_)/2.0_dp_
        this%dn(2,5,i)= (9.0_dp_*(3.0_dp_*c1**2+12.0_dp_*c1*c2-5.0_dp_*c1+9.0_dp_*c2**2-10.0_dp_*c2+2.0_dp_))/2.0_dp_
        this%dn(2,6,i)=-(9.0_dp_*((c1+c2-1.0_dp_)*(6.0_dp_*c2-1.0_dp_)+(3.0_dp_*c2-1.0_dp_)*c2))/2.0_dp_
        this%dn(2,7,i)=(27.0_dp_*c2**2-18.0_dp_*c2+2.0_dp_)/2.0_dp_
        this%dn(2,8,i)= (9.0_dp_*(6.0_dp_*c2-1.0_dp_)*c1)/2.0_dp_
        this%dn(2,9,i)= (9.0_dp_*(3.0_dp_*c1-1.0_dp_)*c1)/2.0_dp_
        this%dn(2,10,i)=-27.0_dp_*(((c2-1.0_dp_)+c1)+c2)*c1

      case (15)
        t1=c1-0.25_dp_
        t2=c1-0.5_dp_
        t3=c1-0.75_dp_
        t4=c2-0.25_dp_
        t5=c2-0.5_dp_
        t6=c2-0.75_dp_
        t7=c3-0.25_dp_
        t8=c3-0.5_dp_
        t9=c3-0.75_dp_
        this%n(1,i)=32.0_dp_/3.0_dp_*c1*t1*t2*t3
        this%n(2,i)=128.0_dp_/3.0_dp_*c3*c1*t1*t2
        this%n(3,i)=64.0_dp_*c3*c1*t1*t7
        this%n(4,i)=128.0_dp_/3.0_dp_*c3*c1*t7*t8
        this%n(5,i)=32.0_dp_/3.0_dp_*c3*t7*t8*t9
        this%n(6,i)=128.0_dp_/3.0_dp_*c2*c3*t7*t8
        this%n(7,i)=64.0_dp_*c2*c3*t4*t7
        this%n(8,i)=128.0_dp_/3.0_dp_*c2*c3*t4*t5
        this%n(9,i)=32.0_dp_/3.0_dp_*c2*t4*t5*t6
        this%n(10,i)=128.0_dp_/3.0_dp_*c1*c2*t4*t5
        this%n(11,i)=64.0_dp_*c1*c2*t1*t4
        this%n(12,i)=128.0_dp_/3.0_dp_*c1*c2*t1*t2
        this%n(13,i)=128.0_dp_*c1*c2*t1*c3
        this%n(14,i)=128.0_dp_*c1*c2*c3*t7
        this%n(15,i)=128.0_dp_*c1*c2*c3*t4

        this%dn(1,1,i)=32.0_dp_/3.0_dp_*(t2*t3*(t1+c1)+c1*t1*(t3+t2))
        this%dn(1,2,i)=128.0_dp_/3.0_dp_*(c3*(t2*(t1+c1)+c1*t1)-c1*t1*t2)
        this%dn(1,3,i)=64.0_dp_*(c3*t7*(t1+c1)-c1*t1*(t7+c3))
        this%dn(1,4,i)=128.0_dp_/3.0_dp_*(c3*t7*t8-c1*(t8*(t7+c3)+c3*t7))
        this%dn(1,5,i)=-32.0_dp_/3.0_dp_*(t8*t9*(t7+c3)+c3*t7*(t8+t9))
        this%dn(1,6,i)=-128.0_dp_/3.0_dp_*c2*(t8*(t7+c3)+c3*t7)
        this%dn(1,7,i)=-64.0_dp_*c2*t4*(t7+c3)
        this%dn(1,8,i)=-128.0_dp_/3.0_dp_*c2*t4*t5
        this%dn(1,9,i)=0.0_dp_
        this%dn(1,10,i)=128.0_dp_/3.0_dp_*c2*t4*t5
        this%dn(1,11,i)=64.0_dp_*c2*t4*(t1+c1)
        this%dn(1,12,i)=128.0_dp_/3.0_dp_*c2*(t2*(t1+c1)+c1*t1)
        this%dn(1,13,i)=128.0_dp_*c2*(c3*(t1+c1)-c1*t1)
        this%dn(1,14,i)=128.0_dp_*c2*(c3*t7-c1*(t7+c3))
        this%dn(1,15,i)=128.0_dp_*c2*t4*(c3-c1)

        this%dn(2,1,i)=0.0_dp_
        this%dn(2,2,i)=-128.0_dp_/3.0_dp_*c1*t1*t2
        this%dn(2,3,i)=-64.0_dp_*c1*t1*(t7+c3)
        this%dn(2,4,i)=-128.0_dp_/3.0_dp_*c1*(t8*(t7+c3)+c3*t7)
        this%dn(2,5,i)=-32.0_dp_/3.0_dp_*(t8*t9*(t7+c3)+c3*t7*(t8+t9))
        this%dn(2,6,i)=128.0_dp_/3.0_dp_*(c3*t7*t8-c2*(t8*(t7+c3)+c3*t7))
        this%dn(2,7,i)=64.0_dp_*(c3*t7*(t4+c2)-c2*t4*(t7+c3))
        this%dn(2,8,i)=128.0_dp_/3.0_dp_*((c3*(t5*(t4+c2)+c2*t4))-c2*t4*t5)
        this%dn(2,9,i)=32.0_dp_/3.0_dp_*(t5*t6*(t4+c2)+c2*t4*(t6+t5))
        this%dn(2,10,i)=128.0_dp_/3.0_dp_*c1*(t5*(t4+c2)+c2*t4)
        this%dn(2,11,i)=64.0_dp_*c1*t1*(t4+c2)
        this%dn(2,12,i)=128.0_dp_/3.0_dp_*c1*t1*t2
        this%dn(2,13,i)=128.0_dp_*c1*t1*(c3-c2)
        this%dn(2,14,i)=128.0_dp_*c1*(c3*t7-c2*(c3+t7))
        this%dn(2,15,i)=128.0_dp_*c1*(c3*(t4+c2)-c2*t4)

      case default
        error stop 'Unsupported shape functions for triangle'
      end select

    enddo

    nullify(qp)

  end subroutine tri_make_shape

end module triangle
