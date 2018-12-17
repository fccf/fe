module element_interface
  implicit none

  public :: ele_t

  private

  !=============================================================================
  type, abstract :: ele_t
    !< abstract element type
    character(:),allocatable :: name !< element name
    integer :: degree  !< accuracy of shape function degree.
    integer :: nd !< number of dimension.
    integer :: nv !< number of vertices.
    integer :: ne !< number of edges.
    integer :: ns !< number of surface.
    integer :: nn !< number of nodes for element.
    integer :: md !< max degree
    integer :: nq !< number of quadratures

    real, pointer :: n(:,:)  => null()
    !< shape function. (nn, nq)

    real, pointer :: dn(:,:,:) => null()
    !< shape function partial derivatives. (nd,nn,nq)

    real, pointer :: qw(:)  => null() !< Quadrature weights. (nq)
    real, pointer :: qp(:,:)=> null() !< Quadrature points. (nq,nd)
  contains
    !-----------------
    !> Constructors
    !-----------------
    procedure :: allocate => element_allocate
    !< allocate arrary, increase reference count

    procedure(init_element_ifc), deferred :: init
    !< Initialize an empty element. reference count add 1.

    procedure(make_element_quadrature_ifc), deferred :: make_quadrature
    !< make the quadrature information for element.

    procedure(make_element_shape_ifc), deferred :: make_shape
    !< evaluate element shape function.

    procedure(build_element_ifc), deferred :: build
    !< Fill an initialized element.


    !-----------
    !> Accessors
    !-----------
    ! procedure :: get_degree
    ! procedure :: get_nq
    ! procedure :: get_nd
    ! procedure :: get_nn
    procedure :: info => element_information


    !---------------
    !> Destructors
    !---------------
    procedure :: destroy => element_destroy
    !< If hasref, decrease reference counting
    !< else set all element attributes to 0 and deallocate any internal data.

    !---------------
    !> to physical
    !---------------
    procedure :: to_physical => element_to_physical

  end type ele_t

  !=============================================================================
  abstract interface

    subroutine init_element_ifc(this, degree)
      import ele_t
      class(ele_t), intent(inout) :: this
      integer, intent(in) :: degree
    end subroutine init_element_ifc

    subroutine build_element_ifc(this, degree)
      import ele_t
      class(ele_t), intent(inout) :: this
      integer, intent(in) :: degree
    end subroutine build_element_ifc

    subroutine make_element_quadrature_ifc(this)
      import ele_t
      class(ele_t), intent(inout) :: this
    end subroutine make_element_quadrature_ifc

    subroutine make_element_shape_ifc(this)
      import ele_t
      class(ele_t), intent(inout) :: this
    end subroutine make_element_shape_ifc
  end interface

contains
  !=============================================================================
  subroutine element_allocate(this)
    class(ele_t), intent(inout) :: this

    allocate(this%qw(this%nq))
    allocate(this%qp(this%nq, this%nd))

    allocate(this%n(this%nn, this%nq))
    allocate(this%dn(this%nd, this%nn, this%nq))

  end subroutine element_allocate
  !=============================================================================
  elemental subroutine element_destroy(this)
    class(ele_t), intent(inout) :: this

    this%nd = 0
    this%nv = 0
    this%nn = 0
    this%nq = 0
    this%degree = 0

    deallocate(this%qp, this%qw, this%n, this%dn)

  end subroutine element_destroy
  !=============================================================================
  subroutine element_information(this, unit)
    class(ele_t), intent(in) :: this
    integer, intent(in) :: unit

    write(unit=unit, fmt=*) 'name = ', this%name
    write(unit=unit, fmt=*) 'nd = ', this%nd
    write(unit=unit, fmt=*) 'nv = ', this%nv
    write(unit=unit, fmt=*) 'ne = ', this%ne
    write(unit=unit, fmt=*) 'ns = ', this%ns
    write(unit=unit, fmt=*) 'nn = ', this%nn
    write(unit=unit, fmt=*) 'nq = ', this%nq
    write(unit=unit, fmt=*) 'md = ', this%md

  end subroutine element_information
  !=============================================================================
  subroutine element_to_physical(this, coo, detwei, dshape)
    class(ele_t), intent(in) :: this
    real, intent(in) :: coo(:,:)  !< (nn, nd) element nodes coordinates
    real, intent(out):: detwei(this%nq)
    real, intent(out), optional :: dshape(this%nd, this%nn, this%nq ) !< (nd, nn, nq) transformed dshape

    real :: detJ
    real :: jac(this%nd, this%nd)
    integer :: iq

    do iq = 1, this%nq
      jac = matmul(this%dn(:,:,iq),coo)

      detJ = det(jac)
      detwei(iq) = abs(detJ)*this%qw(iq)

      if(present(dshape)) then
        call invert(jac)
        dshape(:,:,iq) = matmul(jac, this%dn(:,:,iq))
      endif

    enddo

  end subroutine element_to_physical
  !=============================================================================
  !!> Determinate of matrix
  function det(mat) result(det_out)
    real, intent(in) :: mat(:,:)
    real :: det_out

    det_out = huge(0.0)

    select case (size(mat,1))
    case(1)
       det_out = mat(1,1)
    case(2)
       det_out = (mat(1,1)*mat(2,2))-(mat(1,2)*mat(2,1))
    case(3)
       det_out = mat(1,1)*(mat(2,2)*mat(3,3)-mat(2,3)*mat(3,2)) &
            - mat(1,2)*(mat(2,1)*mat(3,3)-mat(2,3)*mat(3,1)) &
            + mat(1,3)*(mat(2,1)*mat(3,2)-mat(2,2)*mat(3,1))
    case default
       error stop "Determinant not implemented for this dimension"
    end select

  end function det
  !=============================================================================
  !!> Replace the matrix A with its inverse.
  subroutine invert(A)
    real, intent(inout) :: A(:,:)
    real    :: a33(3,3), det_a, tmp

    select case (size(A,1))
    case (1)
       A(1,1)=1.0/A(1,1)
     case (2)
        det_a=A(1,1)*A(2,2)-A(1,2)*A(2,1)
        tmp=A(1,1)
        A(1,1)=A(2,2)
        A(2,2)=tmp
        A(1,2)=-A(1,2)
        A(2,1)=-A(2,1)
        A=A/det_a
    case (3)
       det_a=A(1,1)*(A(2,2)*A(3,3)-A(3,2)*A(2,3)) &
            -A(2,1)*(A(1,2)*A(3,3)-A(3,2)*A(1,3)) &
            +A(3,1)*(A(1,2)*A(2,3)-A(2,2)*A(1,3))

       a33(1,1)=A(2,2)*A(3,3)-A(3,2)*A(2,3)
       a33(1,2)=A(3,2)*A(1,3)-A(1,2)*A(3,3)
       a33(1,3)=A(1,2)*A(2,3)-A(2,2)*A(1,3)

       a33(2,1)=A(2,3)*A(3,1)-A(3,3)*A(2,1)
       a33(2,2)=A(3,3)*A(1,1)-A(1,3)*A(3,1)
       a33(2,3)=A(1,3)*A(2,1)-A(2,3)*A(1,1)

       a33(3,1)=A(2,1)*A(3,2)-A(3,1)*A(2,2)
       a33(3,2)=A(3,1)*A(1,2)-A(1,1)*A(3,2)
       a33(3,3)=A(1,1)*A(2,2)-A(2,1)*A(1,2)

       A=a33/det_a
    case default
      error stop "Invert not implemented for this dimension"
    end select

  end subroutine invert

end module element_interface
