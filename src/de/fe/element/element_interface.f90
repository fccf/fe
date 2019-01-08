module element_interface
  use fc, only: ref_count
  use blas_interface, only: det, invert
  implicit none

  public :: ele_t

  private

  !=============================================================================
  type, abstract, extends(ref_count) :: ele_t
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

    call this%incref()

  end subroutine element_allocate
  !=============================================================================
  elemental subroutine element_destroy(this)
    class(ele_t), intent(inout) :: this

    call this%decref()
    if(this%hasref()) return

    this%nd = 0
    this%nv = 0
    this%nn = 0
    this%nq = 0
    this%degree = 0

    deallocate(this%qp, this%qw, this%n, this%dn)

  end subroutine element_destroy
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

end module element_interface
