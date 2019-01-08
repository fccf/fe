module mesh
  use calculate_volume
  implicit none

  public :: mesh_t

  private
  !=============================================================================
  type node_t
    integer :: nd  !< number of dimension
    integer :: nn  !< number of nodes
    integer, allocatable :: id(:)    !< nodes index (nn)
    real, allocatable    :: coo(:,:) !< cooinate (nd,nn)
  contains
    procedure :: destroy => node_destroy
    procedure :: byte => node_byte
    ! final     :: node_final
  end type node_t
  !=============================================================================
  type connect_t
    integer :: type  !< element type
    integer :: ne  !< number of element
    integer :: enn !< element nodes number
    integer, allocatable :: eni(:,:) !< element node index (enn, ne)
    integer, allocatable :: id(:)  !< element index
    integer, allocatable :: er(:)  !< element region
    real, allocatable    :: ev(:) !< element volume
  contains
    procedure :: destroy => connect_destroy
    procedure :: byte => connect_byte
    ! final     :: connect_final
  end type connect_t
  !=============================================================================
  type :: mesh_t
    !< meshometry
    logical         :: is_init = .false.
    type(node_t)    :: nodes
    type(connect_t) :: domain
    type(connect_t) :: boundary
  contains
    procedure :: destroy => mesh_destroy
    procedure :: cal_vol => mesh_cal_volume
    procedure :: byte => mesh_byte
    ! final     :: mesh_final
  end type mesh_t

contains
  !=============================================================================
  pure function node_byte(this) result(res)
    class(node_t), intent(in) :: this
    integer(8) :: res
    res = (2 + size(this%id))* kind(0)
    res = res + size(this%coo) * kind(0.0)
  end function node_byte
  !=============================================================================
  pure function connect_byte(this) result(res)
    class(connect_t), intent(in) :: this
    integer(8) :: res
    res = (3 + size(this%id) + size(this%eni) + size(this%er))* kind(0)
    res = res + size(this%ev) * kind(0.0)
  end function connect_byte
  !=============================================================================
  pure function mesh_byte(this) result(res)
    class(mesh_t), intent(in) :: this
    integer(8) :: res
    res = this%nodes%byte()
    res = res + this%domain%byte()
    res = res + this%boundary%byte()
  end function mesh_byte
  !=============================================================================
  subroutine node_destroy(this)
    class(node_t), intent(inout) :: this

    this%nn = 0
    this%nd = 0
    if(allocated(this%id)) deallocate(this%id)
    if(allocated(this%coo))deallocate(this%coo)

  end subroutine node_destroy
  !=============================================================================
  subroutine node_final(this)
    type(node_t), intent(inout) :: this
    call this%destroy()
  end subroutine node_final
  !=============================================================================
  subroutine connect_destroy(this)
    class(connect_t), intent(inout) :: this
    this% type = 0
    this% ne   = 0
    this% enn  = 0
    if(allocated(this%eni)) deallocate(this%eni)
    if(allocated(this%id)) deallocate(this%id)
    if(allocated(this%er)) deallocate(this%er)
    if(allocated(this%ev)) deallocate(this%ev)
  end subroutine connect_destroy
  !=============================================================================
  subroutine connect_final(this)
    type(connect_t), intent(inout) :: this
    call this%destroy()
  end subroutine connect_final
  !=============================================================================
  subroutine mesh_destroy(this)
    class(mesh_t), intent(inout) :: this
    call this%nodes%destroy()
    call this%domain%destroy()
    call this%boundary%destroy()
  end subroutine mesh_destroy
  !=============================================================================
  subroutine mesh_final(this)
    type(mesh_t), intent(inout) :: this
    call this%destroy()
  end subroutine mesh_final
  !=============================================================================
  subroutine mesh_cal_volume(this)
    class(mesh_t), intent(inout) :: this

    if(.not.this%is_init) error stop 'the mesh has not been initialized.'
    call cal_mesh_volume(this%domain,this%nodes)
    call cal_mesh_volume(this%boundary,this%nodes)

  end subroutine mesh_cal_volume
  !=============================================================================
  subroutine cal_mesh_volume(this,node)
    class(connect_t),intent(inout) :: this
    class(node_t), intent(in)   :: node
    real ::line_coo(3,2), tri_coo(3,3),tet_coo(3,4)

    integer :: ie
    line_coo = 0.0
    tri_coo  = 0.0
    tet_coo  = 0.0

    allocate(this%ev(this%ne))
    !calculate the volume of  the domain mesh
    do ie =1, this%ne
      associate(num=>this%eni(:,ie),vol => this%ev(ie))
        associate(coo=>node%coo(:,num))

          if(this%type == 3)then
            tet_coo = coo(1:3,1:4)
            vol =  cal_tet_vol(tet_coo)

          elseif(this%type == 2)then
            tri_coo(1:node%nd,1:3) = coo(1:node%nd,1:3)
            vol =  cal_tri_area(tri_coo)

          elseif(this%type  == 1)then
            line_coo(1:node%nd,1:2) = coo(1:node%nd,1:2)
            vol = cal_line_len(line_coo)
          elseif(this%type == 0) then
            vol = 1.0
          else
            error stop 'dimension error.'

          end if

        end associate

      end associate

    end do

  end subroutine cal_mesh_volume

end module mesh
