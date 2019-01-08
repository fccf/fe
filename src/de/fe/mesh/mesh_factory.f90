module mesh_factory
  use mesh
  use io
  implicit none

  public :: load_mesh_from_gmsh
  public :: load_mesh_from_hdf5
  public :: save_mesh_to_hdf5
  public :: save_mesh_to_xh5

  private

contains

  !=============================================================================
  subroutine load_mesh_from_gmsh(mesh, gmsh)
    class(mesh_t), intent(inout) :: mesh
    class(msh_file), intent(in) :: gmsh

    if(.not.gmsh%is_init) error stop 'gmsh file has not been initialized.'

    !> copy to mesh%nodes
    mesh%nodes%nd = gmsh%get_nd()
    mesh%nodes%nn = gmsh%get_nn()
    mesh%nodes%id = gmsh%get_nid()
    mesh%nodes%coo = gmsh%get_coo()

    !> copy to mesh%domain
    mesh%domain%ne = gmsh%get_ne()
    mesh%domain%type = gmsh%get_nd()
    mesh%domain%enn = gmsh%get_enn()
    mesh%domain%id = gmsh%get_eid()
    mesh%domain%er = gmsh%get_er()
    mesh%domain%eni = gmsh%get_eni()

    !> copy to mesh%boundary
    mesh%boundary%ne = gmsh%get_nf()
    mesh%boundary%type = gmsh%get_nd() - 1 !< boundary mesh type = space dimension - 1
    mesh%boundary%enn = gmsh%get_fnn()
    mesh%boundary%id = gmsh%get_fid()
    mesh%boundary%er = gmsh%get_fr()
    mesh%boundary%eni = gmsh%get_fni()

    mesh%is_init = .true.

    call mesh%cal_vol()

  end subroutine load_mesh_from_gmsh
  !=============================================================================
  subroutine load_mesh_from_hdf5(mesh, h5, mesh_path)
    type(mesh_t), intent(inout) :: mesh
    type(hdf5_file), intent(in) :: h5
    character(*), intent(in), optional :: mesh_path
    character(:), allocatable :: pre

    pre = '/mesh'
    if(present(mesh_path)) pre = mesh_path

    !> get nodes
    call h5%get(pre//'/nodes/nd', mesh%nodes%nd)
    call h5%get(pre//'/nodes/nn', mesh%nodes%nn)
    call h5%get(pre//'/nodes/id', mesh%nodes%id)
    call h5%get(pre//'/nodes/coo', mesh%nodes%coo)

    !> get domain
    call h5%get(pre//'/domain/ne', mesh%domain%ne)
    call h5%get(pre//'/domain/type', mesh%domain%type)
    call h5%get(pre//'/domain/enn', mesh%domain%enn)
    call h5%get(pre//'/domain/id', mesh%domain%id)
    call h5%get(pre//'/domain/er', mesh%domain%er)
    ! call h5%get(pre//'/domain/ev', mesh%domain%ev)
    call h5%get(pre//'/domain/eni', mesh%domain%eni)

    !> get boundary
    call h5%get(pre//'/boundary/ne', mesh%boundary%ne)
    call h5%get(pre//'/boundary/type', mesh%boundary%type)
    call h5%get(pre//'/boundary/enn', mesh%boundary%enn)
    call h5%get(pre//'/boundary/id', mesh%boundary%id)
    call h5%get(pre//'/boundary/er', mesh%boundary%er)
    ! call h5%get(pre//'/boundary/ev', mesh%boundary%ev)
    call h5%get(pre//'/boundary/eni', mesh%boundary%eni)

    mesh%is_init = .true.

    call mesh%cal_vol()

  end subroutine load_mesh_from_hdf5
  !=============================================================================
  subroutine save_mesh_to_hdf5(mesh, h5, mesh_path)
    class(mesh_t), intent(in) :: mesh
    type(hdf5_file), intent(inout) :: h5
    character(*), intent(in), optional :: mesh_path
    character(:), allocatable :: pre

    pre = '/mesh'
    if(present(mesh_path)) pre = mesh_path

    !> save nodes
    call h5%add(pre//'/nodes/nd', mesh%nodes%nd)
    call h5%add(pre//'/nodes/nn', mesh%nodes%nn)
    call h5%add(pre//'/nodes/id', mesh%nodes%id)
    call h5%add(pre//'/nodes/coo', mesh%nodes%coo)

    !> save domain
    call h5%add(pre//'/domain/ne', mesh%domain%ne)
    call h5%add(pre//'/domain/type', mesh%domain%type)
    call h5%add(pre//'/domain/enn', mesh%domain%enn)
    call h5%add(pre//'/domain/id', mesh%domain%id)
    call h5%add(pre//'/domain/er', mesh%domain%er)
    call h5%add(pre//'/domain/ev', mesh%domain%ev)
    call h5%add(pre//'/domain/eni', mesh%domain%eni)

    !> save boundary
    call h5%add(pre//'/boundary/ne', mesh%boundary%ne)
    call h5%add(pre//'/boundary/type', mesh%boundary%type)
    call h5%add(pre//'/boundary/enn', mesh%boundary%enn)
    call h5%add(pre//'/boundary/id', mesh%boundary%id)
    call h5%add(pre//'/boundary/er', mesh%boundary%er)
    call h5%add(pre//'/boundary/ev', mesh%boundary%ev)
    call h5%add(pre//'/boundary/eni', mesh%boundary%eni)

  end subroutine save_mesh_to_hdf5
  !=============================================================================
  subroutine save_mesh_to_xh5(mesh, xh5)
    type(mesh_t), intent(in) :: mesh
    type(xh5for_t), intent(inout) :: xh5

    associate( nn => mesh%nodes%nn, nd => mesh%nodes%nd, &
      & ne => mesh%domain%ne, type => mesh%domain%type)
      associate(eind => reshape(mesh%domain%eni, shape=(/size(mesh%domain%eni)/)),&
            & xyz => reshape(mesh%nodes%coo(1:nd,:), shape=(/nd*nn/)),&
            & er => mesh%domain%er )

        !< Write XDMF/HDF5 file
      if(type == 2) then
        call xh5%SetGrid(NumberOfNodes=mesh%nodes%nn,&
                        &  NumberOfElements=mesh%domain%ne,&
                        &  TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, &
                        &  GeometryType=XDMF_GEOMETRY_TYPE_XY)

      else
        call xh5%SetGrid(NumberOfNodes=mesh%nodes%nn,&
                        &  NumberOfElements=mesh%domain%ne,&
                        &  TopologyType=XDMF_TOPOLOGY_TYPE_TETRAHEDRON, &
                        &  GeometryType=XDMF_GEOMETRY_TYPE_XYZ)
      endif


      call xh5%WriteTopology(Connectivities=eind-1)
      call xh5%WriteGeometry(XYZ=xyz)
      call xh5%WriteAttribute(Name='Material', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=er)

      end associate
    end associate

  end subroutine save_mesh_to_xh5

end module mesh_factory
