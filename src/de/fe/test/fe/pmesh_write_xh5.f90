program pmesh_write_xh5
  use io
  use mesh_partition
  use fe
  use fc
  use la
  implicit none

  type(mesh_t)    :: mesh
  type(xh5for_t)  :: xh5
  type(hdf5_file) :: h5f

  character(:), allocatable :: h5fname
  integer :: ictxt, ip, np, info, i

  h5fname = 'fish.h5'


  call psb_init(ictxt)
  call psb_info(ictxt, ip, np)


  call h5f%initialize()
  call h5f%open_file(h5fname,status='old',action='rw')
  call load_mesh_from_hdf5(mesh, h5f, mesh_path= '/partition/mesh_'//to_str(ip+1))

  call xh5%Open(FilePrefix='fish_2d', GridType=XDMF_GRID_TYPE_UNSTRUCTURED, &
    & Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS, Action=XDMF_ACTION_WRITE)

  associate(nn=> mesh%nodes%nn, ne => mesh%domain%ne, &
    & eind => reshape(mesh%domain%eni, shape=(/size(mesh%domain%eni)/)), &
    & xyz  => reshape(mesh%nodes%coo,shape=(/size(mesh%nodes%coo)/)), &
    & er   => mesh%domain%er, &
    & ep   => [(ip,i=1,mesh%domain%ne)])

  call xh5%SetGrid(NumberOfNodes=nn,&
                    &  NumberOfElements=ne,&
                    &  TopologyType=XDMF_TOPOLOGY_TYPE_TRIANGLE, &
                    &  GeometryType=XDMF_GEOMETRY_TYPE_XY)

  call xh5%WriteTopology(Connectivities=eind-1)
  call xh5%WriteGeometry(XYZ=xyz)
  call xh5%WriteAttribute(Name='Material', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,&
          & Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=er)

  call xh5%WriteAttribute(Name='processor', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,&
          & Center=XDMF_ATTRIBUTE_CENTER_CELL , Values=ep)
  end associate

  call xh5%Close()
  call xh5%Free()

  ! call h5f%close_file()
  ! call h5f%finalize()
  call psb_exit(ictxt)

end program pmesh_write_xh5
