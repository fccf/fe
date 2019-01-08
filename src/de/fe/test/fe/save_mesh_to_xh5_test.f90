program save_mesh_to_xh5_test
  use mesh
  use mesh_factory
  use io
  implicit none

  type(mesh_t)   :: mesh_data
  type(xh5for_t) :: xh5
  type(hdf5_file):: h5
  integer :: i
  real, allocatable :: flux(:)

  call h5%initialize()
  call h5%open_file('fish.h5',status='old',action='read')

  call load_mesh_from_hdf5(mesh_data,h5)
  call xh5%Open(FilePrefix='fish', GridType=XDMF_GRID_TYPE_UNSTRUCTURED, Strategy=XDMF_STRATEGY_DATASET_PER_PROCESS, Action=XDMF_ACTION_WRITE)
  call save_mesh_to_xh5(mesh_data,xh5)

  !>
  allocate(flux(mesh_data%nodes%nn))
  do i = 1, mesh_data%nodes%nn
    flux(i) = real(i)
  enddo

  call xh5%WriteAttribute(Name='flux-p1-g1', Type=XDMF_ATTRIBUTE_TYPE_SCALAR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=flux)

  call xh5%Close()
  call xh5%Free()

  call h5%close_file()
  ! call h5%finalize()


end program save_mesh_to_xh5_test
