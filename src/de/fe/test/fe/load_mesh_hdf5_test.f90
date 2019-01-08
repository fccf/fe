program load_hdf5_mesh_test
  use io
  use mesh
  use mesh_factory
  implicit none

  type(mesh_t)   :: mesh_info
  type(hdf5_file) :: h5
  character(*), parameter :: hdf5_filename = 'fish.h5'

  call h5%initialize()

  call h5%open_file(hdf5_filename, status='old', action='rw')

  call load_mesh_from_hdf5(mesh_info, h5)

  call save_mesh_to_hdf5(mesh_info,h5, mesh_path = '/restart/mesh')

  call h5%close_file()

  call h5%finalize()


end program load_hdf5_mesh_test
