program load_gmsh_test
  use io
  use mesh
  use mesh_factory
  implicit none

  type(mesh_t)   :: mesh_info
  type(msh_file) :: gmsh
  type(hdf5_file) :: h5
  character(*), parameter :: gmsh_filename = '../data/gmsh.msh'
  character(*), parameter :: hdf5_filename = 'fish.h5'

  call h5%initialize()

  call gmsh%init(gmsh_filename)


  call load_mesh_from_gmsh(mesh_info,gmsh)

  call h5%open_file(hdf5_filename, status='new', action='w')

  call save_mesh_to_hdf5(mesh_info,h5)

  call h5%close_file()

  call h5%finalize()


end program load_gmsh_test
