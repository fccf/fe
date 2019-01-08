program load_hdf5_mesh_test
  use io
  use mesh
  use mesh_factory
  use mesh_partition
  use fc
  implicit none

  type(mesh_t)   :: mesh_info
  type(pmesh_t), allocatable :: pmeshs(:)
  type(hdf5_file) :: h5
  character(*), parameter :: hdf5_filename = 'fish.h5'
  integer :: ip, np

  call h5%initialize()
  call h5%open_file(hdf5_filename, status='old', action='rw')
  call load_mesh_from_hdf5(mesh_info, h5)

  np = 4

  if(have_option("-np")) call get_option('-np',np)

  pmeshs = part_mesh(mesh_info, np = np, method = 0)
  do ip = 1, size(pmeshs)
    call save_pmesh_to_hdf5(pmeshs(ip), h5, mesh_path= '/partition/mesh_'//to_str(ip-1))
  enddo

  call h5%close_file()

  call h5%finalize()


end program load_hdf5_mesh_test
