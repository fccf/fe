program mesh_partition_test
  use mesh_partition
  use string
  use io
  use fe
  use fc
  implicit none

  type(msh_file) :: gmsh
  type(mesh_t)   :: mesh_data
  type(pmesh_t), allocatable :: pmeshs(:)
  integer :: ip, np
  type(hdf5_file) :: h5f

  character(:), allocatable :: fname, h5fname

  fname = '../data/gmsh.msh'
  h5fname = 'fish.h5'
  np = 4

  if(have_option("-np")) call get_option('-np',np)

  call gmsh%init(fname)

  call load_mesh_from_gmsh(mesh_data, gmsh)

  pmeshs = part_mesh(mesh_data, np = np, method = 0)

  call h5f%initialize()
  call h5f%open_file(h5fname,status='new',action='rw')

  call save_mesh_to_hdf5(mesh_data, h5f, mesh_path= '/mesh')
  do ip = 1, size(pmeshs)
    call save_pmesh_to_hdf5(pmeshs(ip), h5f, mesh_path= '/partition/mesh_'//to_str(ip))
  enddo


  call h5f%close_file()
  call h5f%finalize()


end program mesh_partition_test
