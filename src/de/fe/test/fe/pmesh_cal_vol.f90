program pmesh_cal_vol
  use io
  use la
  use fe
  use fc
  implicit none

  integer :: ictxt, ip, np
  type(hdf5_file) :: h5f
  type(mesh_t) :: mesh
  character(:), allocatable ::  h5fname
  real, allocatable :: eni(:,:)
  integer :: i,j

  h5fname = 'fish.h5'

  call psb_init(ictxt)
  call psb_info(ictxt, ip, np)

  ! if(ip == psb_root_) then
    call h5f%initialize()
    call h5f%open_file(h5fname,status='old',action='rw')
  ! endif

  ! if(ip /= 2) then
    call load_mesh_from_hdf5(mesh, h5f, mesh_path= '/restart/mesh_'//to_str(ip+1))

      ! call save_mesh_to_hdf5(mesh, h5f, mesh_path= '/restart2/mesh_'//to_str(ip))
    print*, ip, mesh%boundary%ne
  ! endif

  ! if(ip == psb_root_) then
    call h5f%close_file()
    call h5f%finalize()
  ! endif

  call psb_exit(ictxt)

end program pmesh_cal_vol
