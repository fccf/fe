program load_region_hdf5_test
  use io
  use region
  use region_factory
  implicit none

  type(region_t)   :: regn
  type(hdf5_file) :: h5
  character(*), parameter :: hdf5_filename = 'fish.h5'

  call h5%initialize()

  call h5%open_file(hdf5_filename, status='old', action='rw')

  call load_region_from_hdf5(regn, h5)

  call save_region_to_hdf5(regn,h5, region_path = '/restart/region')

  call h5%close_file()

  call h5%finalize()


end program load_region_hdf5_test
