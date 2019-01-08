module region_factory
  use region
  use io
  implicit none

  public :: load_region_from_hdf5
  public :: save_region_to_hdf5

  private

contains
  !=============================================================================
  subroutine load_region_from_hdf5(regn, h5, region_path)
    type(region_t), intent(inout) :: regn
    type(hdf5_file), intent(in) :: h5
    character(*), intent(in), optional :: region_path

    character(:), allocatable :: pre
    pre = '/region'
    if(present(region_path)) pre = region_path

    !> get region index
    call h5%get(pre//'/nr', regn%nr)
    call h5%get(pre//'/m_idx', regn%m_idx)
    call h5%get(pre//'/s_idx', regn%s_idx)

  end subroutine load_region_from_hdf5
  !=============================================================================
  subroutine save_region_to_hdf5(regn, h5, region_path)
    type(region_t), intent(inout) :: regn
    type(hdf5_file), intent(in) :: h5
    character(*), intent(in), optional :: region_path

    character(:), allocatable :: pre
    pre = '/region'
    if(present(region_path)) pre = region_path

    !> save region index
    call h5%add(pre//'/nr', regn%nr)
    call h5%add(pre//'/m_idx', regn%m_idx)
    call h5%add(pre//'/s_idx', regn%s_idx)

  end subroutine save_region_to_hdf5

end module region_factory
