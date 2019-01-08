module region

  implicit none

  public :: region_t
  
  private

  type region_t
    !> region material index
    integer :: nr !< number of region
    integer, allocatable :: m_idx(:) !< material index in each reigon (nr)
    integer, allocatable :: s_idx(:) !< source index in each region (nr)

  contains
    procedure :: byte => region_byte
    procedure :: destroy => region_destory
    final     :: region_final
  end type region_t

contains
  !=============================================================================
  pure function region_byte(this) result(res)
    class(region_t), intent(in) :: this
    integer(8) :: res
    res = size(this%m_idx) * kind(0)*2 +1
  end function region_byte
  !=============================================================================
  subroutine region_destory(this)
    class(region_t), intent(inout) :: this
    this%nr = 0
    if(allocated(this%m_idx)) deallocate(this%m_idx)
    if(allocated(this%s_idx)) deallocate(this%s_idx)
  end subroutine region_destory
  !=============================================================================
  subroutine region_final(this)
    type(region_t), intent(inout) :: this
    call this%destroy()
  end subroutine region_final

end module region
