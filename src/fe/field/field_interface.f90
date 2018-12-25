module field_interface

  implicit none

  type, abstract :: field_t
    integer :: n_nodes = 0
    integer :: n_elems = 0

    integer, allocatable :: eind(:) !< element node index
    integer, allocatable :: eptr(:) !<
    integer, allocatable :: region_idx(:)

  end type field_t

contains

end module field_interface
