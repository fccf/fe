program test_element
  use element
  implicit none

  class(ele_t), pointer :: ele
  real :: coo(3,2) = reshape([0.,1.,2.,0.,0.,2.], shape=[3,2])
  real, allocatable :: detwei(:), rhs(:), dshape(:,:,:), nn(:,:), dnn(:,:,:)
  real :: vol

  call choose_element(ele, type='tri', degree = 1)

  allocate(detwei(ele%nq), dshape(ele%nd, ele%nn, ele%nq))

  call ele%to_physical(coo, detwei, dshape)
  !> cal volume
  vol = sum(detwei)
  print*, 'vol = ', vol

  !>
  rhs = shape_rhs(ele%n, detwei)
  print*, 'shape_rhs = ', rhs

  !>
  nn =shape_shape(ele%n, ele%n, detwei)
  print*, 'nn = ', nn

  !>
  dnn = dshape_shape(dshape, ele%n, detwei)
  print*, 'dnn = ', dnn


end program test_element
