program test_element
  use element
  implicit none

  class(ele_t), pointer :: ele
  real :: coo(3,2) = reshape([0.,1.,1.,0.,0.,1.], shape=[3,2])
  real, allocatable :: detwei(:), rhs(:), drhs(:,:), &
                    & shape(:,:), dshape(:,:,:), nn(:,:), dnn(:,:,:)
  real :: vol
  integer :: i,j

  call choose_element(ele, type='tri', degree = 1)

  call ele%to_physical(coo)
  !> cal volume
  detwei = ele%get_detwei()
  vol = sum(detwei)
  write(*,'(1x,a,es21.11)') 'vol = ', vol

  !> rhs
  shape = ele%get_shape()
  rhs = shape_rhs(shape, detwei)
  write(*,'(1x,a,3es21.11)') 'shape_rhs = ', rhs

  !>
  nn =shape_shape(shape, shape, detwei)

  write(*,*) 'nn = '
  write(*,'(3es21.11)') ((nn(i,j),i =1,size(nn,1)),j=1,size(nn,2))


  !>
  dshape = ele%get_dshape()
  !> drhs
  drhs = dshape_rhs(dshape,detwei)

  write(*,'(1x,a,3es21.11)') 'dx_rhs = ', drhs(1,:)
  write(*,'(1x,a,3es21.11)') 'dy_rhs = ', drhs(2,:)

  dnn = dshape_shape(dshape, shape, detwei)
  write(*,*) 'dxn = '
  write(*,'(3es21.11)') ((dnn(1,i,j),i =1,size(dnn,2)),j=1,size(dnn,3))
  write(*,*) 'dyn = '
  write(*,'(3es21.11)') ((dnn(2,i,j),i =1,size(dnn,2)),j=1,size(dnn,3))


end program test_element
