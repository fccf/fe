# finite element library

- [ ] ele_t
  - [x] line
  - [x] triangle
  - [x] quadrilateral
  - [x] tetrahedron
  - [x] hexahedron
  - [ ] prism

# example
``` fortran
program test
  use element
  implicit none

  class(ele_t), pointer :: ele
  real :: coo(3,2) = reshape([0.,1.,1.,0.,0.,1.], shape=[3,2])
  real, allocatable :: detwei(:), rhs(:), drhs(:,:), &
                     & shape(:,:), dshape(:,:,:), nn(:,:), dnn(:,:,:)

  call choose_element(ele, type='tri', degree = 1)

  call ele%to_physical(coo)

  detwei = ele%get_detwei()
  shape  = ele%get_shape()
  rhs    = shape_rhs(shape, detwei)
  nn     = shape_shape(shape, shape, detwei)
  dshape = ele%get_dshape()
  drhs   = dshape_rhs(dshape,detwei)
  dnn    = dshape_shape(dshape, shape, detwei)

end program test

```
