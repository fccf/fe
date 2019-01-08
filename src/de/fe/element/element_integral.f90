module element_integral
  implicit none

contains
  !=============================================================================
  pure function shape_rhs(shape, detwei) result(rhs)
    !!<             /
    !!<   Calculate | shape detwei dV
    !!<             /
    real, intent(in) :: shape(:,:) !< (enn,nq)
    real, intent(in) :: detwei(size(shape,2)) !< transformed weights
    real :: rhs(size(shape,1))

    rhs = matmul(shape, detwei)

  end function shape_rhs
  !=============================================================================
  pure function dshape_rhs(dshape, detwei) result(rhs)
    !!<            /
    !!<  Calculate | dshape detwei dV
    !!<            /
    real, intent(in) :: dshape(:,:,:) !< (nd, enn, nq) transformed dshape
    real, intent(in) :: detwei(size(dshape,2)) !< transformed weights
    real :: rhs(size(dshape,1),size(dshape,2)) !< (nd, enn)

    integer :: nd,i

    nd = size(dshape,1)

    forall(i=1:nd)
       rhs(i,:)=matmul(dshape(i,:,:),detwei)
    end forall

  end function dshape_rhs
  !=============================================================================
  pure function shape_shape(shape1, shape2, detwei)
    !!<             /
    !!<   Calculate | shape1 shape2 detwei dV
    !!<             /
    real, intent(in) :: shape1(:,:) !< (enn,nq)
    real, intent(in) :: shape2(:,:) !< (enn,nq)
    real, intent(in) :: detwei(size(shape1,2)) !< transformed weights
    real :: shape_shape(size(shape1,1),size(shape2,1))

    integer :: i, j

    forall(i=1:size(shape1,1),j=1:size(shape2,1))
       ! Main mass matrix.
       shape_shape(i,j)=&
            dot_product(shape1(i,:)*shape2(j,:),detwei)
    end forall

  end function shape_shape
  !=============================================================================
  pure function shape_dshape(shape, dshape, detwei)
    !!<             /
    !!<   Calculate | shape dshape detwei dV
    !!<             /
    real, intent(in) :: shape(:,:) !< (enn,nq)
    real, intent(in) :: dshape(:,:,:) !< (nd, enn, nq) transformed dshape
    real, intent(in) :: detwei(size(shape,2)) !< transformed weights
    real :: shape_dshape(size(dshape,1),size(shape,1),size(dshape,2))

    integer :: iloc,jloc, idim

    forall(iloc=1:size(shape,1),jloc=1:size(dshape,2),idim=1:size(dshape,1))
       shape_dshape(idim,iloc,jloc)= sum(detwei * shape(iloc,:) * dshape(idim, jloc,:))
    end forall

  end function shape_dshape
  !=============================================================================
  function dshape_shape(dshape, shape, detwei)
    !!<             /
    !!<   Calculate | dshape shape detwei dV
    !!<             /
    real, intent(in) :: dshape(:,:,:) !< (enn, nq, nd) transformed dshape
    real, intent(in) :: shape(:,:) !< (enn,nq)
    real, intent(in) :: detwei(size(shape,2)) !< transformed weights
    real :: dshape_shape(size(dshape,1),size(dshape,2),size(shape,1))

    integer :: iloc,jloc,idim

    forall(iloc=1:size(dshape,2),jloc=1:size(shape,1),idim=1:size(dshape,1))
       dshape_shape(idim,iloc,jloc)= sum(detwei * dshape(idim,iloc,:) * shape(jloc,:))
    end forall


  end function dshape_shape

end module element_integral
