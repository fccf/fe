module calculate_volume

  implicit none

  public :: cal_tet_vol
  public :: cal_tri_area
  public :: cal_line_len
  public :: cal_nor
  public :: cal_boundary_nor
  public :: global2local

  private

contains
  !=============================================================================
  function cal_tet_vol(coord) result(vol)

    real, intent(in) :: coord(3,4)
    real :: vol

    real :: x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4
    real :: a(3),b(3),c(3)

    x1=coord(1,1);x2=coord(1,2);x3=coord(1,3);x4=coord(1,4)
    y1=coord(2,1);y2=coord(2,2);y3=coord(2,3);y4=coord(2,4)
    z1=coord(3,1);z2=coord(3,2);z3=coord(3,3);z4=coord(3,4)

    a(1)=x2-x1;a(2)=y2-y1;a(3)=z2-z1
    b(1)=x3-x1;b(2)=y3-y1;b(3)=z3-z1
    c(1)=x4-x1;c(2)=y4-y1;c(3)=z4-z1

    vol=(a(1)*b(2)*c(3)-a(1)*b(3)*c(2)-a(2)*b(1)*c(3)+a(2)*b(3)*c(1)+&
         a(3)*b(1)*c(2)-a(3)*b(2)*c(1))/6.0

  end function cal_tet_vol
  !=============================================================================
  function cal_tri_area(coord) result(area)

    real,intent(in)  :: coord(3,3)
    real :: area

    real :: nor(3)

    call calculate_normal(nor,coord)

    area = 0.5*sqrt(nor(1)*nor(1)+nor(2)*nor(2)+nor(3)*nor(3))

  end function cal_tri_area
  !=============================================================================
  function cal_nor(coord) result(nor)

    real,intent(in)  :: coord(3,3)
    real :: nor(3)

    real :: coefficients

    call calculate_normal(nor,coord)
    coefficients = sqrt(nor(1)*nor(1)+nor(2)*nor(2)+nor(3)*nor(3))
    nor(:) = nor(:)/coefficients

  end function cal_nor
  !=============================================================================
  subroutine cal_boundary_nor(nor,coord)
    real,allocatable, intent(out) :: nor(:)
    real, intent(in) :: coord(:,:)

    real :: a(3),b(3)
    integer :: nd, enn

    nd = size(coord,1)
    enn = size(coord,2)

    allocate(nor(nd))
    nor = 0.0

    if(nd == 2) then
      associate(x1 => coord(1,1), y1 => coord(2,1),&
        x2 => coord(1,2),y2 => coord(2,2))
        nor(1) = y2-y1
        nor(2) = x1-x2
      end associate
      nor = nor/sqrt(nor(1)*nor(1)+nor(2)*nor(2))
    elseif(nd == 3) then
      associate( x1 => coord(1,1),y1 => coord(2,1), z1 => coord(3,1),&
           x2 => coord(1,2),y2 => coord(2,2), z2 => coord(3,2),&
           x3 => coord(1,3),y3 => coord(2,3), z3 => coord(3,3))
        a(1)=x2-x1;a(2)=y2-y1;a(3)=z2-z1
        b(1)=x3-x1;b(2)=y3-y1;b(3)=z3-z1
        nor(1)= a(2)*b(3) - a(3)*b(2)
        nor(2)= a(3)*b(1) - a(1)*b(3)
        nor(3)= a(1)*b(2) - a(2)*b(1)
      end associate
      nor = nor/sqrt(nor(1)*nor(1)+nor(2)*nor(2)+nor(3)*nor(3))
    endif
  end subroutine cal_boundary_nor
  !=============================================================================
  function cal_line_len(coord) result(len)

    real,intent(in)  :: coord(3,2)
    real :: len

    real :: x1,x2,y1,y2,z1,z2
    real :: a(3)

    x1 = coord(1,1);x2=coord(1,2)
    y1 = coord(2,1);y2=coord(2,2)
    z1 = coord(3,1);z2=coord(3,2)

    a(1)=x2-x1;a(2)=y2-y1;a(3)=z2-z1
    len = sqrt(a(1)*a(1)+a(2)*a(2)+a(3)*a(3))

  end function cal_line_len
  !=============================================================================
  subroutine calculate_normal(nor,coord)

    real,intent(out) :: nor(3)
    real,intent(in)  :: coord(3,3)

    real :: x1,x2,x3,y1,y2,y3,z1,z2,z3
    real :: a(3),b(3)

    x1=coord(1,1);x2=coord(1,2);x3=coord(1,3)
    y1=coord(2,1);y2=coord(2,2);y3=coord(2,3)
    z1=coord(3,1);z2=coord(3,2);z3=coord(3,3)

    a(1)=x2-x1;a(2)=y2-y1;a(3)=z2-z1
    b(1)=x3-x1;b(2)=y3-y1;b(3)=z3-z1

    nor(1)= a(2)*b(3) - a(3)*b(2)
    nor(2)= a(3)*b(1) - a(1)*b(3)
    nor(3)= a(1)*b(2) - a(2)*b(1)

  end subroutine calculate_normal
  !=============================================================================
  subroutine global2local(is_inside,local,global,coord)

    logical,intent(out) :: is_inside
    real,allocatable,intent(out)  :: local(:)
    real,intent(in)  :: global(:)
    real,intent(in)  :: coord(:,:)

    real,allocatable :: coord_temp(:,:),vol(:)
    integer :: nd,enn,i
    real :: volumn

    real,parameter :: point_tol = 1.0e-3,eps_zero = 1.0e-8

    nd = size(global,1)
    enn = size(coord,2)
    if(allocated(local)) deallocate(local)
    allocate(local(enn))

    allocate(vol(enn))
    allocate(coord_temp,source = coord)

    if(enn == 3) then
       volumn = cal_tri_area(coord)
       do i = 1,enn
          coord_temp      = coord
          coord_temp(:,i) = global
          vol(i) = cal_tri_area(coord_temp)
          local(i) = vol(i)/volumn
       end do

    elseif(enn == 4) then
       volumn = cal_tet_vol(coord)
       do i = 1,enn
          coord_temp      = coord
          coord_temp(:,i) = global
          vol(i) = cal_tet_vol(coord_temp)
          local(i) = vol(i)/volumn
       end do

    end if

    is_inside = .true.

    if(abs(sum(local) - 1.0) > eps_zero ) then
       is_inside = .false.
    endif


    do i = 1,enn
       if( local(i) < 0.0 - point_tol) then
          is_inside = .false.
          local(i) = 0.0
       elseif(local(i) > 1.0 + point_tol) then
          is_inside = .false.
          local(i) = 1.0
       end if

    end do

  end subroutine global2local


end module calculate_volume
