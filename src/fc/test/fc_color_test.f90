program fc_clolor_test
  use fc_color
  use fc_string
  integer :: i, j, k
  integer, allocatable :: rgb(:)

  do i = 0,5
    do j = 0,5
      do k = 0,5
        rgb = [i,j,k]
        print*, colorize('color', rgb), to_str(rgb)
      enddo
    enddo
  enddo

  print*, colorize('black','black')
  print*, colorize('white','white')
  print*, colorize('red','red')
  print*, colorize('green','green')
  print*, colorize('blue','blue')
  print*, colorize('yello','yello')
  print*, colorize('cyan','cyan')
  print*, colorize('purple','purple')

  do i = 0,20
    print*, colorize('corlormap',color_map(v=0.05*i,r=[0.,1.]))
  enddo


end program fc_clolor_test
