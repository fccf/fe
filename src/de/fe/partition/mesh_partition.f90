module mesh_partition
  use mesh
  use metis_interface
  use hdf5_interface
  use search
  use psb_util_mod
  implicit none

  type, extends(mesh_t) :: pmesh_t
    integer, allocatable :: halo_points(:)
    integer, allocatable :: inner_points(:)
  end type pmesh_t

  ! integer, allocatable :: epart(:), npart(:), bpart(:)

contains
  !=============================================================================
  function part_mesh(mesh, np, method) result(pmeshs)
    type(mesh_t),intent(in) :: mesh
    integer, intent(in) :: np
    integer, intent(in), optional :: method

    type(pmesh_t) :: pmeshs(np)

    integer, allocatable :: epart(:), npart(:), bpart(:)
    integer :: method_
    integer :: options(0:39)
    integer :: info, objval, ncommon, i

    method_ = 0
    if(present(method)) then
      method_ = method
    endif

    !>
    associate( nd => mesh%nodes%nd, nn => mesh%nodes%nn, &
      & ne => mesh%domain%ne, nf => mesh%boundary%ne,&
      & enn => mesh%domain%enn, fenn =>mesh%boundary%enn, &
      & eni => mesh%domain%eni, feni => mesh%boundary%eni,&
      & er => mesh%domain%er, fer => mesh%boundary%er, &
      & ev => mesh%domain%ev, fev => mesh%boundary%ev)
      associate(eind => reshape(eni, shape=(/size(eni)/)), eptr => [(i*enn+1,i=0,ne)])

        allocate(epart(ne))
        allocate(npart(nn))
        allocate(bpart(nf))

        info = METIS_SetDefaultOptions(options)
        options(17) = 1

        if(enn == 4) then
          ncommon = 3
        elseif(enn == 3) then
          ncommon = 2
        endif

        if(method_ == 0) then
          info = METIS_PartMeshDual(ne,nn,eptr,eind,ncommon = ncommon,nparts=np,&
                & options=options, objval=objval,epart=epart,npart=npart)
        elseif(method_ == 1) then
          info = METIS_PartMeshNodal(ne,nn,eptr,eind,nparts=np, &
                & options=options, objval=objval,epart=epart,npart=npart)
        else
          error stop "Unsupported partition method!"
        endif

        print*, epart
        print*, npart
        print*, objval

        !> initialize
        block
          integer :: ip
          do ip = 1, np
            pmeshs(ip)%inner_points  = [integer :: ]
            pmeshs(ip)%halo_points = [integer :: ]
            pmeshs(ip)%nodes%id = [integer :: ]
            pmeshs(ip)%domain%id = [integer :: ]
            pmeshs(ip)%boundary%id = [integer :: ]
          enddo
        end block

        !> partition mesh%domain mesh%nodes
        block
          integer :: ip, in, ie, id, ne_p, nn_p
          integer, allocatable :: eind_p(:)
          !> make pmeshs(ip)%domain%id
          do ie = 1, ne
            ip = epart(ie)
            pmeshs(ip)%domain%id = [pmeshs(ip)%domain%id, ie]
          enddo

          do ip = 1, np
            ne_p = size(pmeshs(ip)%domain%id)
            pmeshs(ip)%domain%ne = ne_p
            pmeshs(ip)%domain%enn = enn
            pmeshs(ip)%domain%type = mesh%domain%type

            allocate(pmeshs(ip)%domain%eni(enn, ne_p))
            allocate(pmeshs(ip)%domain%er(ne_p))
            allocate(pmeshs(ip)%domain%ev(ne_p))

            do ie = 1, ne_p
              id = pmeshs(ip)%domain%id(ie)
              pmeshs(ip)%domain%eni(:, ie) = eni(:,id)
              pmeshs(ip)%domain%er(ie) = er(id)
              pmeshs(ip)%domain%ev(ie) = ev(id)
            enddo

            !> pmeshs(ip)%nodes
            allocate(eind_p(ne_p*enn))

            eind_p = reshape(pmeshs(ip)%domain%eni, [ne_p*enn])
            call psb_msort(eind_p)

            id = eind_p(1)
            pmeshs(ip)%nodes%id = [pmeshs(ip)%nodes%id, id]
            do in = 2, size(eind_p)
              if(eind_p(in) /= id) then
                id = eind_p(in)
                pmeshs(ip)%nodes%id = [pmeshs(ip)%nodes%id, id]
              endif
            enddo

            deallocate(eind_p)

            nn_p = size(pmeshs(ip)%nodes%id)
            pmeshs(ip)%nodes%nn = nn_p
            pmeshs(ip)%nodes%nd = nd
            allocate(pmeshs(ip)%nodes%coo(nd, nn_p))

            do in = 1, nn_p
              id = pmeshs(ip)%nodes%id(in)
              pmeshs(ip)%nodes%coo(1:nd, in) = mesh%nodes%coo(1:nd,id)
            enddo

            !> renumbering pmesh(ip)%domain
            do ie = 1, ne_p
              do in = 1, enn
                id = pmeshs(ip)%domain%eni(in, ie)
                pmeshs(ip)%domain%eni(in, ie) = binary_search(id, pmeshs(ip)%nodes%id)
              enddo
            enddo

          enddo

        end block

        !>part boundary
        block
          integer :: ie, in, in0, nf_p, ip, id, ip0, id0, ip1, index
          logical :: found = .FALSE.

          do ie = 1, nf
            if(fer(ie) == 0) cycle
            loop1:do in = 1, fenn
              id = feni(in, ie)
              ip = npart(id)
              do in0 = 1, fenn
                id0 = feni(in0, ie)
                ip0 = npart(id0)
                if(in0 == in) cycle
                if(ip0 == ip) cycle
                index = binary_search(id0, pmeshs(ip)%nodes%id)
                if(index == 0) exit
                if(in0 == fenn) exit loop1
              enddo
            enddo loop1

            bpart(ie) = ip
          enddo
          !!<< NOTE@ have bug

          !> make pmeshs(ip)%boundary%id
          do ie = 1, nf
            if(fer(ie) == 0) cycle
            ip = bpart(ie)
            pmeshs(ip)%boundary%id = [pmeshs(ip)%boundary%id, ie]
          enddo

          do ip = 1, np
            nf_p = size(pmeshs(ip)%boundary%id)
            pmeshs(ip)%boundary%ne = nf_p
            pmeshs(ip)%boundary%enn = fenn
            pmeshs(ip)%boundary%type = mesh%boundary%type

            allocate(pmeshs(ip)%boundary%eni(fenn, nf_p))
            allocate(pmeshs(ip)%boundary%er(nf_p))
            allocate(pmeshs(ip)%boundary%ev(nf_p))

            do ie = 1, nf_p
              id = pmeshs(ip)%boundary%id(ie)
              pmeshs(ip)%boundary%eni(:, ie) = feni(:,id)
              pmeshs(ip)%boundary%er(ie) = fer(id)
              pmeshs(ip)%boundary%ev(ie) = fev(id)
            end do

            do ie = 1, nf_p
              do in = 1, fenn
                id = pmeshs(ip)%boundary%eni(in, ie)
                pmeshs(ip)%boundary%eni(in, ie) = binary_search(id, pmeshs(ip)%nodes%id)
              enddo
            enddo
          enddo

        endblock

        !> halo_points, inner_points
        block
          integer :: in, ip, ii, jj
          !> make pmeshs(ip)%inner_points
          do in = 1, nn
            ip = npart(in)
            pmeshs(ip)%inner_points = [pmeshs(ip)%inner_points, in]
          enddo
          do ip = 1, np
            ii = 1
            jj = 1
            do
              if(pmeshs(ip)%nodes%id(jj) == pmeshs(ip)%inner_points(ii)) then
                ii = ii + 1
                if(ii >= size(pmeshs(ip)%inner_points)) ii = size(pmeshs(ip)%inner_points)
                jj = jj + 1
              else
                pmeshs(ip)%halo_points = [pmeshs(ip)%halo_points, pmeshs(ip)%nodes%id(jj)]
                jj = jj + 1
              endif
              if(jj == pmeshs(ip)%nodes%nn) exit
            enddo
          enddo

        end block

      end associate
    end associate

  end function part_mesh
  !=============================================================================
  subroutine save_pmesh_to_hdf5(mesh, h5, mesh_path)
    class(pmesh_t), intent(in) :: mesh
    type(hdf5_file), intent(inout) :: h5
    character(*), intent(in), optional :: mesh_path
    character(:), allocatable :: pre

    pre = '/mesh'
    if(present(mesh_path)) pre = mesh_path

    !> save halo
    call h5%add(pre//'/inner_points', mesh%inner_points)
    call h5%add(pre//'/halo_points', mesh%halo_points)

    !> save nodes
    call h5%add(pre//'/nodes/nd', mesh%nodes%nd)
    call h5%add(pre//'/nodes/nn',mesh%nodes%nn)
    call h5%add(pre//'/nodes/id',mesh%nodes%id)
    call h5%add(pre//'/nodes/coo', mesh%nodes%coo)

    !> save domain
    call h5%add(pre//'/domain/ne', mesh%domain%ne)
    call h5%add(pre//'/domain/type', mesh%domain%type)
    call h5%add(pre//'/domain/enn', mesh%domain%enn)
    call h5%add(pre//'/domain/id', mesh%domain%id)
    call h5%add(pre//'/domain/er', mesh%domain%er)
    call h5%add(pre//'/domain/ev', mesh%domain%ev)
    call h5%add(pre//'/domain/eni', mesh%domain%eni)

    !> save boundary
    call h5%add(pre//'/boundary/ne', mesh%boundary%ne)
    call h5%add(pre//'/boundary/type', mesh%boundary%type)
    call h5%add(pre//'/boundary/enn', mesh%boundary%enn)
    call h5%add(pre//'/boundary/id', mesh%boundary%id)
    call h5%add(pre//'/boundary/er', mesh%boundary%er)
    call h5%add(pre//'/boundary/ev', mesh%boundary%ev)
    call h5%add(pre//'/boundary/eni', mesh%boundary%eni)

  end subroutine save_pmesh_to_hdf5

end module mesh_partition
