module psb_metispart_mesh
  use metis_interface
  use psb_interface
  use mesh
  implicit none


contains
  !=============================================================================
  subroutine build_mesh_descriptor(desc, mesh)
    type(psb_desc_type), intent(out) :: desc


  end subroutine build_mesh_descriptor
  !=============================================================================
  subroutine mesh_partition(mesh, np, npart, epart)
    type(mesh_t),intent(in) :: mesh
    integer, intent(in)     :: np
    integer, allocatable, intent(out) :: npart(:)
    integer, allocatable, intent(out) :: epart(:)

    integer :: options(0:39)
    integer :: info, objval, ncommon

    !>
    associate(nn => mesh%nodes%nn, ne => mesh%domain%ne,enn => mesh%domain%enn&
      & eni => mesh%domain%eni)
      associate(eind => reshape(eni, shape=(/size(eni)/)), eptr => [(i*enn+1,i=0,ne)])

        allocate(epart(ne))
        allocate(npart(nn))

        info = METIS_SetDefaultOptions(options)
        options(17) = 1

        if(enn == 4) then
          ncommon = 3
        elseif(enn == 3) then
          ncommon = 2
        endif

        info = METIS_PartMeshDual(ne,nn,eptr,eind,ncommon = ncommon,nparts=np,&
              & options=options, objval=objval,epart=epart,npart=npart)

    end associate; end associate

  end subroutine mesh_partition
  !=============================================================================

end module psb_metispart_mesh
