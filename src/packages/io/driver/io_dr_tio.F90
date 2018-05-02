! @HEADER@
! Crown Copyright 2018 AWE.
!
! This file is part of BookLeaf.
!
! BookLeaf is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
! 
! BookLeaf is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE. See the GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License along with
! BookLeaf. If not, see http://www.gnu.org/licenses/.
! @HEADER@
MODULE TyphonIO_mod

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: write_tio_dump

CONTAINS

  INTEGER FUNCTION testErr(iErr,smessage)  

    USE kinds_mod,     ONLY: ink,rlk
    USE error_mod,     ONLY: halt
    USE timing_mod,    ONLY: bookleaf_times
    USE typh_util_mod, ONLY: get_time,set_info,set_comm_self
    USE typhonio

    INTEGER(kind=TIO_ERRK), INTENT(IN) :: iErr
    CHARACTER(LEN=*),INTENT(IN) :: smessage
    CHARACTER(LEN=40) :: string
    IF (iErr /= TIO_SUCCESS_F) THEN
      string="Error: "//smessage
      print*,"Error is:",iErr
      CALL halt(string,iout=0,zend=.false.)
    ENDIF
    testErr = 0
    RETURN
  END FUNCTION testErr

  SUBROUTINE write_tio_dump(fname,zerr)

    USE kinds_mod,     ONLY: ink,rlk
    USE typhonio
    USE integers_mod,    ONLY: nel,nnod,nmat,NProcW,rankW,CommS
    USE reals_mod,       ONLY: time
    USE logicals_mod,    ONLY: zMProcW,zparallel
    USE pointers_mod,    ONLY: ielnd,ndx,ndy,ielmat,ein,pre,rho,csqrd,   &
&                              ndu,ndv,indtype,ielreg,iellocglob,        &
&                              indlocglob
    USE utilities_mod,   ONLY: arth
    USE typh_util_mod, ONLY: get_time,set_info,set_comm_self
    USE typh_collect_mod,ONLY: TYPH_Gather,TYPH_Reduce,TYPH_OP_SUM
    USE timing_mod,   ONLY: bookleaf_times

    CHARACTER(LEN=*), INTENT(IN) :: fname
    LOGICAL,INTENT(IN),OPTIONAL  :: zerr
    INTEGER(kind=TIO_FILEK)      :: file_id
    INTEGER(kind=TIO_OBJK)       :: state_id, obj_id, mat_id, qnt_id
    INTEGER(kind=TIO_IPK)        :: xfer = TIO_XFER_INDEPENDENT_F
    INTEGER(kind=TIO_SIZEK)      :: idx
    INTEGER(kind=ink)            :: iErr,ii,jj,kk
    INTEGER(KIND=ink)            :: nelg,nnodg
    INTEGER(kind=ink), ALLOCATABLE, DIMENSION(:)   :: connectivity
    INTEGER(kind=ink),              DIMENSION(2)   :: var
    INTEGER(kind=ink), DIMENSION(2,0:NprocW-1)     :: var_t
    INTEGER(kind=TIO_SHAPEK),       DIMENSION(1)   :: shapes = (/TIO_SHAPE_QUAD4_F/)
    INTEGER(kind=ink),              DIMENSION(1)   :: nc_per_shape
    INTEGER(KIND=ink)              :: oldgroup,newgroup,pinfo
    INTEGER(KIND=ink),DIMENSION(1) :: ranks
    REAL(KIND=rlk)                               :: t0,t1

    t0 = get_time()

!   Single proc abort
    IF (PRESENT(zerr)) THEN
      IF (zerr) THEN
        iErr=set_comm_self(CommS)
        rankW=0
        NprocW=1
        nelg=nel
        nnodg=nnod
      ELSE
        IF (zparallel) THEN
!         calculate global number of elements and nodes
          iErr=TYPH_Reduce(nel,nelg,TYPH_OP_SUM,CommS)
          iErr=TYPH_Reduce(nnod,nnodg,TYPH_OP_SUM,CommS)
        ELSE
!         serial
          nelg=nel
          nnodg=nnod
        ENDIF
      ENDIF
    ELSE
      IF (zparallel) THEN
!       calculate global number of elements and nodes
        iErr=TYPH_Reduce(nel,nelg,TYPH_OP_SUM,CommS)
        iErr=TYPH_Reduce(nnod,nnodg,TYPH_OP_SUM,CommS)
      ELSE
!       serial
        nelg=nel
        nnodg=nnod
      ENDIF
    ENDIF

    idx = rankW+1
    iErr = set_info(pinfo)

#ifdef NOMPI
    iErr = testErr(TIO_Create_F(filename=fname,fileID=file_id,   &
&                  access=TIO_ACC_REPLACE_F,codename="Bookleaf", &
&                  version="1.0",date="-",title="End dump"),     &
&                  "TIO_Create_F")
#else
    iErr = testErr(TIO_Create_F(filename=fname,fileID=file_id,   &
&                  access=TIO_ACC_REPLACE_F,codename="Bookleaf", &
&                  version="1.0",date="-",title="End dump",      &
&                  comm=CommS,info=pinfo,rank=rankW),            &
&                  "TIO_Create_F")
#endif
    iErr = testErr(TIO_Create_State_F(fileID=file_id,name="End State", &
&                  stateID=state_id,step=1,time=time,units='-'),       &
&                  "TIO_Create_State_F")
    iErr = testErr(TIO_Create_Mesh_f(fileID=file_id,stateID=state_id,      &
&                  name='Mesh',meshID=obj_id,meshtype=TIO_MESH_UNSTRUCT_F, &
&                  coordtype=TIO_COORD_CARTESIAN_F,isAMR=.FALSE.,          &
&                  group='Whole',order=1_ink,graph_datatype=TIO_INTEGER4_F,&
&                  coord_datatype=TIO_REAL8_F,ndims=TIO_2D_F,n1=nnodg,     &
&                  n2=nelg,n3=NprocW,n4=4_ink*nelg,nchunks=NProcW),        &
&                  "TIO_Create_Mesh_f")

    var(1) = nnod
    var(2) = nel
    IF (zparallel) THEN
      ierr=TYPH_Gather(var,var_t,CommS)
    ELSE
      var_t(1:2,0) = var
    ENDIF
    DO ii=0,NprocW-1
      iErr = testErr(TIO_Set_Unstr_Chunk_f(fileID=file_id,meshID=obj_id,&
&                    idx=ii+1,ndims=TIO_2D_F,nnodes=var_t(1,ii),        &
&                    ncells=var_t(2,ii),nshapes=1_TIO_SIZEK,            &
&                    nconnectivity=4*var_t(2,ii),nghost_nodes=0_ink,    &
&                    nghost_cells=0_ink,nghost_shapes=0_ink,            &
&                    nghost_connectivity=0_ink,nmixcell=0_TIO_SIZEK,    &
&                    nmixcomp=0_TIO_SIZEK), "TIO_Set_Unstr_Chunk_f")
    ENDDO

    nc_per_shape = (/ nel /)

    ALLOCATE(connectivity(4*nel))
    kk=1_ink
    DO ii=1,nel
      DO jj=1,4
        connectivity(kk)=ielnd(jj,ii)
        kk=kk+1_ink
      ENDDO
    ENDDO

    iErr = testErr(TIO_Write_UnstrMesh_Chunk_f(fileID=file_id,meshID=obj_id,&
&                  idx=idx,xfer=xfer,graph_datatype=TIO_INTEGER4_F,         &
&                  coord_datatype=TIO_REAL8_F,nodeIDs=indlocglob(1:nnod),   &
&                  cellIDs=iellocglob(1:nel),shapes=shapes,                 &
&                  ncells_per_shape=nc_per_shape,connectivity=connectivity, &
&                  icoords=ndx(1:nnod),jcoords=ndy(1:nnod)),                &
&                  "TIO_Write_UnstrMesh_Chunk_f")

    DEALLOCATE(connectivity)

    iErr = testErr(TIO_Create_Mat_f(file_id,obj_id,'Material',mat_id, &
&                  TIO_INTEGER4_F,nmat,TIO_GHOSTS_NONE_F,.false.),    &
&                  "TIO_Create_Mat_f")

!   Assumes no mix
    iErr = testErr(TIO_Write_UnstrMat_Chunk_f(file_id,mat_id,idx,xfer,  &
&                  TIO_INTEGER4_F,ielmat(1:nel)),"TIO_Write_UnstrMat_Chunk_f")
    iErr = testErr(TIO_Close_Mat_f(fileID=file_id, materialID=mat_id ),"TIO_Close_Mat_f")

! Quants: energy, pressure, density, cdqrd, velocity
! energy
    iErr = testErr(TIO_Create_Quant_F(fileID=file_id,meshID=obj_id,     &
&                  name="Energy",quantID=qnt_id,datatype=TIO_REAL8_F,   &
&                  centring=TIO_CENTRE_CELL_F,nghosts=TIO_GHOSTS_NONE_F,&
&                  ismixed=.false.,units="-"),                          &
&                  "TIO_Create_Quant_F: ein")
    iErr = testErr(TIO_Write_UnstrQuant_Chunk_f(fileID=file_id, &
&                  quantID=qnt_id,idx=idx,xfer=xfer,            & 
&                  datatype=TIO_REAL8_F,qdat=ein(1:nel)),       &
&                  "TIO_Write_UnstrQuant_Chunk_f: ein")
    iErr = testErr(TIO_Close_Quant_F(fileID  = file_id, quantID = qnt_id  ), &
                  "TIO_Close_Quant_F: ein")
!   pressure
    iErr = testErr(TIO_Create_Quant_F(fileID=file_id,meshID=obj_id,     &
&                  name= "Pressure",quantID=qnt_id,                     &
&                  datatype=TIO_REAL8_F,centring=TIO_CENTRE_CELL_F,     &
&                  nghosts=TIO_GHOSTS_NONE_F,ismixed=.false.,units="-"),&
&                  "TIO_Create_Quant_F: pre")
    iErr = testErr(TIO_Write_UnstrQuant_Chunk_f(fileID=file_id,quantID=qnt_id,&
&                  idx=idx,xfer=xfer,datatype=TIO_REAL8_F,qdat=pre(1:nel)),   &
&                  "TIO_Write_UnstrQuant_Chunk_f: pre")
    iErr = testErr(TIO_Close_Quant_F(fileID  = file_id, quantID = qnt_id  ), &
&                 "TIO_Close_Quant_F: pre")

!   density
    iErr = testErr(TIO_Create_Quant_F(fileID=file_id,meshID=obj_id,     &
&                  name="Density",quantID=qnt_id, datatype=TIO_REAL8_F, &
&                  centring=TIO_CENTRE_CELL_F,nghosts=TIO_GHOSTS_NONE_F,&
&                  ismixed=.false.,units="-"),                          &
&                  "TIO_Create_Quant_F: rho")
    iErr = testErr(TIO_Write_UnstrQuant_Chunk_f(fileID=file_id,        &
&                  quantID=qnt_id,idx=idx, xfer=xfer,                  &
&                  datatype=TIO_REAL8_F,qdat=rho(1:nel)),              &
&                  "TIO_Write_UnstrQuant_Chunk_f: rho")
    iErr = testErr(TIO_Close_Quant_F(fileID=file_id,quantID=qnt_id),   &
&                  "TIO_Close_Quant_F: rho")

  ! sound speed sqrd
    iErr = testErr(TIO_Create_Quant_F(fileID=file_id,meshID=obj_id,     &
&                  name="csqrd",quantID=qnt_id,datatype=TIO_REAL8_F,    &
&                  centring=TIO_CENTRE_CELL_F,nghosts=TIO_GHOSTS_NONE_F,&
&                  ismixed=.false.,units="-"),                          &
&                  "TIO_Create_Quant_F: csqrd")
    iErr = testErr(TIO_Write_UnstrQuant_Chunk_f(fileID=file_id,        &
&                  quantID=qnt_id,idx=idx,xfer=xfer,                   &
&                  datatype=TIO_REAL8_F, qdat=csqrd(1:nel)),           &
&                  "TIO_Write_UnstrQuant_Chunk_f: csqrd")
    iErr = testErr(TIO_Close_Quant_F(fileID=file_id,quantID=qnt_id),   &
&                 "TIO_Close_Quant_F: csqrd")

!   x node velocity
    iErr = testErr(TIO_Create_Quant_F(fileID=file_id,meshID=obj_id,     &
&                  name="X velocity",quantID=qnt_id,                    &
&                  datatype=TIO_REAL8_F, centring=TIO_CENTRE_NODE_F,    &
&                  nghosts=TIO_GHOSTS_NONE_F,ismixed=.false.,units="-"),&
&                  "TIO_Create_Quant_F: ndu")
    iErr = testErr(TIO_Write_UnstrQuant_Chunk_f(fileID=file_id,        &
&                  quantID=qnt_id,idx=idx,xfer=xfer,                   &
&                  datatype=TIO_REAL8_F, qdat=ndu(1:nnod)),            &
&                  "TIO_Write_UnstrQuant_Chunk_f: ndu")
    iErr = testErr(TIO_Close_Quant_F(fileID=file_id, quantID=qnt_id  ),&
&                 "TIO_Close_Quant_F: ndu")

!   y node velocity
    iErr = testErr(TIO_Create_Quant_F(fileID=file_id,meshID=obj_id,      &
&                  name="Y velocity",quantID=qnt_id,datatype=TIO_REAL8_F,&
&                  centring=TIO_CENTRE_NODE_F,nghosts=TIO_GHOSTS_NONE_F, &
&                  ismixed=.false., units="-"),                          &
&                  "TIO_Create_Quant_F: ndv")
    iErr = testErr(TIO_Write_UnstrQuant_Chunk_f(fileID=file_id,          &
&                  quantID=qnt_id,idx=idx,xfer=xfer,                     &
&                  datatype=TIO_REAL8_F, qdat=ndv(1:nnod)),              &
&                  "TIO_Write_UnstrQuant_Chunk_f: ndv")
    iErr = testErr(TIO_Close_Quant_F(fileID=file_id, quantID=qnt_id  ),  &
&                 "TIO_Close_Quant_F: ndv")

!   node type
    iErr = testErr(TIO_Create_Quant_F(fileID=file_id,meshID=obj_id,        &
&                  name="Node type",quantID=qnt_id,datatype=TIO_INTEGER4_F,&
&                  centring=TIO_CENTRE_NODE_F,nghosts=TIO_GHOSTS_NONE_F,   &
&                  ismixed=.false.,units="-"),                             &
&                  "TIO_Create_Quant_F: indtype")
    iErr = testErr(TIO_Write_UnstrQuant_Chunk_f(fileID=file_id,            &
&                  quantID=qnt_id,idx=idx,xfer=xfer,                       &
&                  datatype=TIO_INTEGER4_F,qdat=indtype(1:nnod)),          &
&                  "TIO_Write_UnstrQuant_Chunk_f: indtype")
    iErr = testErr(TIO_Close_Quant_F(fileID=file_id, quantID=qnt_id  ),    &
&                 "TIO_Close_Quant_F: indtype")

!   ielreg
    iErr = testErr(TIO_Create_Quant_F(fileID=file_id,meshID=obj_id,        &
&                  name="ielreg",quantID=qnt_id,datatype=TIO_INTEGER4_F,   &
&                  centring=TIO_CENTRE_CELL_F,nghosts=TIO_GHOSTS_NONE_F,   &
&                  ismixed=.false.,units="-"),                             &
&                  "TIO_Create_Quant_F: ielreg")
    iErr = testErr(TIO_Write_UnstrQuant_Chunk_f(fileID=file_id,            &
&                  quantID=qnt_id,idx=idx,xfer=xfer,                       &
&                  datatype=TIO_INTEGER4_F,                                &
&                  qdat=ielreg(1:nel)),                                    &
&                  "TIO_Write_UnstrQuant_Chunk_f: ielreg")
    iErr = testErr(TIO_Close_Quant_F(fileID=file_id, quantID=qnt_id),      &
&                 "TIO_Close_Quant_F: ielreg")

    iErr = testErr(TIO_Close_Mesh_F(fileID=file_id, meshID=obj_id ),"TIO_Close_Mesh_F")
    iErr = testErr(TIO_Close_State_f(fileID=file_id,stateID=state_id),"TIO_Close_State_f")
    iErr = testErr(TIO_Close_f(fileID=file_id),"TIO_Close_f")

    ! Timing data
    t1 = get_time()
    t1=t1-t0
    bookleaf_times%time_in_io=bookleaf_times%time_in_io+t1

  END SUBROUTINE write_tio_dump

END MODULE TyphonIO_mod
