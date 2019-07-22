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
MODULE data_mod

  ! Internal
  USE dataAPI_kinds_mod, ONLY: ink,lak,rlk,lok
  USE dataAPI_params_mod,ONLY: NFACE,NCORN,SUCCESS,ESTREAM,HALT_SINGLE
  USE dataAPI_types_mod, ONLY: data_t,config_t,sizes_t,error_t
  USE dataAPI_id_mod,    ONLY: eldensityid,elenergyid,elpressureid,elcs2id,    &
&                              elvolumeid,a1id,a2id,a3id,b1id,b2id,b3id,       &
&                              cnwtid,cnxid,cnyid,rscratch21id,rscratch22id,   &
&                              rscratch23id,rscratch24id,rscratch25id,         &
&                              rscratch26id,rscratch27id,rscratch28id,         &
&                              cnviscxid,cnviscyid,elcviscid,elmassid,cnmassid,&
&                              rscratch11id,rscratch12id,rscratch13id,         &
&                              rscratch14id,rscratch15id,rscratch16id,         &
&                              rscratch17id,ndxid,ndyid,nduid,ndvid,indtypeid, &
&                              ielsort1id,ielndid,ielelid,ielfcid,ielmatid,    &
&                              ielregid,ielsort2id,spmassid,iscratch11id,      &
&                              zscratch11id,iellocglobid,indlocglobid,         &
&                              cpdensityid,cpenergyid,cppressureid,cpcs2id,    &
&                              cpvolumeid,frvolumeid,cpmassid,frmassid,        &
&                              cpviscxid,cpviscyid,cpcviscid,icpmatid,imxelid, &
&                              imxfcpid,imxncpid,icpnextid,icpprevid,cpa1id,   &
&                              cpa3id,cpb1id,cpb3id,icpscratch11id,            &
&                              icpscratch12id,rcpscratch11id,rcpscratch21id,   &
&                              rcpscratch22id,rcpscratch23id,rcpscratch24id,   &
&                              rcpscratch25id
  USE TYPH_types_mod,    ONLY: TYPH_MESH_DIM,TYPH_REAL,TYPH_GHOSTS_ONE,        &
&                              TYPH_GHOSTS_TWO
  USE TYPH_register_mod, ONLY: TYPH_PURE,TYPH_CENTRE_CELL,TYPH_add_quant
  USE TYPH_quant_mod,    ONLY: TYPH_set_quant_address
  ! External
  USE iso_c_binding,     ONLY: c_loc,c_f_pointer,c_ptr

  IMPLICIT NONE

  ! permanent
  INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE,TARGET,PRIVATE :: ielreg,ielmat,  &
&                                                              indtype,ielnd,  &
&                                                              ielsort1,icpmat,&
&                                                              ielsort2,       &
&                                                              iellocglob,     &
&                                                              indlocglob,     &
&                                                              ielel,ielfc,    &
&                                                              icpnext,icpprev,&
&                                                              imxfcp,imxncp,  &
&                                                              imxel
  REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE,TARGET,PRIVATE :: eldensity,a1,a2,&
&                                                              elenergy,elmass,&
&                                                              elpressure,a3,  &
&                                                              elvolume,elcs2, &
&                                                              ndx,ndy,ndu,ndv,&
&                                                              cnviscx,cnviscy,&
&                                                              spmass,cnmass,  &
&                                                              cnwt,cnx,cny,b1,&
&                                                              elcvisc,b2,b3,  &
&                                                              cpdensity,cpcs2,&
&                                                              cpenergy,cpmass,&
&                                                              cpvolume,frmass,&
&                                                              frvolume,cpa1,  &
&                                                              cpa3,cpb1,cpb3, &
&                                                              cpcvisc,cpviscx,&
&                                                              cpviscy,        &
&                                                              cppressure
  ! temporary
  INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE,TARGET,PRIVATE :: iscratch11,     &
&                                                              icpscratch11,   &
&                                                              icpscratch12
  LOGICAL(KIND=lak),DIMENSION(:),ALLOCATABLE,TARGET,PRIVATE :: zscratch11
  REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE,TARGET,PRIVATE :: rscratch11,     &
&                                                              rscratch12,     &
&                                                              rscratch13,     &
&                                                              rscratch14,     &
&                                                              rscratch15,     &
&                                                              rscratch16,     &
&                                                              rscratch17,     &
&                                                              rscratch21,     &
&                                                              rscratch22,     &
&                                                              rscratch23,     &
&                                                              rscratch24,     &
&                                                              rscratch25,     &
&                                                              rscratch26,     &
&                                                              rscratch27,     &
&                                                              rscratch28,     &
&                                                              rcpscratch11,   &
&                                                              rcpscratch21,   &
&                                                              rcpscratch22,   &
&                                                              rcpscratch23,   &
&                                                              rcpscratch24,   &
&                                                              rcpscratch25

  ! Parameters
  INTEGER(KIND=ink),PARAMETER,PRIVATE :: NID=80_ink
  INTEGER(KIND=ink),PARAMETER,PRIVATE :: IINIT=-2000000000_ink
  REAL(KIND=rlk),   PARAMETER,PRIVATE :: RINIT=-2.0e12_rlk
  LOGICAL(KIND=lak),PARAMETER,PRIVATE :: ZINIT=.FALSE._lak

  INTERFACE data_entry
    MODULE PROCEDURE data_entryr,data_entryi,data_entryz,data_entrytr1,        &
&                    data_entrytr2
  END INTERFACE data_entry

  INTERFACE data_reset
    MODULE PROCEDURE data_resetr,data_reseti,data_resettr,data_resetti,        &
&                    data_resetsi,data_resetsr
  END INTERFACE data_reset

  PRIVATE :: data_entry,data_entryr,data_entryi,data_entryz,data_entrytr1,     &
&            data_entrytr2,data_reset,data_resetr,data_reseti,data_resettr,    &
&            data_resetti
  PUBLIC  :: data_set,data_setinit,data_setmesh,data_setquant,data_settyph,    &
&            data_reset_mxquant,data_reset_cpquant

CONTAINS

  SUBROUTINE data_set(config,sizes,dh,error)

    ! Argument list
    TYPE(config_t),                         INTENT(IN)    :: config
    TYPE(sizes_t),                          INTENT(INOUT) :: sizes
    TYPE(data_t),  DIMENSION(:),ALLOCATABLE,INTENT(OUT)   :: dh
    TYPE(error_t),                          INTENT(OUT)   :: error

    ! Initialise data setup
    CALL data_setinit(sizes,dh)

    ! Set mesh data
    CALL data_setmesh(sizes,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! Set quant data
    CALL data_setquant(config,sizes,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN

  END SUBROUTINE data_set

  SUBROUTINE data_setinit(sizes,dh)

    ! Argument list
    TYPE(sizes_t),                         INTENT(INOUT) :: sizes
    TYPE(data_t), DIMENSION(:),ALLOCATABLE,INTENT(OUT)   :: dh
    ! Local
    INTEGER(KIND=ink) :: ii

    ! Setup dh
    ALLOCATE(dh(NID))  
    DO ii=1,NID
      NULLIFY(dh(ii)%iaddr,dh(ii)%raddr,dh(ii)%zaddr)
    ENDDO

    ! Store maximum size
    sizes%nsz=MAX(sizes%nel2,sizes%nnd2)

  END SUBROUTINE data_setinit

  SUBROUTINE data_setquant(config,sizes,dh,error)

    ! Argument list
    TYPE(config_t),             INTENT(IN)    :: config
    TYPE(sizes_t),              INTENT(IN)    :: sizes
    TYPE(data_t),  DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),              INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink) :: nel,nnd,nsz
    LOGICAL(KIND=lok) :: zmpi

    ! initialise
    error%ierr=SUCCESS

    ! copy to local storage
    nel=sizes%nel2
    nnd=sizes%nnd2
    nsz=sizes%nsz
    zmpi=config%comm%zmpi

    ! set quant. memory
    CALL data_entry("eldensity",TYPH_GHOSTS_TWO,TYPH_CENTRE_CELL,zmpi,RINIT,   &
&                   [nel],eldensity,dh(eldensityid),error%ierr)
    CALL data_entry("elenergy",TYPH_GHOSTS_TWO,TYPH_CENTRE_CELL,zmpi,RINIT,    &
&                   [nel],elenergy,dh(elenergyid),error%ierr)
    CALL data_entry("elpressure",RINIT,[nel],elpressure,dh(elpressureid),      &
&                   error%ierr)
    CALL data_entry("elcs2",RINIT,[nel],elcs2,dh(elcs2id),error%ierr)
    CALL data_entry("elvolume",TYPH_GHOSTS_TWO,TYPH_CENTRE_CELL,zmpi,RINIT,    &
&                   [nel],elvolume,dh(elvolumeid),error%ierr)
    CALL data_entry("a1",RINIT,[nel],a1,dh(a1id),error%ierr)
    CALL data_entry("a2",RINIT,[nel],a2,dh(a2id),error%ierr)
    CALL data_entry("a3",RINIT,[nel],a3,dh(a3id),error%ierr)
    CALL data_entry("b1",RINIT,[nel],b1,dh(b1id),error%ierr)
    CALL data_entry("b2",RINIT,[nel],b2,dh(b2id),error%ierr)
    CALL data_entry("b3",RINIT,[nel],b3,dh(b3id),error%ierr)
    CALL data_entry("cnwt",TYPH_GHOSTS_TWO,TYPH_CENTRE_CELL,zmpi,RINIT,2,      &
&                   [NCORN,nel],cnwt,dh(cnwtid),error%ierr)
    CALL data_entry("cnx",RINIT,[NCORN,nel],cnx,dh(cnxid),error%ierr)
    CALL data_entry("cny",RINIT,[NCORN,nel],cny,dh(cnyid),error%ierr)
    CALL data_entry("ndu",RINIT,[nnd],ndu,dh(nduid),error%ierr)
    CALL data_entry("ndv",RINIT,[nnd],ndv,dh(ndvid),error%ierr)
    CALL data_entry("rscratch21",TYPH_GHOSTS_TWO,TYPH_CENTRE_CELL,zmpi,RINIT,2,&
&                   [NCORN,nsz],rscratch21,dh(rscratch21id),error%ierr)
    CALL data_entry("rscratch22",TYPH_GHOSTS_TWO,TYPH_CENTRE_CELL,zmpi,RINIT,2,&
&                   [NCORN,nsz],rscratch22,dh(rscratch22id),error%ierr)
    CALL data_entry("rscratch23",TYPH_GHOSTS_ONE,TYPH_CENTRE_CELL,zmpi,RINIT,2,&
&                   [NCORN,nsz],rscratch23,dh(rscratch23id),error%ierr)
    CALL data_entry("rscratch24",TYPH_GHOSTS_ONE,TYPH_CENTRE_CELL,zmpi,RINIT,2,&
&                   [NCORN,nsz],rscratch24,dh(rscratch24id),error%ierr)
    CALL data_entry("rscratch25",TYPH_GHOSTS_ONE,TYPH_CENTRE_CELL,zmpi,RINIT,2,&
&                   [NCORN,nsz],rscratch25,dh(rscratch25id),error%ierr)
    CALL data_entry("rscratch26",TYPH_GHOSTS_ONE,TYPH_CENTRE_CELL,zmpi,RINIT,2,&
&                   [NCORN,nsz],rscratch26,dh(rscratch26id),error%ierr)
    CALL data_entry("rscratch27",TYPH_GHOSTS_TWO,TYPH_CENTRE_CELL,zmpi,RINIT,2,&
&                   [NCORN,nsz],rscratch27,dh(rscratch27id),error%ierr)
    CALL data_entry("rscratch28",TYPH_GHOSTS_TWO,TYPH_CENTRE_CELL,zmpi,RINIT,2,&
&                   [NCORN,nsz],rscratch28,dh(rscratch28id),error%ierr)
    CALL data_entry("cnviscx",RINIT,[NCORN,nel],cnviscx,dh(cnviscxid),         &
&                   error%ierr)
    CALL data_entry("cnviscy",RINIT,[NCORN,nel],cnviscy,dh(cnviscyid),         &
&                   error%ierr)
    CALL data_entry("elcvisc",RINIT,[nel],elcvisc,dh(elcviscid),error%ierr)
    CALL data_entry("elmass",RINIT,[nel],elmass,dh(elmassid),error%ierr)
    CALL data_entry("cnmass",TYPH_GHOSTS_TWO,TYPH_CENTRE_CELL,zmpi,RINIT,2,    &
&                   [NCORN,nel],cnmass,dh(cnmassid),error%ierr)
    CALL data_entry("rscratch11",TYPH_GHOSTS_TWO,TYPH_CENTRE_CELL,zmpi,RINIT,  &
&                   [nsz],rscratch11,dh(rscratch11id),error%ierr)
    CALL data_entry("rscratch12",RINIT,[nsz],rscratch12,dh(rscratch12id),      &
&                   error%ierr)
    CALL data_entry("rscratch13",RINIT,[nsz],rscratch13,dh(rscratch13id),      &
&                   error%ierr)
    CALL data_entry("rscratch14",RINIT,[nsz],rscratch14,dh(rscratch14id),      &
&                   error%ierr)
    CALL data_entry("rscratch15",RINIT,[nsz],rscratch15,dh(rscratch15id),      &
&                   error%ierr)
    CALL data_entry("rscratch16",RINIT,[nsz],rscratch16,dh(rscratch16id),      &
&                   error%ierr)
    CALL data_entry("rscratch17",RINIT,[nsz],rscratch17,dh(rscratch17id),      &
&                   error%ierr)
    CALL data_entry("ielsort1",IINIT,[nel],ielsort1,dh(ielsort1id),error%ierr)
    CALL data_entry("ielel",IINIT,[NFACE,nel],ielel,dh(ielelid),error%ierr)
    CALL data_entry("ielfc",IINIT,[NFACE,nel],ielfc,dh(ielfcid),error%ierr)
    CALL data_entry("iscratch11",IINIT,[nsz],iscratch11,dh(iscratch11id),      &
&                   error%ierr)
    IF (config%hydro%zsp) THEN
      CALL data_entry("spmass",RINIT,[NCORN,nel],spmass,dh(spmassid),          &
&                     error%ierr)
    ENDIF
    IF (config%ale%zexist) THEN
      CALL data_entry("ielsort2",IINIT,[nel],ielsort2,dh(ielsort2id),          &
&                     error%ierr)
      CALL data_entry("zscratch11",ZINIT,[nsz],zscratch11,dh(zscratch11id),    &
&                     error%ierr)
    ENDIF

    ! error handle
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr='ERROR: failed in data_setquant'
      error%iout=HALT_SINGLE
    ENDIF

  END SUBROUTINE data_setquant

  SUBROUTINE data_setmesh(sizes,dh,error)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),             INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink) :: nel,nnd

    ! initialise
    error%ierr=SUCCESS

    ! copy local sizes
    nel=sizes%nel2
    nnd=sizes%nnd2

    ! set mesh memory
    CALL data_entry("ndx",RINIT,[nnd],ndx,dh(ndxid),error%ierr)
    CALL data_entry("ndy",RINIT,[nnd],ndy,dh(ndyid),error%ierr)
    CALL data_entry("ndtype",IINIT,[nnd],indtype,dh(indtypeid),error%ierr)
    CALL data_entry("elnd",IINIT,[NCORN,nel],ielnd,dh(ielndid),error%ierr)
    CALL data_entry("elmaterial",IINIT,[nel],ielmat,dh(ielmatid),error%ierr)
    CALL data_entry("elregion",IINIT,[nel],ielreg,dh(ielregid),error%ierr)

    ! error handle
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr='ERROR: failed in data_setmesh'
      error%iout=HALT_SINGLE
    ENDIF

  END SUBROUTINE data_setmesh

  SUBROUTINE data_settyph(sizes,dh,error)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),             INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink) :: nel,nnd

    ! initialise
    error%ierr=SUCCESS

    ! copy local sizes
    nel=sizes%nel2
    nnd=sizes%nnd2

    ! set typhon memory
    CALL data_entry("iellocglob",IINIT,[nel],iellocglob,dh(iellocglobid),      &
&                   error%ierr)
    CALL data_entry("indlocglob",IINIT,[nnd],indlocglob,dh(indlocglobid),      &
&                   error%ierr)

    ! error handle
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr='ERROR: failed in data_settyph'
      error%iout=HALT_SINGLE
    ENDIF

  END SUBROUTINE data_settyph

  SUBROUTINE data_reset_cpquant(nsize,sizes,dh,ierr)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: nsize
    TYPE(sizes_t),                 INTENT(INOUT) :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    INTEGER(KIND=ink),             INTENT(OUT)   :: ierr

    ! initialise
    ierr=SUCCESS

    ! Return if new data size smaller than current data size
    IF (nsize.LE.sizes%mcp) RETURN

    ! reset component permanent data
    CALL data_reset("cpdensity",RINIT,[nsize],cpdensity,dh(cpdensityid),ierr)
    CALL data_reset("cpenergy",RINIT,[nsize],cpenergy,dh(cpenergyid),ierr)
    CALL data_reset("cppressure",RINIT,[nsize],cppressure,dh(cppressureid),    &
&                   ierr)
    CALL data_reset("cpcs2",RINIT,[nsize],cpcs2,dh(cpcs2id),ierr)
    CALL data_reset("cpvolume",RINIT,[nsize],cpvolume,dh(cpvolumeid),ierr)
    CALL data_reset("frvolume",RINIT,[nsize],frvolume,dh(frvolumeid),ierr)
    CALL data_reset("cpmass",RINIT,[nsize],cpmass,dh(cpmassid),ierr)
    CALL data_reset("frmass",RINIT,[nsize],frmass,dh(frmassid),ierr)
    CALL data_reset("cpviscx",RINIT,[nsize],cpviscx,dh(cpviscxid),ierr)
    CALL data_reset("cpviscy",RINIT,[nsize],cpviscy,dh(cpviscyid),ierr)
    CALL data_reset("cpcvisc",RINIT,[nsize],cpcvisc,dh(cpcviscid),ierr)
    CALL data_reset("cpa1",RINIT,[nsize],cpa1,dh(cpa1id),ierr)
    CALL data_reset("cpa3",RINIT,[nsize],cpa3,dh(cpa3id),ierr)
    CALL data_reset("cpb1",RINIT,[nsize],cpb1,dh(cpb1id),ierr)
    CALL data_reset("cpb3",RINIT,[nsize],cpb3,dh(cpb3id),ierr)
    CALL data_reset("icpmat",IINIT,[nsize],icpmat,dh(icpmatid),ierr)
    CALL data_reset("icpnext",IINIT,[nsize],icpnext,dh(icpnextid),ierr)
    CALL data_reset("icpprev",IINIT,[nsize],icpprev,dh(icpprevid),ierr)

    ! reset component scratch data
    CALL data_reset("icpscratch11",[nsize],icpscratch11,dh(icpscratch11id),    &
&                   ierr)
    CALL data_reset("icpscratch12",[nsize],icpscratch12,dh(icpscratch12id),    &
&                   ierr)
    CALL data_reset("rcpscratch11",[nsize],rcpscratch11,dh(rcpscratch11id),    &
&                   ierr)
    CALL data_reset("rcpscratch21",[NCORN,nsize],rcpscratch21,                 &
&                   dh(rcpscratch21id),ierr)
    CALL data_reset("rcpscratch22",[NCORN,nsize],rcpscratch22,                 &
&                   dh(rcpscratch22id),ierr)
    CALL data_reset("rcpscratch23",[NCORN,nsize],rcpscratch23,                 &
&                   dh(rcpscratch23id),ierr)
    CALL data_reset("rcpscratch24",[NCORN,nsize],rcpscratch24,                 &
&                   dh(rcpscratch24id),ierr)
    CALL data_reset("rcpscratch25",[NCORN,nsize],rcpscratch25,                 &
&                   dh(rcpscratch25id),ierr)

    ! error handle
    IF (ierr.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed in data_reset_cpquant"
      RETURN
    ENDIF

    ! store new size
    sizes%mcp=nsize

  END SUBROUTINE data_reset_cpquant

  SUBROUTINE data_reset_mxquant(nsize,sizes,dh,ierr)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: nsize
    TYPE(sizes_t),                 INTENT(INOUT) :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    INTEGER(KIND=ink),             INTENT(OUT)   :: ierr

    ! initialise
    ierr=SUCCESS

    ! return if new data size smaller than current data size
    IF (nsize.LT.sizes%mmx) RETURN

    ! reset multi-material permanent data
    CALL data_reset("imxel", IINIT,[nsize],imxel, dh(imxelid), ierr)
    CALL data_reset("imxncp",IINIT,[nsize],imxncp,dh(imxncpid),ierr)
    CALL data_reset("imxfcp",IINIT,[nsize],imxfcp,dh(imxfcpid),ierr)

    ! error handle
    IF (ierr.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed in data_reset_mxquant"
      RETURN
    ENDIF

    ! store new size
    sizes%mmx=nsize

  END SUBROUTINE data_reset_mxquant

  SUBROUTINE data_entryr(dname,rinit,idims,array,dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                 INTENT(IN)  :: dname
    REAL(KIND=rlk),                                   INTENT(IN)  :: rinit
    INTEGER(KIND=ink),DIMENSION(:),                   INTENT(IN)  :: idims
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE,TARGET,INTENT(OUT) :: array
    TYPE(data_t),                                     INTENT(OUT) :: dh
    INTEGER(KIND=ink),                                INTENT(OUT) :: ierr
    ! Local
    TYPE(c_ptr)  :: caddr

    ierr=SUCCESS
    dh%dname=TRIM(ADJUSTL(dname))
    dh%dsize=PRODUCT(idims)
    ALLOCATE(array(dh%dsize),STAT=ierr)
    IF (ierr.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dh%dname
      RETURN
    ENDIF
    array(:)=rinit
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%raddr,SHAPE=[1])
    dh%taddr=0_ink

  END SUBROUTINE data_entryr

  SUBROUTINE data_entryi(dname,iinit,idims,array,dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                 INTENT(IN)  :: dname
    INTEGER(KIND=ink),                                INTENT(IN)  :: iinit
    INTEGER(KIND=ink),DIMENSION(:),                   INTENT(IN)  :: idims
    INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE,TARGET,INTENT(OUT) :: array
    TYPE(data_t),                                     INTENT(OUT) :: dh
    INTEGER(KIND=ink),                                INTENT(OUT) :: ierr
    ! Local
    TYPE(c_ptr) :: caddr

    ierr=SUCCESS
    dh%dname=TRIM(ADJUSTL(dname))
    dh%dsize=PRODUCT(idims)
    ALLOCATE(array(dh%dsize),STAT=ierr)
    IF (ierr.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dh%dname
      RETURN
    ENDIF
    array(:)=iinit
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%iaddr,SHAPE=[1])
    dh%taddr=0_ink

  END SUBROUTINE data_entryi

  SUBROUTINE data_entryz(dname,zinit,idims,array,dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                 INTENT(IN)  :: dname
    LOGICAL(KIND=lak),                                INTENT(IN)  :: zinit
    INTEGER(KIND=ink),DIMENSION(:),                   INTENT(IN)  :: idims
    LOGICAL(KIND=lak),DIMENSION(:),ALLOCATABLE,TARGET,INTENT(OUT) :: array
    TYPE(data_t),                                     INTENT(OUT) :: dh
    INTEGER(KIND=ink),                                INTENT(OUT) :: ierr
    ! Local
    TYPE(c_ptr) :: caddr

    ierr=SUCCESS
    dh%dname=TRIM(ADJUSTL(dname))
    dh%dsize=PRODUCT(idims)
    ALLOCATE(array(dh%dsize),STAT=ierr)
    IF (ierr.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dh%dname
      RETURN
    ENDIF
    array(:)=zinit
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%zaddr,SHAPE=[1])
    dh%taddr=0_ink

  END SUBROUTINE data_entryz

  SUBROUTINE data_entrytr1(dname,nghost,centre,zmpi,rinit,idims,array,dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                   INTENT(IN)  :: dname
    INTEGER(KIND=ink),                                  INTENT(IN)  :: nghost
    INTEGER(KIND=ink),                                  INTENT(IN)  :: centre
    LOGICAL(KIND=lok),                                  INTENT(IN)  :: zmpi
    REAL(KIND=rlk),                                     INTENT(IN)  :: rinit
    INTEGER(KIND=ink),  DIMENSION(:),                   INTENT(IN)  :: idims
    REAL(KIND=rlk),     DIMENSION(:),ALLOCATABLE,TARGET,INTENT(OUT) :: array
    TYPE(data_t),                                       INTENT(OUT) :: dh
    INTEGER(KIND=ink),                                  INTENT(OUT) :: ierr
    ! Local
    TYPE(c_ptr) :: caddr

    ierr=SUCCESS
    dh%dname=TRIM(ADJUSTL(dname))
    dh%dsize=PRODUCT(idims)
    ALLOCATE(array(dh%dsize),STAT=ierr)
    IF (ierr.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dh%dname
      RETURN
    ENDIF
    array(:)=rinit
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%raddr,SHAPE=[1])
    IF (zmpi) THEN
      ierr=TYPH_add_quant(dh%taddr,dh%dname,nghost,TYPH_REAL,centre,TYPH_PURE)
      ierr=TYPH_set_quant_address(dh%taddr,array)
    ELSE
      dh%taddr=0_ink
    ENDIF

  END SUBROUTINE data_entrytr1

  SUBROUTINE data_entrytr2(dname,nghost,centre,zmpi,rinit,meshdim,idims,array, &
&                          dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                   INTENT(IN)  :: dname
    INTEGER(KIND=ink),                                  INTENT(IN)  :: nghost
    INTEGER(KIND=ink),                                  INTENT(IN)  :: centre
    LOGICAL(KIND=lok),                                  INTENT(IN)  :: zmpi
    REAL(KIND=rlk),                                     INTENT(IN)  :: rinit
    INTEGER(KIND=ink),                                  INTENT(IN)  :: meshdim
    INTEGER(KIND=ink),  DIMENSION(:),                   INTENT(IN)  :: idims
    REAL(KIND=rlk),     DIMENSION(:),ALLOCATABLE,TARGET,INTENT(OUT) :: array
    TYPE(data_t),                                       INTENT(OUT) :: dh
    INTEGER(KIND=ink),                                  INTENT(OUT) :: ierr
    ! Local
    INTEGER(KIND=ink),  DIMENSION(SIZE(idims)) :: tdims
    TYPE(c_ptr)                                :: caddr

    ierr=SUCCESS
    dh%dname=TRIM(ADJUSTL(dname))
    dh%dsize=PRODUCT(idims)
    ALLOCATE(array(dh%dsize),STAT=ierr)
    IF (ierr.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dh%dname
      RETURN
    ENDIF
    array(:)=rinit
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%raddr,SHAPE=[1])
    IF (zmpi) THEN
      tdims=idims
      tdims(meshdim)=TYPH_MESH_DIM
      ierr=TYPH_add_quant(dh%taddr,dh%dname,nghost,TYPH_REAL,centre,TYPH_PURE, &
&                         dims=tdims)
      ierr=TYPH_set_quant_address(dh%taddr,array)
    ELSE
      dh%taddr=0_ink
    ENDIF

  END SUBROUTINE data_entrytr2

  SUBROUTINE data_resetr(dname,rinit,idims,array,dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                 INTENT(IN)    :: dname
    REAL(KIND=rlk),                                   INTENT(IN)    :: rinit
    INTEGER(KIND=ink),DIMENSION(:),                   INTENT(IN)    :: idims
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE,TARGET,INTENT(INOUT) :: array
    TYPE(data_t),                                     INTENT(INOUT) :: dh
    INTEGER(KIND=ink),                                INTENT(INOUT) :: ierr
    ! Local
    INTEGER(KIND=ink)                          :: ierrl
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE :: scratch
    TYPE(c_ptr)                                :: caddr

    ierrl=SUCCESS
    dh%dsize=PRODUCT(idims)
    ALLOCATE(scratch(dh%dsize),STAT=ierrl)
    IF (ierrl.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dname
      ierr=ierr+ABS(ierrl)
      RETURN
    ENDIF
    IF (ALLOCATED(array)) THEN
      scratch(1:SIZE(array))=array(:)
      scratch(SIZE(array)+1:dh%dsize)=rinit
      DEALLOCATE(array,STAT=ierrl)
      IF (ierrl.NE.SUCCESS) THEN
        WRITE(ESTREAM,*) "ERROR: failed to deallocate "//dh%dname
        ierr=ierr+ABS(ierrl)
        RETURN
      ENDIF
    ELSE
      dh%dname=TRIM(ADJUSTL(dname))
      scratch(:)=rinit
    ENDIF
    CALL MOVE_ALLOC(scratch,array)
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%raddr,SHAPE=[1])
    dh%taddr=0_ink

  END SUBROUTINE data_resetr

  SUBROUTINE data_reseti(dname,iinit,idims,array,dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                 INTENT(IN)    :: dname
    INTEGER(KIND=ink),                                INTENT(IN)    :: iinit
    INTEGER(KIND=ink),DIMENSION(:),                   INTENT(IN)    :: idims
    INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE,TARGET,INTENT(INOUT) :: array
    TYPE(data_t),                                     INTENT(INOUT) :: dh
    INTEGER(KIND=ink),                                INTENT(INOUT) :: ierr
    ! Local
    INTEGER(KIND=ink)                          :: ierrl
    INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE :: scratch
    TYPE(c_ptr)                                :: caddr

    ierrl=SUCCESS
    dh%dsize=PRODUCT(idims)
    ALLOCATE(scratch(dh%dsize),STAT=ierrl)
    IF (ierrl.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dname
      ierr=ierr+ABS(ierrl)
      RETURN
    ENDIF
    IF (ALLOCATED(array)) THEN
      scratch(1:SIZE(array))=array(:)
      scratch(SIZE(array)+1:dh%dsize)=iinit
      DEALLOCATE(array,STAT=ierrl)
      IF (ierrl.NE.SUCCESS) THEN
        WRITE(ESTREAM,*) "ERROR: failed to deallocate "//dh%dname
        ierr=ierr+ABS(ierrl)
        RETURN
      ENDIF
    ELSE
      dh%dname=TRIM(ADJUSTL(dname))
      scratch(:)=iinit
    ENDIF
    CALL MOVE_ALLOC(scratch,array)
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%iaddr,SHAPE=[1])
    dh%taddr=0_ink

  END SUBROUTINE data_reseti

  SUBROUTINE data_resetsi(dname,idims,array,dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                 INTENT(IN)    :: dname
    INTEGER(KIND=ink),DIMENSION(:),                   INTENT(IN)    :: idims
    INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE,TARGET,INTENT(INOUT) :: array
    TYPE(data_t),                                     INTENT(INOUT) :: dh
    INTEGER(KIND=ink),                                INTENT(INOUT) :: ierr
    ! Local
    INTEGER(KIND=ink)                          :: ierrl
    INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE :: scratch
    TYPE(c_ptr)                                :: caddr

    ierrl=SUCCESS
    dh%dsize=PRODUCT(idims)
    ALLOCATE(scratch(dh%dsize),STAT=ierrl)
    IF (ierrl.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dname
      ierr=ierr+ABS(ierrl)
      RETURN
    ENDIF
    IF (ALLOCATED(array)) THEN
      DEALLOCATE(array,STAT=ierrl)
      IF (ierrl.NE.SUCCESS) THEN
        WRITE(ESTREAM,*) "ERROR: failed to deallocate "//dh%dname
        ierr=ierr+ABS(ierrl)
        RETURN
      ENDIF
    ELSE
      dh%dname=TRIM(ADJUSTL(dname))
    ENDIF
    CALL MOVE_ALLOC(scratch,array)
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%iaddr,SHAPE=[1])
    dh%taddr=0_ink

  END SUBROUTINE data_resetsi

  SUBROUTINE data_resetsr(dname,idims,array,dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                 INTENT(IN)    :: dname
    INTEGER(KIND=ink),DIMENSION(:),                   INTENT(IN)    :: idims
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE,TARGET,INTENT(INOUT) :: array
    TYPE(data_t),                                     INTENT(INOUT) :: dh
    INTEGER(KIND=ink),                                INTENT(INOUT) :: ierr
    ! Local
    INTEGER(KIND=ink)                          :: ierrl
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE :: scratch
    TYPE(c_ptr)                                :: caddr

    ierrl=SUCCESS
    dh%dsize=PRODUCT(idims)
    ALLOCATE(scratch(dh%dsize),STAT=ierrl)
    IF (ierrl.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dname
      ierr=ierr+ABS(ierrl)
      RETURN
    ENDIF
    IF (ALLOCATED(array)) THEN
      DEALLOCATE(array,STAT=ierrl)
      IF (ierrl.NE.SUCCESS) THEN
        WRITE(ESTREAM,*) "ERROR: failed to deallocate "//dh%dname
        ierr=ierr+ABS(ierrl)
        RETURN
      ENDIF
    ELSE
      dh%dname=TRIM(ADJUSTL(dname))
    ENDIF
    CALL MOVE_ALLOC(scratch,array)
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%raddr,SHAPE=[1])
    dh%taddr=0_ink

  END SUBROUTINE data_resetsr

  SUBROUTINE data_resettr(dname,nghost,centre,zmpi,rinit,idims,array,dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                 INTENT(IN)    :: dname
    INTEGER(KIND=ink),                                INTENT(IN)    :: nghost
    INTEGER(KIND=ink),                                INTENT(IN)    :: centre
    LOGICAL(KIND=lok),                                INTENT(IN)    :: zmpi
    REAL(KIND=rlk),                                   INTENT(IN)    :: rinit
    INTEGER(KIND=ink),DIMENSION(:),                   INTENT(IN)    :: idims
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE,TARGET,INTENT(INOUT) :: array
    TYPE(data_t),                                     INTENT(INOUT) :: dh
    INTEGER(KIND=ink),                                INTENT(INOUT) :: ierr
    ! Local
    INTEGER(KIND=ink)                          :: ierrl
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE :: scratch
    TYPE(c_ptr)                                :: caddr

    ierrl=SUCCESS
    dh%dsize=PRODUCT(idims)
    ALLOCATE(scratch(dh%dsize),STAT=ierrl)
    IF (ierrl.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dname
      ierr=ierr+ABS(ierrl)
      RETURN
    ENDIF
    IF (ALLOCATED(array)) THEN
      scratch(1:SIZE(array))=array(:)
      scratch(SIZE(array)+1:dh%dsize)=rinit
      DEALLOCATE(array,STAT=ierrl)
      IF (ierrl.NE.SUCCESS) THEN
        WRITE(ESTREAM,*) "ERROR: failed to deallocate "//dh%dname
        ierr=ierr+ABS(ierrl)
        RETURN
      ENDIF
    ELSE
      dh%dname=TRIM(ADJUSTL(dname))
      dh%taddr=-1_ink
      scratch(:)=rinit
    ENDIF
    CALL MOVE_ALLOC(scratch,array)
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%raddr,SHAPE=[1])
    IF (zmpi) THEN
      IF (dh%taddr.EQ.-1_ink) ierr=TYPH_add_quant(dh%taddr,dh%dname,nghost,    &
&      TYPH_REAL,centre,TYPH_PURE)
      ierr=TYPH_set_quant_address(dh%taddr,array)
    ELSE
      dh%taddr=0_ink
    ENDIF

  END SUBROUTINE data_resettr

  SUBROUTINE data_resetti(dname,nghost,centre,zmpi,iinit,idims,array,dh,ierr)

    ! Argument list
    CHARACTER(LEN=*),                                 INTENT(IN)    :: dname
    INTEGER(KIND=ink),                                INTENT(IN)    :: nghost
    INTEGER(KIND=ink),                                INTENT(IN)    :: centre
    LOGICAL(KIND=lok),                                INTENT(IN)    :: zmpi
    INTEGER(KIND=ink),                                INTENT(IN)    :: iinit
    INTEGER(KIND=ink),DIMENSION(:),                   INTENT(IN)    :: idims
    INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE,TARGET,INTENT(INOUT) :: array
    TYPE(data_t),                                     INTENT(INOUT) :: dh
    INTEGER(KIND=ink),                                INTENT(INOUT) :: ierr
    ! Local
    INTEGER(KIND=ink)                          :: ierrl
    INTEGER(KIND=ink),DIMENSION(:),ALLOCATABLE :: scratch
    TYPE(c_ptr)                                :: caddr

    ierrl=SUCCESS
    dh%dsize=PRODUCT(idims)
    ALLOCATE(scratch(dh%dsize),STAT=ierrl)
    IF (ierrl.NE.SUCCESS) THEN
      WRITE(ESTREAM,*) "ERROR: failed to allocate "//dname
      ierr=ierr+ABS(ierrl)
      RETURN
    ENDIF
    IF (ALLOCATED(array)) THEN
      scratch(1:SIZE(array))=array(:)
      scratch(SIZE(array)+1:dh%dsize)=iinit
      DEALLOCATE(array,STAT=ierrl)
      IF (ierrl.NE.SUCCESS) THEN
        WRITE(ESTREAM,*) "ERROR: failed to deallocate "//dh%dname
        ierr=ierr+ABS(ierrl)
        RETURN
      ENDIF
    ELSE
      dh%dname=TRIM(ADJUSTL(dname))
      dh%taddr=-1_ink
      scratch(:)=iinit
    ENDIF
    CALL MOVE_ALLOC(scratch,array)
    caddr=C_LOC(array)
    CALL C_F_POINTER(caddr,dh%raddr,SHAPE=[1])
    IF (zmpi) THEN
      IF (dh%taddr.EQ.-1_ink) ierr=TYPH_add_quant(dh%taddr,dh%dname,nghost,    &
&      TYPH_REAL,centre,TYPH_PURE)
      ierr=TYPH_set_quant_address(dh%taddr,array)
    ELSE
      dh%taddr=0_ink
    ENDIF

  END SUBROUTINE data_resetti

END MODULE data_mod

