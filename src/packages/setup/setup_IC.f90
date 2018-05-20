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
MODULE setup_IC_mod

  USE dataAPI_kinds_mod,       ONLY: ink,rlk,lok
  USE dataAPI_types_mod,       ONLY: data_t,sizes_t,error_t,comm_t,io_t,       &
&                                    global_t
  USE dataAPI_params_mod,      ONLY: SUCCESS,FAILURE,HALT_SINGLE,SLEN,NLEN,    &
&                                    NCORN,NDIM,OSTREAM
  USE dataAPI_id_mod,          ONLY: ielregid,ielmatid,ielndid,elvolumeid,     &
&                                    cpvolumeid,elmassid,cpmassid,eldensityid, &
&                                    cpdensityid,elenergyid,cpenergyid,        &
&                                    icpmatid,iellocglobid,ndxid,ndyid,nduid,  &
&                                    ndvid,cnxid=>rscratch21id,imxfcpid,       &
&                                    cnyid=>rscratch22id,frvolumeid,imxncpid,  &
&                                    imxelid,icpmatid,icpnextid,cnwtid,        &
&                                    rscratch11id
  USE timerAPI_types_mod,      ONLY: timer_t
  USE timerAPI_id_mod,         ONLY: TSETUPICID,TGETGEOMETRYIID
  USE timer_advance_mod,       ONLY: timer_start,timer_end
  USE io_cf_mod,               ONLY: io_cf_get,io_cf_set
  USE utils_kn_string_mod,     ONLY: utils_kn_findstr,utils_kn_convupper
  USE utils_dr_bc_mod,         ONLY: utils_dr_bc
  USE utils_kn_gather_mod,     ONLY: utils_kn_cngather
  USE utils_kn_access_mod,     ONLY: utils_kn_get,utils_kn_set
  USE geometry_dr_volume_mod,  ONLY: geometry_dr_getgeometry
  USE geometry_kn_centroid_mod,ONLY: geometry_kn_getcentroid
  USE mix_dr_list_mod,         ONLY: mix_dr_addel,mix_dr_addcp,mix_dr_flatten

  IMPLICIT NONE

  TYPE,PRIVATE :: thermodynamic_t
    INTEGER(KIND=ink) :: itype,param,ScaleEnergy
    REAL(KIND=rlk)    :: density,energy
  END TYPE thermodynamic_t

  TYPE,PRIVATE :: kinematic_t
    INTEGER(KIND=ink)           :: itype,igeom,param
    REAL(KIND=rlk),DIMENSION(3) :: dat
  END TYPE kinematic_t

  TYPE,PRIVATE :: IC_t
    TYPE(thermodynamic_t),DIMENSION(:),ALLOCATABLE :: thermodynamic
    TYPE(kinematic_t),    DIMENSION(:),ALLOCATABLE :: kinematic
  END TYPE IC_t

  TYPE,PRIVATE :: indicator_t
    INTEGER(KIND=ink) :: itype,param,inumber
  END TYPE indicator_t

  TYPE,PRIVATE :: shape_t
    INTEGER(KIND=ink)              :: itype
    REAL(KIND=rlk),   DIMENSION(5) :: param
  END TYPE shape_t

  TYPE(indicator_t),DIMENSION(:),ALLOCATABLE,SAVE,PRIVATE :: materials,regions
  TYPE(IC_t),                                     PRIVATE :: IC
  TYPE(shape_t),    DIMENSION(:),ALLOCATABLE,SAVE,PRIVATE :: forms
  LOGICAL(KIND=lok),                         SAVE,PRIVATE :: rmesh=.FALSE._lok,&
&                                                            mmesh=.FALSE._lok
  INTEGER(KIND=ink),PARAMETER,                    PRIVATE :: CELL=1_ink,       &
&                                                            MESH=2_ink,       &
&                                                            BACKGROUND=3_ink, &
&                                                            SHAPES=4_ink,     &
&                                                            REGION=5_ink,     &
&                                                            MATERIAL=6_ink,   &
&                                                            LN=100_ink,       &
&                                                            VOLUME=1_ink,     &
&                                                            MASS=2_ink,       &
&                                                            ZERO=0_ink,       &
&                                                            RADIAL=1_ink,     &
&                                                            PLANAR=2_ink,     &
&                                                            RECTANGLE=1_ink,  &
&                                                            CIRCLE=2_ink,     &
&                                                            MAXIT=10_ink

  INTERFACE setup_init_zero
    MODULE PROCEDURE setup_init_izero,setup_init_rzero
  END INTERFACE setup_init_zero

  PROCEDURE(inside_circle),POINTER,PRIVATE :: inside
  PROCEDURE(region_shape), POINTER,PRIVATE :: apply_shape

  PRIVATE :: setup_regions,setup_materials,setup_state,setup_flag,             &
&            setup_flag_cell,setup_flag_shape,setup_flag_region,               &
&            setup_flag_background,setup_thermodynamic,setup_kinematic,        &
&            setup_indicator_types,setup_shapes_type,setup_IC_type,            &
&            setup_kinematic_background,setup_kinematic_region,                &
&            setup_thermodynamic_rationalise,setup_kinematic_rationalise,      &
&            setup_init_zero,setup_init_izero,setup_init_rzero,                &
&            setup_check_zero,setup_check_positive,setup_check_nonnegative,    &
&            region_shape,material_shape,inside_circle,inside_rectangle,       &
&            setup_print_shape,intersect,subdivide
  PUBLIC  :: setup_IC,setup_IC_read,setup_IC_print

CONTAINS

  SUBROUTINE setup_IC_read(iunit,io,sizes,error)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)    :: iunit
    TYPE(io_t),       INTENT(INOUT) :: io
    TYPE(sizes_t),    INTENT(INOUT) :: sizes
    TYPE(error_t),    INTENT(OUT)   :: error

    ! setup shapes
    CALL setup_shapes_type(iunit,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! setup regions and materials
    CALL setup_indicator_types(iunit,io,sizes,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! setup IC
    CALL setup_IC_type(iunit,sizes,error)
    IF (error%ierr.NE.SUCCESS) RETURN

  END SUBROUTINE setup_IC_read

  SUBROUTINE setup_IC(global,comm,sizes,timer,dh,error)

    ! Argument list
    TYPE(global_t),            INTENT(IN)    :: global
    TYPE(comm_t),              INTENT(IN)    :: comm
    TYPE(sizes_t),             INTENT(INOUT) :: sizes
    TYPE(timer_t),DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),             INTENT(OUT)   :: error

    ! Timer
    CALL timer_start(timer(TSETUPICID))

    ! setup regions
    CALL setup_regions(comm,sizes,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! setup materials
    CALL setup_materials(comm,sizes,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! setup state
    CALL setup_state(global,sizes,timer(TGETGEOMETRYIID),dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! Timer
    CALL timer_end(timer(TSETUPICID))

  END SUBROUTINE setup_IC

  SUBROUTINE setup_IC_print(io)

    ! Argument list
    TYPE(io_t),INTENT(IN) :: io
    ! Local
    INTEGER(KIND=ink) :: ii
    CHARACTER(LEN=22) :: str

    ! Indicators
    IF (ALLOCATED(regions)) THEN
      DO ii=1,SIZE(regions)
        SELECT CASE(regions(ii)%itype)
          CASE(MESH)
            WRITE(OSTREAM,'(a12,a10,54X,a7,31X,a18)') '  Region:   ',          &
&            io%sregion(regions(ii)%inumber), ' type: ','set from MESH     '
          CASE(BACKGROUND)
            WRITE(OSTREAM,'(a12,a10,54X,a7,31X,a18)') '  Region:   ',          &
&            io%sregion(regions(ii)%inumber), ' type: ','BACKGROUND        '
          CASE(CELL)
            WRITE(OSTREAM,'(a12,a10,54X,a7,31X,a6,i12)') '  Region:   ',       &
&            io%sregion(regions(ii)%inumber), ' type: ','CELL: ',              &
&            regions(ii)%param
          CASE(SHAPES)
            WRITE(OSTREAM,'(a12,a10,54X,a7,31X,a7,i11)') '  Region:   ',       &
&            io%sregion(regions(ii)%inumber), ' type: ','SHAPE: ',             &
&            regions(ii)%param
            CALL setup_print_shape(regions(ii)%param)
        END SELECT
      ENDDO
    ENDIF
    IF (ALLOCATED(materials)) THEN
      DO ii=1,SIZE(materials)
        SELECT CASE(materials(ii)%itype)
          CASE(MESH)
            WRITE(OSTREAM,'(a12,a10,54X,a7,31X,a18)') '  Material: ',          &
&            io%smaterial(materials(ii)%inumber),' type: ','set from MESH     '
          CASE(BACKGROUND)
            WRITE(OSTREAM,'(a12,a10,54X,a7,31X,a18)') '  Material: ',          &
&            io%smaterial(materials(ii)%inumber),' type: ','BACKGROUND        '
          CASE(CELL)
            WRITE(OSTREAM,'(a12,a10,54X,a7,31X,a6,i12)') '  Material: ',       &
&            io%smaterial(materials(ii)%inumber),' type: ','CELL: ',           &
&            materials(ii)%param
          CASE(REGION)
            WRITE(OSTREAM,'(a12,a10,54X,a7,31X,a8,a10)') '  Material: ',       &
&            io%smaterial(materials(ii)%inumber),' type: ','REGION: ',         &
&            io%sregion(materials(ii)%param)
          CASE(SHAPES)
            WRITE(OSTREAM,'(a12,a10,54X,a7,31X,a7,i11)') '  Material: ',       &
&            io%smaterial(materials(ii)%inumber),' type: ','SHAPE: ',          &
&            materials(ii)%param
            CALL setup_print_shape(materials(ii)%param)
        END SELECT
      ENDDO
    ENDIF

    ! Initial conditions
    IF (ALLOCATED(IC%thermodynamic)) THEN
      DO ii=1,SIZE(IC%thermodynamic)
        SELECT CASE(IC%thermodynamic(ii)%itype)
          CASE(REGION)
            WRITE(OSTREAM,'(a12,a10,a11,f16.10,a10,f16.10)') '  Region:   ',   &
&            io%sregion(IC%thermodynamic(ii)%param),' density = ',             &
&            IC%thermodynamic(ii)%density,' energy = ',                        &
&            IC%thermodynamic(ii)%energy
          CASE(MATERIAL)
            WRITE(OSTREAM,'(a12,a10,a11,f16.10,a10,f16.10)') '  Material: ',   &
&            io%smaterial(IC%thermodynamic(ii)%param),' density = ',           &
&            IC%thermodynamic(ii)%density,' energy = ',                        &
&            IC%thermodynamic(ii)%energy
        END SELECT
        SELECT CASE(IC%thermodynamic(ii)%ScaleEnergy)
          CASE(VOLUME)
            WRITE(OSTREAM,'(a27)') '    Energy scaled by volume'
          CASE(MASS)
            WRITE(OSTREAM,'(a25)') '    Energy scaled by mass'
        END SELECT
      ENDDO
    ENDIF
    IF (ALLOCATED(IC%kinematic)) THEN
      DO ii=1,SIZE(IC%kinematic)
        SELECT CASE(IC%kinematic(ii)%itype)
          CASE(BACKGROUND)
            str(:)='  Background:         '
          CASE(REGION)
            str(:)='  Region:   '//io%sregion(IC%kinematic(ii)%param)
        END SELECT
        SELECT CASE(IC%kinematic(ii)%igeom)
          CASE(RADIAL)
            WRITE(OSTREAM,'(a22,1X,a38,f16.10,a11,f16.10,a1,f16.10,a1)')       &
&            str,'  Radial velocity profile: velocity = ',                     &
&            IC%kinematic(ii)%dat(1),' centre = (',IC%kinematic(ii)%dat(2),',',&
&            IC%kinematic(ii)%dat(3),')'
          CASE(PLANAR)
            WRITE(OSTREAM,'(a22,1X,a39,f16.10,a1,f16.10,a1)')                  &
&            str,'  Planar velocity profile: velocity = (',                    &
&            IC%kinematic(ii)%dat(1),',',IC%kinematic(ii)%dat(2),')'
        END SELECT
      ENDDO
    ENDIF

  END SUBROUTINE setup_IC_print

  SUBROUTINE setup_regions(comm,sizes,dh,error)

    ! Argument list
    TYPE(comm_t),              INTENT(IN)    :: comm 
    TYPE(sizes_t),             INTENT(INOUT) :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),             INTENT(OUT)   :: error

    ! initialise region
    IF (.NOT.rmesh) CALL setup_init_zero(dh(ielregid)%dsize,dh(ielregid)%iaddr)

    ! set region
    apply_shape=>region_shape
    CALL setup_flag(sizes%nreg,regions,comm,sizes,iellocglobid,ielmatid,       &
&                   ielregid,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN
    NULLIFY(apply_shape)

    ! check mesh fully populated
    CALL setup_check_zero(dh(ielregid)%dsize,dh(ielregid)%iaddr,'region',error)
    IF (error%ierr.NE.SUCCESS) RETURN

  END SUBROUTINE setup_regions

  SUBROUTINE setup_materials(comm,sizes,dh,error)

    ! Argument list
    TYPE(comm_t),              INTENT(IN)    :: comm
    TYPE(sizes_t),             INTENT(INOUT) :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),             INTENT(OUT)   :: error

    ! initialise material
    IF (.NOT.mmesh) CALL setup_init_zero(dh(ielmatid)%dsize,dh(ielmatid)%iaddr)

    ! set material 
    apply_shape=>material_shape
    CALL setup_flag(sizes%nmat,materials,comm,sizes,iellocglobid,ielregid,     &
&                   ielmatid,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN
    NULLIFY(apply_shape)

    ! check mesh fully populated
    CALL setup_check_zero(dh(ielmatid)%dsize,dh(ielmatid)%iaddr,'material',    &
&                         error)
    IF (error%ierr.NE.SUCCESS) RETURN
    IF (sizes%ncp.GT.0_ink) THEN
      CALL setup_check_zero(sizes%ncp,dh(icpmatid)%iaddr,'material',error)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF

  END SUBROUTINE setup_materials

  SUBROUTINE setup_state(global,sizes,timer,dh,error)

    ! Argument list
    TYPE(global_t),            INTENT(IN)    :: global
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(timer_t),             INTENT(INOUT) :: timer
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),             INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink) :: ii

    ! initialise geometry
    CALL geometry_dr_getgeometry(sizes,timer,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! initialise thermodynamic variables from input
    DO ii=1,SIZE(IC%thermodynamic)
      SELECT CASE(IC%thermodynamic(ii)%itype)
        CASE(REGION)
          CALL setup_thermodynamic(dh(ielregid)%dsize,IC%thermodynamic(ii),    &
&                                  dh(ielregid)%iaddr,dh(elvolumeid)%raddr,    &
&                                  dh(eldensityid)%raddr,dh(elenergyid)%raddr)
        CASE(MATERIAL)
          CALL setup_thermodynamic(dh(ielmatid)%dsize,IC%thermodynamic(ii),    &
&                                  dh(ielmatid)%iaddr,dh(elvolumeid)%raddr,    &
&                                  dh(eldensityid)%raddr,dh(elenergyid)%raddr)
          IF (sizes%ncp.GT.0_ink) THEN
            CALL setup_thermodynamic(dh(icpmatid)%dsize,IC%thermodynamic(ii),  &
&                                    dh(icpmatid)%iaddr,dh(cpvolumeid)%raddr,  &
&                                    dh(cpdensityid)%raddr,                    &
&                                    dh(cpenergyid)%raddr)
          ENDIF
        CASE DEFAULT
          error%ierr=FAILURE
          error%serr="ERROR: unrecognised IC type"
          RETURN
      END SELECT
    ENDDO
    IF ((sizes%ncp.GT.0_ink).AND.ANY(IC%thermodynamic(:)%itype.EQ.REGION)) THEN
      CALL setup_thermodynamic_rationalise(sizes%nel,dh(eldensityid)%raddr,    &
&                                          dh(elenergyid)%raddr,sizes%nmx,     &
&                                          dh(imxfcpid)%iaddr,                 &
&                                          dh(imxncpid)%iaddr,                 &
&                                          dh(imxelid)%iaddr,sizes%ncp,        &
&                                          dh(icpmatid)%iaddr,                 &
&                                          dh(icpnextid)%iaddr,                &
&                                          dh(cpdensityid)%raddr,              &
&                                          dh(cpenergyid)%raddr,error)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF

    ! initialise kinematic variables from input
    CALL setup_init_zero(sizes%nnd2,dh(nduid)%raddr)
    CALL setup_init_zero(sizes%nnd2,dh(ndvid)%raddr)
    IF (ALLOCATED(IC%kinematic)) THEN
      CALL setup_kinematic(sizes,global,IC%kinematic,dh)
    ENDIF

    ! enforce boundary conditions
    CALL utils_dr_bc(global,sizes,nduid,ndvid,dh)

    ! check mesh is fully populated
    CALL setup_check_positive(dh(eldensityid)%dsize,dh(eldensityid)%raddr,     &
&                             dh(eldensityid)%dname,error)
    IF (error%ierr.NE.SUCCESS) RETURN
    CALL setup_check_nonnegative(dh(elenergyid)%dsize,dh(elenergyid)%raddr,    &
&                                dh(elenergyid)%dname,error)
    IF (error%ierr.NE.SUCCESS) RETURN
    IF (sizes%ncp.GT.0_ink) THEN
      CALL setup_check_positive(sizes%ncp,dh(cpdensityid)%raddr,               &
&                               dh(cpdensityid)%dname,error)
      IF (error%ierr.NE.SUCCESS) RETURN
      CALL setup_check_nonnegative(sizes%ncp,dh(cpenergyid)%raddr,             &
&                                  dh(cpenergyid)%dname,error)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF

    ! tidy up
    DEALLOCATE(IC%thermodynamic,STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: failed to deallocate IC%thermodynamic"
      RETURN
    ENDIF
    IF (ALLOCATED(IC%kinematic)) THEN
      DEALLOCATE(IC%kinematic,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate IC%kinematic"
        RETURN
      ENDIF
    ENDIF
    DEALLOCATE(forms,STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: failed to deallocate forms"
      RETURN
    ENDIF
    ! clean up materials
    DEALLOCATE(materials,STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%iout=HALT_SINGLE
      error%serr="ERROR: failed to deallocate materials"
      RETURN
    ENDIF
    ! clean up regions
    DEALLOCATE(regions,STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%iout=HALT_SINGLE
      error%serr="ERROR: failed to deallocate regions"
      RETURN
    ENDIF

  END SUBROUTINE setup_state

  SUBROUTINE setup_flag(nsize,indicator,comm,sizes,locglobid,testid,iflagid,dh,&
&                       error)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: nsize,locglobid,iflagid,   &
&                                                   testid
    TYPE(indicator_t),DIMENSION(:),INTENT(IN)    :: indicator
    TYPE(comm_t),                  INTENT(IN)    :: comm
    TYPE(sizes_t),                 INTENT(INOUT) :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),                 INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink)                      :: ii
    INTEGER(KIND=ink),DIMENSION(:),POINTER :: ip

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE
    NULLIFY(ip)

    ! initialise flag
    loop:DO ii=1,nsize
      SELECT CASE(indicator(ii)%itype)
        CASE(MESH)
          EXIT loop
        CASE(BACKGROUND)
          CALL setup_flag_background(dh(iflagid)%dsize,indicator(ii)%inumber,  &
&                                    dh(iflagid)%iaddr)
        CASE(CELL)
          IF (ASSOCIATED(dh(locglobid)%iaddr)) THEN
            ip=>dh(locglobid)%iaddr
          ELSE
            ip=>dh(testid)%iaddr
          ENDIF
          CALL setup_flag_cell(dh(iflagid)%dsize,indicator(ii)%inumber,        &
&                              indicator(ii)%param,comm%nproc,ip,              &
&                              dh(iflagid)%iaddr,error)
          IF (error%ierr.NE.SUCCESS) RETURN
        CASE(SHAPES)
          CALL setup_flag_shape(indicator(ii)%inumber,indicator(ii)%param,     &
&                               iflagid,sizes,dh,error)
          IF (error%ierr.NE.SUCCESS) RETURN
        CASE(REGION)
          CALL setup_flag_region(dh(iflagid)%dsize,indicator(ii)%inumber,      &
&                                indicator(ii)%param,dh(testid)%iaddr,         &
&                                dh(iflagid)%iaddr)
        CASE DEFAULT
          error%ierr=FAILURE
          error%serr="ERROR: unrecognised indicator type"
          RETURN
      END SELECT
    ENDDO loop

  END SUBROUTINE setup_flag

  SUBROUTINE setup_flag_background(nsize,itarget,iflag)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: nsize,itarget
    INTEGER(KIND=ink),DIMENSION(nsize),INTENT(OUT) :: iflag
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=1,nsize
      iflag(ii)=itarget
    ENDDO

  END SUBROUTINE setup_flag_background

  SUBROUTINE setup_flag_cell(nsize,itarget,iindex,nproc,itest,iflag,error)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)    :: nsize,itarget,iindex,  &
&                                                       nproc
    INTEGER(KIND=ink),DIMENSION(nsize),INTENT(IN)    :: itest
    INTEGER(KIND=ink),DIMENSION(nsize),INTENT(INOUT) :: iflag
    TYPE(error_t),                     INTENT(OUT)   :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set flag for cell index
    IF (nproc.GT.1_ink) THEN
      CALL setup_flag_region(nsize,itarget,iindex,itest,iflag)
    ELSE
      IF ((iindex.LT.LBOUND(iflag,DIM=1)).OR.                                  &
&         (iindex.GT.UBOUND(iflag,DIM=1))) THEN
        error%ierr=FAILURE
        error%serr="ERROR: incorrect cell index in setup_flag_cell"
        RETURN
      ENDIF
      iflag(iindex)=itarget
    ENDIF

  END SUBROUTINE setup_flag_cell

  SUBROUTINE setup_flag_shape(itarget,iform,iflagid,sizes,dh,error)

    ! Argument list
    INTEGER(KIND=ink),         INTENT(IN)    :: itarget,iform,iflagid
    TYPE(sizes_t),             INTENT(INOUT) :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),             INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink) :: nforms

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! check shapes exist
    nforms=SIZE(forms)
    IF (nforms.LT.1_ink) THEN
      IF ((iform.LT.1_ink).OR.(iform.GT.nforms)) THEN
        error%serr="ERROR: failed to find shape"
        RETURN
      ENDIF
    ENDIF

    ! Gather co-ordinates
    CALL utils_kn_cngather(sizes%nel,sizes%nnd,dh(ielndid)%iaddr,              &
&                          dh(ndxid)%raddr,dh(cnxid)%raddr)
    CALL utils_kn_cngather(sizes%nel,sizes%nnd,dh(ielndid)%iaddr,              &
&                          dh(ndyid)%raddr,dh(cnyid)%raddr)

    ! set flag
    NULLIFY(inside)
    SELECT CASE(forms(iform)%itype)
      CASE(CIRCLE)
        inside=>inside_circle
      CASE(RECTANGLE)
        inside=>inside_rectangle
      CASE DEFAULT
        error%ierr=FAILURE
        error%serr="ERROR: failed to find shape type"
        RETURN
    END SELECT
    CALL apply_shape(itarget,forms(iform)%param,iflagid,sizes,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! tidy up
    NULLIFY(inside)

  END SUBROUTINE setup_flag_shape

  SUBROUTINE setup_flag_region(nsize,itarget,iindex,itest,iflag)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)    :: nsize,itarget,iindex
    INTEGER(KIND=ink),DIMENSION(nsize),INTENT(IN)    :: itest
    INTEGER(KIND=ink),DIMENSION(nsize),INTENT(INOUT) :: iflag
    ! Local
    INTEGER(KIND=ink) :: ii

    ! set flag for indices matching test
    DO ii=1,nsize
      IF (itest(ii).EQ.iindex) iflag(ii)=itarget
    ENDDO

  END SUBROUTINE setup_flag_region

  SUBROUTINE setup_thermodynamic(nsize,IC,iflag,volumes,densities,energies)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: nsize
    TYPE(thermodynamic_t),                 INTENT(IN)    :: IC
    INTEGER(KIND=ink),    DIMENSION(nsize),INTENT(IN)    :: iflag
    REAL(KIND=rlk),       DIMENSION(nsize),INTENT(IN)    :: volumes
    REAL(KIND=rlk),       DIMENSION(nsize),INTENT(INOUT) :: densities,energies
    ! Local
    INTEGER(KIND=ink) :: ii

    ! initialise density and energy
    DO ii=1,nsize
      IF (iflag(ii).EQ.IC%param) THEN
        densities(ii)=IC%density
        energies(ii)=IC%energy
      ENDIF
    ENDDO

    ! scale energy if necessary
    IF (IC%ScaleEnergy.EQ.VOLUME) THEN
      DO ii=1,nsize
        IF (iflag(ii).EQ.IC%param) THEN
          energies(ii)=energies(ii)/volumes(ii)
        ENDIF
      ENDDO
    ELSEIF (IC%ScaleEnergy.EQ.MASS) THEN
      DO ii=1,nsize
        IF (iflag(ii).EQ.IC%param) THEN
          energies(ii)=energies(ii)/(densities(ii)*volumes(ii))
        ENDIF
      ENDDO
    ENDIF

  END SUBROUTINE setup_thermodynamic

  SUBROUTINE setup_kinematic(sizes,global,kinematic,dh)

    ! Argument list
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    TYPE(global_t),                INTENT(IN)    :: global
    TYPE(kinematic_t),DIMENSION(:),INTENT(IN)    :: kinematic
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    ! Local
    INTEGER(KIND=ink) :: ii

    ! set velocity field
    IF (ANY(kinematic(:)%itype.EQ.REGION)) THEN
      CALL setup_init_zero(sizes%nnd2,dh(rscratch11id)%raddr)
    ENDIF
    DO ii=1,SIZE(kinematic)
      SELECT CASE(kinematic(ii)%itype)
        CASE(BACKGROUND)
          CALL setup_kinematic_background(sizes%nnd,kinematic(ii),             &
&                                         dh(ndxid)%raddr,dh(ndyid)%raddr,     &
&                                         dh(nduid)%raddr,dh(ndvid)%raddr)
        CASE(REGION)
          CALL setup_kinematic_region(sizes%nel1,sizes%nnd1,kinematic(ii),     &
&                                     dh(ielregid)%iaddr,dh(ielndid)%iaddr,    &
&                                     dh(cnwtid)%raddr,dh(cnxid)%raddr,        &
&                                     dh(cnyid)%raddr,dh(nduid)%raddr,         &
&                                     dh(ndvid)%raddr,dh(rscratch11id)%raddr)
      END SELECT
    ENDDO
    IF (ANY(kinematic(:)%itype.EQ.REGION)) THEN
      CALL setup_kinematic_rationalise(sizes%nnd,global%zerocut,               &
&                                      dh(rscratch11id)%raddr,dh(nduid)%raddr, &
&                                      dh(ndvid)%raddr)
    ENDIF

  END SUBROUTINE setup_kinematic

  SUBROUTINE setup_kinematic_background(nsize,kinematic,xx,yy,uu,vv)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: nsize
    TYPE(kinematic_t),                 INTENT(IN)  :: kinematic
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(IN)  :: xx,yy
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(OUT) :: uu,vv
    ! Local
    INTEGER(KIND=ink) :: ii
    REAL(KIND=rlk)    :: rv

    SELECT CASE(kinematic%igeom)
      CASE(RADIAL)
        DO ii=1,nsize
          rv=1.0_rlk/SQRT((xx(ii)-kinematic%dat(2))**2+                        &
&                         (yy(ii)-kinematic%dat(3))**2)
          uu(ii)=kinematic%dat(1)*xx(ii)*rv
          vv(ii)=kinematic%dat(1)*yy(ii)*rv
        ENDDO
      CASE(PLANAR)
        DO ii=1,nsize
          uu(ii)=kinematic%dat(1)
          vv(ii)=kinematic%dat(2)
        ENDDO
    END SELECT

  END SUBROUTINE setup_kinematic_background

  SUBROUTINE setup_kinematic_region(nel,nnd,kinematic,ielreg,ielnd,cnwt,cnx,   &
&                                   cny,uu,vv,wt)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: nel,nnd
    TYPE(kinematic_t),                     INTENT(IN)    :: kinematic
    INTEGER(KIND=ink),DIMENSION(nel),      INTENT(IN)    :: ielreg
    INTEGER(KIND=ink),DIMENSION(NCORN,nel),INTENT(IN)    :: ielnd
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)    :: cnwt,cnx,cny
    REAL(KIND=rlk),   DIMENSION(nnd),      INTENT(INOUT) :: uu,vv,wt
    ! Local
    INTEGER(KIND=ink)              :: iel,ind,ic
    REAL(KIND=rlk)                 :: rv
    REAL(KIND=rlk),DIMENSION(NDIM) :: point

    SELECT CASE(kinematic%igeom)
      CASE(RADIAL)
        DO iel=1,nel
          IF (ielreg(iel).EQ.kinematic%param) THEN
            point=geometry_kn_getcentroid(NCORN,cnx(:,iel),cny(:,iel))
            rv=1.0_rlk/SQRT((point(1)-kinematic%dat(2))**2+                    &
&                           (point(2)-kinematic%dat(3))**2)
            DO ic=1,NCORN
              ind=ielnd(ic,iel)
              uu(ind)=uu(ind)+cnwt(ic,iel)*point(1)*rv*kinematic%dat(1)
              vv(ind)=vv(ind)+cnwt(ic,iel)*point(2)*rv*kinematic%dat(2)
              wt(ind)=wt(ind)+cnwt(ic,iel)
            ENDDO
          ENDIF
        ENDDO
      CASE(PLANAR)
        DO iel=1,nel
          IF (ielreg(iel).EQ.kinematic%param) THEN
            DO ic=1,NCORN
              ind=ielnd(ic,iel)
              uu(ind)=uu(ind)+cnwt(ic,iel)*kinematic%dat(1)
              vv(ind)=vv(ind)+cnwt(ic,iel)*kinematic%dat(2)
              wt(ind)=wt(ind)+cnwt(ic,iel)
            ENDDO
          ENDIF
        ENDDO
      END SELECT

  END SUBROUTINE setup_kinematic_region

  SUBROUTINE setup_thermodynamic_rationalise(nel,eldensity,elenergy,nmx,imxfcp,&
&                                            imxncp,imxel,ncp,icpmat,icpnext,  &
&                                            cpdensity,cpenergy,error)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nel,nmx,ncp
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(IN)    :: eldensity,elenergy
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(IN)    :: imxfcp,imxncp,imxel
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(IN)    :: icpmat,icpnext
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(INOUT) :: cpdensity,cpenergy
    TYPE(error_t),                   INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink)                                   :: iel,imx,icp,imat,ii,jj
    INTEGER(KIND=ink),DIMENSION(SIZE(IC%thermodynamic)) :: ilocal

    ! set materials used for thermodynamic initialisation
    jj=0_ink
    DO ii=1,SIZE(IC%thermodynamic)
      IF (IC%thermodynamic(ii)%itype.EQ.MATERIAL) THEN
        jj=jj+1_ink
        ilocal(jj)=IC%thermodynamic(ii)%param
      ENDIF
    ENDDO

    ! find and set thermodynamic values for components from 
    ! non-material based thermodynamic input
    DO imx=1,nmx
      icp=imxfcp(imx)
      iel=imxel(imx)
      DO ii=1,imxncp(imx)
        imat=icpmat(icp)
        IF (.NOT.ANY(ilocal(1:jj).EQ.imat)) THEN
          SELECT CASE(materials(imat)%itype)
            CASE(REGION,BACKGROUND) 
              cpdensity(icp)=eldensity(iel)
              cpenergy(icp)=elenergy(iel)
            CASE DEFAULT
              error%ierr=FAILURE
              error%iout=HALT_SINGLE
              error%serr="ERROR: failed to rationalise thermodynamic IC due to"&
&              //" incorrect material type"
          END SELECT
        ENDIF
        icp=icpnext(icp)
      ENDDO
    ENDDO

  END SUBROUTINE setup_thermodynamic_rationalise

  SUBROUTINE setup_kinematic_rationalise(nsize,cutoff,wt,uu,vv)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)    :: nsize
    REAL(KIND=rlk),                    INTENT(IN)    :: cutoff
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(IN)    :: wt
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(INOUT) :: uu,vv
    ! Local
    INTEGER(KIND=ink) :: ii
    REAL(KIND=rlk)    :: rv

    DO ii=1,nsize
      IF (ABS(wt(ii)).LT.cutoff) THEN
        uu(ii)=0.0_rlk
        vv(ii)=0.0_rlk
      ELSE
        rv=1.0_rlk/wt(ii)
        uu(ii)=uu(ii)*rv
        vv(ii)=vv(ii)*rv
      ENDIF
    ENDDO

  END SUBROUTINE setup_kinematic_rationalise

  SUBROUTINE setup_indicator_types(iunit,io,sizes,error)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)    :: iunit
    TYPE(io_t),       INTENT(INOUT) :: io
    TYPE(sizes_t),    INTENT(INOUT) :: sizes
    TYPE(error_t),    INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink)                 :: ii,ip
    LOGICAL(KIND=lok)                 :: zflag
    CHARACTER(LEN=SLEN)               :: str
    CHARACTER(LEN=NLEN),DIMENSION(:), ALLOCATABLE :: sarray
    CHARACTER(LEN=SLEN),DIMENSION(LN)             :: region_type,material_type
    CHARACTER(LEN=NLEN),DIMENSION(LN)             :: region_name,material_name    
    INTEGER(KIND=ink),  DIMENSION(LN)             :: region_value,             &
&                                                    material_value

    NAMELIST /INDICATORS/ region_type,region_value,region_name,material_type,  &
&                         material_name,material_value

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set defaults
    region_type(:)="UNKNOWN"
    region_value(:)=-1_ink
    CALL io_cf_get(io=io,sregion=sarray,error=error)
    IF (error%ierr.NE.SUCCESS) RETURN
    region_name(:)=sarray(1)
    DEALLOCATE(sarray,STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: failed to deallocate sarray"
      RETURN
    ENDIF
    material_type(:)="UNKNOWN"
    material_value(:)=-1_ink
    CALL io_cf_get(io=io,smaterial=sarray,error=error)
    IF (error%ierr.NE.SUCCESS) RETURN
    material_name(:)=sarray(1)
    DEALLOCATE(sarray,STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: failed to deallocate sarray"
      RETURN
    ENDIF

    ! read namelist
    zflag=utils_kn_findstr('INDICATORS',iunit,SLEN)
    IF (zflag) THEN
      REWIND(UNIT=iunit)
      READ(UNIT=iunit,NML=INDICATORS,IOSTAT=error%ierr,IOMSG=str)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: "//TRIM(str)
        RETURN
      ENDIF
    ENDIF

    ! rationalise
    DO ii=1,LN
      CALL utils_kn_convupper(region_type(ii))
    ENDDO
    sizes%nreg=COUNT(region_type(:).NE."UNKNOWN")
    IF (sizes%nreg.EQ.LN) THEN
      error%ierr=FAILURE
      error%serr="ERROR: Too many regions for input parser"
      RETURN
    ENDIF
    IF (sizes%nreg.LT.1_ink) THEN
      error%ierr=FAILURE
      error%serr="ERROR: No regions defined"
      RETURN
    ENDIF
    IF (ANY(region_type(1:sizes%nreg).EQ."MESH")) THEN
      IF (.NOT.ALL(region_type(1:sizes%nreg).EQ."MESH")) THEN
        error%ierr=FAILURE
        error%serr="ERROR: Cannot mix region type MESH with other region types"
        RETURN
      ENDIF
      rmesh=.TRUE._lok
    ENDIF
    IF (ANY(region_type(1:sizes%nreg).EQ."REGION")) THEN
      error%ierr=FAILURE
      error%serr="ERROR: Cannot set region type to REGION"
      RETURN
    ENDIF
    IF ((ANY(region_type(1:sizes%nreg).EQ."CELL").OR.                          &
&        ANY(region_type(1:sizes%nreg).EQ."SHAPE")).AND.                       &
&       (.NOT.ANY(region_type(1:sizes%nreg).EQ."BACKGROUND"))) THEN
      error%ierr=FAILURE
      error%serr="ERROR: region types CELL, SHAPE require a BACKGROUND type"
      RETURN
    ENDIF
    IF (COUNT(region_type(1:sizes%nreg).EQ."BACKGROUND").GT.1_ink) THEN
      error%ierr=FAILURE
      error%serr="ERROR: Only one region of type BACKGROUND permitted"
      RETURN
    ENDIF
    DO ii=1,sizes%nreg
      SELECT CASE(region_type(ii))
        CASE("MESH","BACKGROUND")
          CYCLE
        CASE("CELL")
          IF (region_value(ii).LT.1_ink) THEN
            error%ierr=FAILURE
            error%serr="ERROR: cell value for region type out of range"
            RETURN
          ENDIF
        CASE("MATERIAL")
          IF (.NOT.((region_value(ii).GE.1_ink).AND.                           &
&                   (region_value(ii).LE.sizes%nmat))) THEN
            error%ierr=FAILURE
            error%serr="ERROR: material value for region type out of range"
            RETURN
          ENDIF
        CASE("SHAPE")
          IF ((region_value(ii).LT.1_ink).OR.                                  &
&             (region_value(ii).GT.SIZE(forms))) THEN
            error%ierr=FAILURE
            error%serr="ERROR: shape value for region type out of range"
            RETURN
          ENDIF
        CASE DEFAULT
          error%ierr=FAILURE
          error%serr="ERROR: unrecognised value of region_type"
          RETURN
      END SELECT
      IF (LEN_TRIM(region_name(ii)).GT.NLEN) THEN
        error%ierr=FAILURE
        error%serr="ERROR: region name too long"
        RETURN
      ENDIF
    ENDDO
    DO ii=1,LN
      CALL utils_kn_convupper(material_type(ii))
    ENDDO
    sizes%nmat=COUNT(material_type(:).NE."UNKNOWN")
    IF (sizes%nmat.EQ.LN) THEN
      error%ierr=FAILURE
      error%serr="ERROR: Too many materials for input parser"
      RETURN
    ENDIF
    IF (sizes%nmat.LT.1_ink) THEN
      error%ierr=FAILURE
      error%serr="ERROR: No materials defined"
      RETURN
    ENDIF
    IF (ANY(material_type(1:sizes%nmat).EQ."MESH")) THEN
      IF (.NOT.ALL(material_type(1:sizes%nmat).EQ."MESH")) THEN
        error%ierr=FAILURE
        error%serr="ERROR: Cannot mix material type MESH with other material " &
&                  //"types"
        RETURN
      ENDIF
      mmesh=.TRUE._lok
    ENDIF
    IF (ANY(material_type(1:sizes%nmat).EQ."MATERIAL")) THEN
      error%ierr=FAILURE
      error%serr="ERROR: Cannot set material type to MATERIAL"
      RETURN
    ENDIF
    IF ((ANY(material_type(1:sizes%nmat).EQ."CELL").OR.                        &
&        ANY(material_type(1:sizes%nmat).EQ."SHAPE")).AND.                     &
&       (.NOT.ANY(material_type(1:sizes%nmat).EQ."BACKGROUND"))) THEN
      error%ierr=FAILURE
      error%serr="ERROR: material types CELL, SHAPE require a BACKGROUND type"
      RETURN
    ENDIF
    IF (COUNT(material_type(1:sizes%nmat).EQ."BACKGROUND").GT.1_ink) THEN
      error%ierr=FAILURE
      error%serr="ERROR: Only one material of type BACKGROUND permitted"
      RETURN
    ENDIF
    DO ii=1,sizes%nmat
      SELECT CASE(material_type(ii))
        CASE("MESH","BACKGROUND")
          CYCLE
        CASE("CELL")
          IF (material_value(ii).LT.1_ink)THEN
            error%ierr=FAILURE
            error%serr="ERROR: cell value for material type out of range"
            RETURN
          ENDIF
        CASE("REGION")
          IF (.NOT.((material_value(ii).GE.1_ink).AND.                         &
&                   (material_value(ii).LE.sizes%nreg))) THEN
            error%ierr=FAILURE
            error%serr="ERROR: region value for material type out of range"
            RETURN
          ENDIF
        CASE("SHAPE")
          IF ((material_value(ii).LT.1_ink).OR.                                &
&             (material_value(ii).GT.SIZE(forms))) THEN
            error%ierr=FAILURE
            error%serr="ERROR: shape value for material type out of range"
            RETURN
          ENDIF
        CASE DEFAULT
          error%ierr=FAILURE
          error%serr="ERROR: unrecognised value of material_type"
          RETURN
      END SELECT
      IF (LEN_TRIM(material_name(ii)).GT.NLEN) THEN
        error%ierr=FAILURE
        error%serr="ERROR: material name too long"
        RETURN
      ENDIF
    ENDDO

    ! set types
    ALLOCATE(regions(sizes%nreg),STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: failed to allocate regions"
      RETURN
    ENDIF
    ip=1_ink
    DO ii=1,sizes%nreg
      IF (region_type(ii).EQ."BACKGROUND") THEN
        regions(ip)%itype=BACKGROUND
        regions(ip)%param=region_value(ii)
        regions(ip)%inumber=ii
        ip=ip+1_ink
      ENDIF
    ENDDO
    DO ii=1,sizes%nreg
      SELECT CASE(region_type(ii))
        CASE("CELL")
          regions(ip)%itype=CELL
          regions(ip)%param=region_value(ii)
          regions(ip)%inumber=ii
          ip=ip+1_ink
        CASE("MESH")
          regions(ip)%itype=MESH
          regions(ip)%param=region_value(ii)
          regions(ip)%inumber=ii
          ip=ip+1_ink
        CASE("MATERIAL")
          regions(ip)%itype=MATERIAL
          regions(ip)%param=region_value(ii)
          regions(ip)%inumber=ii
          ip=ip+1_ink
      END SELECT
    ENDDO
    DO ii=1,sizes%nreg
      IF (region_type(ii).EQ."SHAPE") THEN
        regions(ip)%itype=SHAPES
        regions(ip)%param=region_value(ii)
        regions(ip)%inumber=ii
        ip=ip+1_ink
      ENDIF
    ENDDO
    ALLOCATE(materials(sizes%nmat),STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: failed to allocate regions"
      RETURN
    ENDIF
    ip=1_ink
    DO ii=1,sizes%nmat
      IF (material_type(ii).EQ."BACKGROUND") THEN
        materials(ip)%itype=BACKGROUND
        materials(ip)%param=material_value(ii)
        materials(ip)%inumber=ii
        ip=ip+1_ink
      ENDIF
    ENDDO
    DO ii=1,sizes%nmat
      SELECT CASE(material_type(ii))
        CASE("CELL")
          materials(ip)%itype=CELL
          materials(ip)%param=material_value(ii)
          materials(ip)%inumber=ii
          ip=ip+1_ink
        CASE("MESH")
          materials(ip)%itype=MESH
          materials(ip)%param=material_value(ii)
          materials(ip)%inumber=ii
          ip=ip+1_ink
        CASE("REGION")
          materials(ip)%itype=REGION
          materials(ip)%param=material_value(ii)
          materials(ip)%inumber=ii
          ip=ip+1_ink
      END SELECT
    ENDDO
    DO ii=1,sizes%nmat
      IF (material_type(ii).EQ."SHAPE") THEN
        materials(ip)%itype=SHAPES
        materials(ip)%param=material_value(ii)
        materials(ip)%inumber=ii
      ENDIF
    ENDDO

    ! set io
    ALLOCATE(sarray(sizes%nreg),STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: failed to allocate sarray"
      RETURN
    ENDIF
    sarray(:)=region_name(1:sizes%nreg)
    CALL io_cf_set(io,sregion=sarray,error=error)
    IF (error%ierr.NE.SUCCESS) RETURN
    ALLOCATE(sarray(sizes%nmat),STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: failed to allocate sarray"
      RETURN
    ENDIF
    sarray(:)=material_name(1:sizes%nmat)
    CALL io_cf_set(io,smaterial=sarray,error=error)
    IF (error%ierr.NE.SUCCESS) RETURN

  END SUBROUTINE setup_indicator_types

  SUBROUTINE setup_shapes_type(iunit,error)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)  :: iunit
    TYPE(error_t),    INTENT(OUT) :: error
    ! Local
    CHARACTER(LEN=SLEN),DIMENSION(LN)   :: shape_type
    REAL(KIND=rlk),     DIMENSION(LN,5) :: shape_param
    CHARACTER(LEN=SLEN)                 :: str
    LOGICAL(KIND=lok)                   :: zflag
    INTEGER(KIND=ink)                   :: ii,is

    NAMELIST /SHAPES/ shape_type,shape_param

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set defaults
    shape_type(:)="UNKNOWN"
    shape_param(:,:)=0.0_rlk

    ! read namelist
    zflag=utils_kn_findstr('SHAPES',iunit,SLEN)
    IF (zflag) THEN
      REWIND(UNIT=iunit)
      READ(UNIT=iunit,NML=SHAPES,IOSTAT=error%ierr,IOMSG=str)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: "//TRIM(str)
        RETURN
      ENDIF
    ENDIF

    ! rationalise
    DO ii=1,LN
      CALL utils_kn_convupper(shape_type(ii))
    ENDDO
    is=COUNT(shape_type(:).NE."UNKNOWN")
    IF (is.EQ.LN) THEN
      error%ierr=FAILURE
      error%serr="ERROR: Too many shapes specified for input parser"
      RETURN
    ENDIF
    DO ii=1,LN
      SELECT CASE(shape_type(ii))
        CASE("RECTANGLE","UNKNOWN")
        CASE("CIRCLE")
          IF (shape_param(ii,3).LT.0.0_rlk) THEN
            error%ierr=FAILURE
            error%serr="ERROR: shape radius < 0"
            RETURN
          ENDIF
        CASE DEFAULT
          error%ierr=FAILURE
          error%serr="ERROR: unrecognised shape type"
          RETURN
      END SELECT
      IF ((shape_param(ii,5).LT.0.0_rlk).OR.                                   &
&         (shape_param(ii,5).GT.1.0_rlk)) THEN
        error%ierr=FAILURE
        error%serr="ERROR: shape fixed fraction < 0 or > 1"
        RETURN
      ENDIF
    ENDDO

    ! set type
    ALLOCATE(forms(is),STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: failed to allocate forms"
      RETURN
    ENDIF
    DO ii=1,is
      forms(ii)%param(:)=shape_param(ii,:)
      SELECT CASE(shape_type(ii))
        CASE("RECTANGLE")
          forms(ii)%itype=RECTANGLE
        CASE("CIRCLE")
          forms(ii)%itype=CIRCLE
          forms(ii)%param(4)=0.0_rlk
        CASE DEFAULT
          forms(ii)%itype=ZERO
      END SELECT
    ENDDO

  END SUBROUTINE setup_shapes_type

  SUBROUTINE setup_IC_type(iunit,sizes,error)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)  :: iunit
    TYPE(sizes_t),    INTENT(IN)  :: sizes
    TYPE(error_t),    INTENT(OUT) :: error
    ! Local
    INTEGER(KIND=ink)                   :: ii,nICt,nICk
    LOGICAL(KIND=lok)                   :: zflag
    CHARACTER(LEN=SLEN),DIMENSION(LN)   :: thermodynamic_type,                 &
&                                          thermodynamic_energy_scale,         &
&                                          kinematic_type,kinematic_geometry
    INTEGER(KIND=ink),  DIMENSION(LN)   :: thermodynamic_value,kinematic_value
    REAL(KIND=rlk),     DIMENSION(LN)   :: density,energy
    CHARACTER(LEN=SLEN)                 :: str
    REAL(KIND=rlk),     DIMENSION(3,LN) :: kinematic_param

    NAMELIST /INITIAL_CONDITIONS/ thermodynamic_type,thermodynamic_value,      &
&                                 thermodynamic_energy_scale,density,energy,   &
&                                 kinematic_type,kinematic_geometry,           &
&                                 kinematic_value,kinematic_param

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set defaults
    thermodynamic_type(:)="UNKNOWN"
    thermodynamic_value(:)=-1_ink
    thermodynamic_energy_scale(:)="UNKNOWN"
    density(:)=-1.0_rlk
    energy(:)=-1.0_rlk
    kinematic_type(:)="UNKNOWN"
    kinematic_geometry(:)="UNKNOWN"
    kinematic_value(:)=-1_ink
    kinematic_param(:,:)=0.0_rlk

    ! read namelist
    zflag=utils_kn_findstr('INITIAL_CONDITIONS',iunit,SLEN)
    IF (zflag) THEN
      REWIND(UNIT=iunit)
      READ(UNIT=iunit,NML=INITIAL_CONDITIONS,IOSTAT=error%ierr,IOMSG=str)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: "//TRIM(str)
        RETURN
      ENDIF
    ENDIF

    ! rationalise
    DO ii=1,LN
      CALL utils_kn_convupper(thermodynamic_type(ii))
      CALL utils_kn_convupper(thermodynamic_energy_scale(ii))
      CALL utils_kn_convupper(kinematic_type(ii))
      CALL utils_kn_convupper(kinematic_geometry(ii))
    ENDDO
    nICt=COUNT(thermodynamic_type(:).NE."UNKNOWN")
    IF (nICt.EQ.LN) THEN
      error%ierr=FAILURE
      error%serr="ERROR: Too many initial conditions for input parser"
      RETURN
    ENDIF
    IF (nICt.LT.1_ink) THEN
      error%ierr=FAILURE
      error%serr="ERROR: No initial conditions defined"
      RETURN
    ENDIF
    thermodynamic_value(:)=PACK(thermodynamic_value(:),                        &
&                               thermodynamic_type(:).NE."UNKNOWN")
    thermodynamic_energy_scale(:)=PACK(thermodynamic_energy_scale(:),          &
&                                      thermodynamic_type(:).NE."UKNOWN")
    density(:)=PACK(density(:),thermodynamic_type(:).NE."UNKNOWN")
    energy(:)=PACK(energy(:),thermodynamic_type(:).NE."UNKNOWN")
    thermodynamic_type(:)=PACK(thermodynamic_type(:),                          &
&                              thermodynamic_type(:).NE."UNKNOWN")
    DO ii=1,nICt
      SELECT CASE(thermodynamic_type(ii))
        CASE("REGION")
          IF (.NOT.((thermodynamic_value(ii).GE.1_ink).AND.                    &
&                   (thermodynamic_value(ii).LE.sizes%nreg))) THEN
            error%ierr=FAILURE
            error%serr="ERROR: thermodynamic value out of range"
            RETURN
          ENDIF
        CASE("MATERIAL")
          IF (.NOT.((thermodynamic_value(ii).GE.1_ink).AND.                    &
&                   (thermodynamic_value(ii).LE.sizes%nmat))) THEN
            error%ierr=FAILURE
            error%serr="ERROR: thermodynamic value out of range"
            RETURN
          ENDIF
        CASE DEFAULT
          error%ierr=FAILURE
          error%serr="ERROR: unrecognised thermodynamic type"
          RETURN
      END SELECT
      SELECT CASE(thermodynamic_energy_scale(ii))
        CASE("VOLUME","MASS","UNKNOWN")
        CASE DEFAULT
          error%ierr=FAILURE
          error%serr="ERROR: unrecognised thermodynamic energy scaling"
          RETURN
      END SELECT
      IF (density(ii).LE.0.0_rlk) THEN
        error%ierr=FAILURE
        error%serr="ERROR: density <= 0"
        RETURN
      ENDIF
      IF (energy(ii).LT.0.0_rlk) THEN
        error%ierr=FAILURE
        error%serr="ERROR: energy < 0"
        RETURN
      ENDIF
    ENDDO
    nICk=COUNT(kinematic_type(:).NE."UNKNOWN")
    IF (nICk.EQ.LN) THEN
      error%ierr=FAILURE
      error%serr="ERROR: Too many initial conditions for input parser"
      RETURN
    ENDIF
    IF (nICk.GT.0_ink) THEN
      ii=COUNT(kinematic_type(:).EQ."BACKGROUND")
      IF (ii.GT.1_ink) THEN
        error%ierr=FAILURE
        error%serr="ERROR: More than 1 background kinematic type specified"
        RETURN
      ENDIF
      kinematic_geometry(:)=PACK(kinematic_geometry(:),                        &
&                                kinematic_type(:).NE."UNKNOWN")
      kinematic_value(:)=PACK(kinematic_value(:),                              &
&                             kinematic_type(:).NE."UNKNOWN")
      DO ii=1,3
        kinematic_param(ii,:)=PACK(kinematic_param(ii,:),                      &
&                                  kinematic_type(:).NE."UNKNOWN")
      ENDDO
      kinematic_type(:)=PACK(kinematic_type(:),kinematic_type(:).NE."UNKNOWN")
      DO ii=1,nICk
        SELECT CASE(kinematic_type(ii))
          CASE("BACKGROUND")
          CASE("REGION")
            IF (.NOT.((kinematic_value(ii).GE.1_ink).AND.                      &
&                     (kinematic_value(ii).LE.sizes%nreg))) THEN
              error%ierr=FAILURE
              error%serr="ERROR: kinematic value out of range"
              RETURN
            ENDIF
          CASE DEFAULT
            error%ierr=FAILURE
            error%serr="ERROR: unrecognised kinematic type"
            RETURN
        END SELECT
        SELECT CASE(kinematic_geometry(ii))
          CASE("RADIAL","PLANAR")
          CASE DEFAULT
            error%ierr=FAILURE
            error%serr="ERROR: unrecognised kinematic geometry"
        END SELECT
      ENDDO
    ENDIF

    ! set types
    ALLOCATE(IC%thermodynamic(nICt),STAT=error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: failed to allocate IC%thermodynamic"
      RETURN
    ENDIF
    DO ii=1,nICt
      SELECT CASE(thermodynamic_type(ii))
        CASE("REGION")
          IC%thermodynamic(ii)%itype=REGION
        CASE("MATERIAL")
          IC%thermodynamic(ii)%itype=MATERIAL
      END SELECT
      IC%thermodynamic(ii)%param=thermodynamic_value(ii)
      SELECT CASE(thermodynamic_energy_scale(ii))
        CASE("VOLUME")
          IC%thermodynamic(ii)%ScaleEnergy=VOLUME
        CASE("MASS")
          IC%thermodynamic(ii)%ScaleEnergy=MASS
        CASE DEFAULT
          IC%thermodynamic(ii)%ScaleEnergy=ZERO
      END SELECT
      IC%thermodynamic(ii)%density=density(ii)
      IC%thermodynamic(ii)%energy=energy(ii)
    ENDDO
    IF (nICk.GT.0_ink) THEN
      ALLOCATE(IC%kinematic(nICk),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate IC%kinematic"
        RETURN
      ENDIF
      DO ii=1,nICk
        SELECT CASE(kinematic_type(ii))
          CASE("BACKGROUND")
            IC%kinematic(ii)%itype=BACKGROUND
          CASE("REGION")
            IC%kinematic(ii)%itype=REGION
        END SELECT
        IC%kinematic(ii)%dat(:)=kinematic_param(:,ii)
        SELECT CASE(kinematic_geometry(ii))
          CASE("RADIAL")
            IC%kinematic(ii)%igeom=RADIAL
          CASE("PLANAR")
            IC%kinematic(ii)%igeom=PLANAR
            IC%kinematic(ii)%dat(3)=0.0_rlk
        END SELECT
        IC%kinematic(ii)%param=kinematic_value(ii)
      ENDDO
    ENDIF

  END SUBROUTINE setup_IC_type

  SUBROUTINE setup_init_izero(nsize,iarray)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: nsize
    INTEGER(KIND=ink),DIMENSION(nsize),INTENT(OUT) :: iarray
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=1,nsize
      iarray(ii)=0_ink
    ENDDO

  END SUBROUTINE setup_init_izero

  SUBROUTINE setup_init_rzero(nsize,rarray)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: nsize
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(OUT) :: rarray
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=1,nsize
      rarray(ii)=0.0_rlk
    ENDDO

  END SUBROUTINE setup_init_rzero

  SUBROUTINE setup_check_zero(nsize,iarray,stype,error)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: nsize
    INTEGER(KIND=ink),DIMENSION(nsize),INTENT(IN)  :: iarray
    CHARACTER(LEN=*),                  INTENT(IN)  :: stype
    TYPE(error_t),                     INTENT(OUT) :: error
    ! Local
    INTEGER(KIND=ink) :: ii

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! check
    DO ii=1,nsize
      IF (iarray(ii).EQ.0_ink) error%ierr=FAILURE
    ENDDO

    IF (error%ierr.EQ.FAILURE) THEN
      error%serr="ERROR: "//TRIM(stype)//" not populated"
      RETURN
    ENDIF

  END SUBROUTINE setup_check_zero

  SUBROUTINE setup_check_positive(nsize,array,stype,error)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: nsize
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(IN)  :: array
    CHARACTER(LEN=*),                  INTENT(IN)  :: stype
    TYPE(error_t),                     INTENT(OUT) :: error
    ! Local
    INTEGER(KIND=ink) :: ii

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! check
    DO ii=1,nsize
      IF (array(ii).LE.0.0_rlk) error%ierr=FAILURE
    ENDDO

    IF (error%ierr.EQ.FAILURE) THEN
      error%serr="ERROR: "//TRIM(stype)//" not populated"
      RETURN
    ENDIF

  END SUBROUTINE setup_check_positive

  SUBROUTINE setup_check_nonnegative(nsize,array,stype,error)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: nsize
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(IN)  :: array 
    CHARACTER(LEN=*),                  INTENT(IN)  :: stype
    TYPE(error_t),                     INTENT(OUT) :: error
    ! Local
    INTEGER(KIND=ink) :: ii

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! check
    DO ii=1,nsize
      IF (array(ii).LT.0.0_rlk) error%ierr=FAILURE
    ENDDO

    IF (error%ierr.EQ.FAILURE) THEN
      error%serr="ERROR: "//TRIM(stype)//" not populated"
      RETURN
    ENDIF

  END SUBROUTINE setup_check_nonnegative

  SUBROUTINE setup_print_shape(ishape)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN) :: ishape

    IF (.NOT.ALLOCATED(forms)) RETURN
    IF ((ishape.LT.LBOUND(forms,DIM=1,KIND=ink)).OR.                           &
&       (ishape.GT.UBOUND(forms,DIM=1,KIND=ink))) RETURN
    SELECT CASE(forms(ishape)%itype)
      CASE(RECTANGLE)
        WRITE(OSTREAM,'(a31,f16.10,a1,f16.10,a3,f16.10,a1,f16.10,a1)')         &
&        '    Rectangle: co-ordinates = (',forms(ishape)%param(1),',',         &
&        forms(ishape)%param(2),') (',forms(ishape)%param(3),',',              &
&        forms(ishape)%param(4),')'
      CASE(CIRCLE)
        WRITE(OSTREAM,'(a22,f16.10,a1,f16.10,a11,f16.10)')                     &
&        '    Circle: centre = (',forms(ishape)%param(1),',',                  &
&        forms(ishape)%param(2),') radius = ',forms(ishape)%param(3)
    END SELECT
    IF (forms(ishape)%param(5).GT.0.0_rlk) THEN
      WRITE(OSTREAM,'(a43,f16.10)') '      Fixed user defined volume fraction '&
&      //'= ',forms(ishape)%param(5)
    ENDIF

  END SUBROUTINE setup_print_shape

  SUBROUTINE region_shape(itarget,param,iflagid,sizes,dh,error)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: itarget,iflagid
    REAL(KIND=rlk),   DIMENSION(5),INTENT(IN)    :: param
    TYPE(sizes_t),                 INTENT(INOUT) :: sizes   
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),                 INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink)                  :: ii,jj,kk
    REAL(KIND=rlk),   DIMENSION(NDIM)  :: point
    REAL(KIND=rlk),   DIMENSION(NCORN) :: xx,yy

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set region flag
    DO ii=1,sizes%nel
      DO jj=1,NCORN
        kk=4_ink*(ii-1_ink)+jj
        xx(jj)=utils_kn_get(kk,4_ink*sizes%nel,dh(cnxid)%raddr)
        yy(jj)=utils_kn_get(kk,4_ink*sizes%nel,dh(cnyid)%raddr)
      ENDDO
      point=geometry_kn_getcentroid(NCORN,xx(:),yy(:))
      IF (inside(param,point)) THEN
        CALL utils_kn_set(ii,sizes%nel,itarget,dh(iflagid)%iaddr)
      ENDIF
    ENDDO

  END SUBROUTINE region_shape

  SUBROUTINE material_shape(itarget,param,iflagid,sizes,dh,error)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: itarget,iflagid
    REAL(KIND=rlk),   DIMENSION(5),INTENT(IN)    :: param
    TYPE(sizes_t),                 INTENT(INOUT) :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),                 INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink)                  :: ii,jj,kk,ll
    REAL(KIND=rlk)                     :: vf
    REAL(KIND=rlk),   DIMENSION(NCORN) :: xx,yy

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set material flag and volume fraction for any multi-material elements
    DO ii=1,sizes%nel
      DO jj=1,NCORN
        kk=4_ink*(ii-1_ink)+jj
        xx(jj)=utils_kn_get(kk,4_ink*sizes%nel,dh(cnxid)%raddr)
        yy(jj)=utils_kn_get(kk,4_ink*sizes%nel,dh(cnyid)%raddr)
      ENDDO
      vf=intersect(param(1:4),xx(:),yy(:))
      IF (vf.GT.0.75_rlk) THEN
        CALL utils_kn_set(ii,sizes%nel,itarget,dh(iflagid)%iaddr)
      ELSEIF (vf.GT.0.25_rlk) THEN
        IF (param(5).GT.0.0_rlk) THEN
          vf=param(5)
        ELSE
          vf=subdivide(0_ink,param(1:4),xx(:),yy(:))
        ENDIF
        jj=utils_kn_get(ii,sizes%nel,dh(iflagid)%iaddr)
        IF (jj.GT.0_ink) THEN
          ll=jj
          jj=mix_dr_addel(ii,sizes,dh,error)
          IF (error%ierr.NE.SUCCESS) RETURN
          kk=mix_dr_addcp(-jj,sizes,dh,error)
          IF (error%ierr.NE.SUCCESS) RETURN
          CALL utils_kn_set(kk,sizes%ncp,ll,dh(icpmatid)%iaddr)
          CALL utils_kn_set(kk,sizes%ncp,1.0_rlk-vf,dh(frvolumeid)%raddr)
        ENDIF
        kk=mix_dr_addcp(-jj,sizes,dh,error)
        IF (error%ierr.NE.SUCCESS) RETURN
        CALL utils_kn_set(kk,sizes%ncp,itarget,dh(icpmatid)%iaddr)
        CALL utils_kn_set(kk,sizes%ncp,vf,dh(frvolumeid)%raddr)
      ENDIF
    ENDDO
    CALL mix_dr_flatten(sizes,dh)

  END SUBROUTINE material_shape

  PURE LOGICAL(KIND=lok) FUNCTION inside_circle(param,point)

    ! Argument list
    REAL(KIND=rlk),DIMENSION(4),    INTENT(IN) :: param
    REAL(KIND=rlk),DIMENSION(NDIM), INTENT(IN) :: point

    inside_circle=SQRT((point(1)-param(1))**2_ink+(point(2)-param(2))**2_ink)  &
&                 .LE.param(3)

  END FUNCTION inside_circle

  PURE LOGICAL(KIND=lok) FUNCTION inside_rectangle(param,point)

    ! Argument list
    REAL(KIND=rlk),DIMENSION(4),   INTENT(IN) :: param
    REAL(KIND=rlk),DIMENSION(NDIM),INTENT(IN) :: point

    inside_rectangle=(point(1).GT.param(1)).AND.(point(2).GT.param(2)).AND.    &
&                    (point(1).LT.param(3)).AND.(point(2).LT.param(4))

  END FUNCTION inside_rectangle

  PURE REAL(KIND=rlk) FUNCTION intersect(param,xx,yy)

    ! Argument list
    REAL(KIND=rlk),DIMENSION(4),    INTENT(IN) :: param
    REAL(KIND=rlk),DIMENSION(NCORN),INTENT(IN) :: xx,yy
    ! Local
    INTEGER(KIND=ink)                  :: ii
    LOGICAL(KIND=lok),DIMENSION(NCORN) :: zflag

    DO ii=1,NCORN
      zflag(ii)=inside(param,[xx(ii),yy(ii)])
    ENDDO
    IF (ALL(zflag)) THEN
      intersect=1.0_rlk
    ELSEIF (ANY(zflag)) THEN
      intersect=0.5_rlk
    ELSE
      intersect=0.0_rlk
    ENDIF

  END FUNCTION intersect

  PURE RECURSIVE FUNCTION subdivide(nit,param,xx,yy) RESULT(vf)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN) :: nit
    REAL(KIND=rlk),   DIMENSION(4),    INTENT(IN) :: param
    REAL(KIND=rlk),   DIMENSION(NCORN),INTENT(IN) :: xx,yy
    ! Result
    REAL(KIND=rlk) :: vf
    ! Local
    INTEGER(KIND=ink)                  :: ii,jj,nit_local
    LOGICAL(KIND=lok)                  :: zflag
    REAL(KIND=rlk)                     :: vf_local
    REAL(KIND=rlk),   DIMENSION(NDIM)  :: centroid
    REAL(KIND=rlk),   DIMENSION(NCORN) :: xx_local,yy_local

    nit_local=nit+1_ink
    centroid=geometry_kn_getcentroid(NCORN,xx,yy)

    IF (nit_local.GT.MAXIT) THEN
      zflag=inside(param,centroid)
      IF (zflag) THEN
        vf=1.0_rlk
      ELSE
        vf=0.0_rlk
      ENDIF
      RETURN
    ENDIF

    vf=0.0_rlk
    DO ii=1,NCORN
      xx_local(1)=xx(ii)
      yy_local(1)=yy(ii)
      jj=MOD(ii,NCORN)+1_ink
      xx_local(2)=xx(jj)
      yy_local(2)=yy(jj)
      xx_local(3)=centroid(1)
      yy_local(3)=centroid(2)
      jj=MOD(ii+2,NCORN)+1_ink
      xx_local(4)=xx(jj)
      yy_local(4)=yy(jj)
      vf_local=intersect(param,xx_local,yy_local)
      IF ((vf_local.GT.0.25_rlk).AND.(vf_local.LT.0.75_rlk)) THEN
        vf_local=subdivide(nit_local,param,xx_local,yy_local)
      ENDIF
      vf=vf+vf_local
    ENDDO
    vf=0.25_rlk*vf

  END FUNCTION subdivide

END MODULE setup_IC_mod 
