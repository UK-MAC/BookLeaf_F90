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
MODULE setup_read_mod

  USE dataAPI_kinds_mod,  ONLY: ink,lok,rlk
  USE dataAPI_types_mod,  ONLY: error_t,config_t,sizes_t,timestep_t,time_t
  USE dataAPI_params_mod, ONLY: SUCCESS,FAILURE,HALT_SINGLE,SLEN,OSTREAM
  USE utils_kn_string_mod,ONLY: utils_kn_convupper,utils_kn_findstr
  USE eos_cf_mod,         ONLY: eos_cf_get,eos_cf_set,eos_cf_rationalise
  USE io_cf_mod,          ONLY: io_cf_rationalise
  USE global_cf_mod,      ONLY: global_cf_get,global_cf_set,                   &
&                               global_cf_rationalise

  IMPLICIT NONE

  INTEGER(KIND=ink),PARAMETER,PRIVATE :: LN=100_ink

  PRIVATE :: setup_cutoff_nml,setup_eos_nml
  PUBLIC  :: setup_read_command,setup_control_nml,setup_utils_rationalise,     &
&            setup_utils,setup_sizes_rationalise,setup_timestep_rationalise,   &
&            setup_ale_nml

CONTAINS

  SUBROUTINE setup_read_command(sfile,error)

    ! Argument list
    CHARACTER(LEN=*),INTENT(INOUT)   :: sfile
    TYPE(error_t),   INTENT(OUT)     :: error
    ! Local
    INTEGER(KIND=ink)                :: nargs,ii,il,ires
    CHARACTER(LEN=LEN(sfile)+5)      :: str

    ! initialise
    error%ierr=SUCCESS

    nargs=COMMAND_ARGUMENT_COUNT()
    IF (nargs.LE.0_ink) RETURN
    loop:DO ii=1,nargs
      CALL GET_COMMAND_ARGUMENT(NUMBER=ii,VALUE=str,LENGTH=il,STATUS=ires)
      IF (ires.NE.SUCCESS) THEN
        error%ierr=FAILURE
        error%iout=HALT_SINGLE
        error%serr="ERROR: Get_Command_Argument failed"
        RETURN
      ENDIF
      IF (il.LE.5_ink) CYCLE loop
      CALL utils_kn_convupper(str(1:5))
      IF (str(1:5).EQ.'FILE=') THEN
        sfile=' '
        sfile=TRIM(str(6:il))
      ENDIF
    ENDDO loop

  END SUBROUTINE setup_read_command

  SUBROUTINE setup_sizes_rationalise(sizes,error)

    ! Argument list
    TYPE(sizes_t),INTENT(INOUT) :: sizes
    TYPE(error_t),INTENT(OUT)   :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! rationalise
    IF ((sizes%nreg.LT.0_ink).OR.(sizes%nreg.GT.LN)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: incorrect value of nreg"
      RETURN
    ENDIF
    IF ((sizes%nmat.LT.0_ink).OR.(sizes%nmat.GT.LN)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: incorrect value of nmat"
      RETURN
    ENDIF
    IF ((sizes%nel.LT.0_ink).OR.(sizes%nnd.LT.0_ink)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: incorrect mesh extent"
      RETURN
    ENDIF

  END SUBROUTINE setup_sizes_rationalise

  SUBROUTINE setup_timestep_rationalise(time,timestep)

    ! Argument list
    TYPE(time_t),    INTENT(IN)    :: time
    TYPE(timestep_t),INTENT(INOUT) :: timestep

    ! rationalise
    timestep%time=time%time_start
    timestep%dt=time%dt_initial

  END SUBROUTINE setup_timestep_rationalise

  SUBROUTINE setup_control_nml(iunit,rtime_start,rtime_end,rdt_initial,rdt_g,  &
&                              rdt_min,rdt_max,rcfl_sf,rdiv_sf,rzdtnotreg,     &
&                              rzmidlength,rztq,rcvisc1,rcvisc2,rkappareg,     &
&                              rpmeritreg,error)

    ! Argument list
    INTEGER(KIND=ink),                         INTENT(IN)    :: iunit
    TYPE(error_t),                             INTENT(OUT)   :: error
    REAL(KIND=rlk),                            INTENT(INOUT) :: rtime_start,   &
&                                                               rtime_end,     &
&                                                               rdt_initial,   &
&                                                               rdt_g,rdt_min, &
&                                                               rdt_max,       &
&                                                               rcfl_sf,       &
&                                                               rdiv_sf,       &
&                                                               rcvisc1,       &
&                                                               rcvisc2
    LOGICAL(KIND=lok),                         INTENT(INOUT) :: rztq
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: rkappareg,     &
&                                                               rpmeritreg
    LOGICAL(KIND=lok),DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: rzdtnotreg,    &
&                                                               rzmidlength
    ! Local
    REAL(KIND=rlk)                    :: time_start,time_end,dt_initial,dt_g,  &
&                                        dt_min,dt_max,cfl_sf,div_sf,cvisc1,   &
&                                        cvisc2,kappaall,pmeritall
    REAL(KIND=rlk),     DIMENSION(LN) :: kappareg,pmeritreg
    LOGICAL(KIND=lok),  DIMENSION(LN) :: zdtnotreg,zmidlength
    LOGICAL(KIND=lok)                 :: zflag
    CHARACTER(LEN=SLEN)               :: str,visc

    NAMELIST /CONTROL/ time_start,time_end,dt_initial,dt_g,dt_min,dt_max,      &
&                      cfl_sf,div_sf,zdtnotreg,zmidlength,visc,cvisc1,cvisc2,  &
&                      kappaall,kappareg,pmeritall,pmeritreg

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! find namelist
    zflag=utils_kn_findstr('control',iunit,SLEN)

    IF (zflag) THEN
      ! set defaults
      time_start=rtime_start
      time_end=rtime_end
      dt_initial=rdt_initial
      dt_g=rdt_g
      dt_min=rdt_min
      dt_max=rdt_max
      cfl_sf=rcfl_sf
      div_sf=rdiv_sf
      zdtnotreg(:)=rzdtnotreg(1)
      zmidlength(:)=rzmidlength(1)
      IF (rztq) THEN
        visc='TENSOR'
      ELSE
        visc='EDGE'
      ENDIF
      cvisc1=rcvisc1
      cvisc2=rcvisc2
      kappaall=rkappareg(1)
      kappareg(:)=rkappareg(1)
      pmeritall=rpmeritreg(1)
      pmeritreg(:)=rpmeritreg(1)
      ! read namelist
      REWIND(UNIT=iunit)
      READ(UNIT=iunit,NML=CONTROL,IOSTAT=error%ierr,IOMSG=str)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: "//TRIM(str)
        RETURN
      ENDIF
      ! set variables
      rtime_start=time_start
      rtime_end=time_end
      rdt_initial=dt_initial
      rdt_g=dt_g
      rdt_min=dt_min
      rdt_max=dt_max
      rcfl_sf=cfl_sf
      rdiv_sf=div_sf
      DEALLOCATE(rzdtnotreg,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rzdtnotreg"
        RETURN
      ENDIF
      ALLOCATE(rzdtnotreg(SIZE(zdtnotreg)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rzdtnotreg"
        RETURN
      ENDIF
      rzdtnotreg(:)=zdtnotreg(:)
      DEALLOCATE(rzmidlength,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rzmidlength"
        RETURN
      ENDIF
      ALLOCATE(rzmidlength(SIZE(zmidlength)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rzmidlength"
        RETURN
      ENDIF
      rzmidlength(:)=zmidlength(:)
      CALL utils_kn_convupper(visc)
      SELECT CASE(visc)
        CASE('TENSOR')
          rztq=.TRUE._lok
        CASE('EDGE')
          rztq=.FALSE._lok
        CASE DEFAULT
          error%ierr=FAILURE
          error%serr="ERROR: unknown value of visc"
          RETURN
      END SELECT
      rcvisc1=cvisc1
      rcvisc2=cvisc2
      DEALLOCATE(rkappareg,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rkappareg"
        RETURN
      ENDIF
      ALLOCATE(rkappareg(SIZE(kappareg)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rkappareg"
        RETURN
      ENDIF
      rkappareg(:)=MERGE(kappareg,kappaall,kappareg.GT.0.0_rlk)
      DEALLOCATE(rpmeritreg,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rpmeritreg"
        RETURN
      ENDIF
      ALLOCATE(rpmeritreg(SIZE(pmeritreg)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rpmeritreg"
        RETURN
      ENDIF
      rpmeritreg(:)=MERGE(pmeritreg,pmeritall,pmeritreg.GT.0.0_rlk)
    ENDIF

  END SUBROUTINE setup_control_nml

  SUBROUTINE setup_ale_nml(iunit,rnpch,radv_type,rzeul,rale_sf,rpch_ntrig,     &
&                          rpch_type,rpch_motion,rpch_trig,rpch_ontime,        &
&                          rpch_offtime,rpch_om,rpch_minvel,rpch_maxvel,error)

    ! Argument list
    INTEGER(KIND=ink),                           INTENT(IN)    :: iunit
    INTEGER(KIND=ink),                           INTENT(INOUT) :: radv_type,   &
&                                                                 rnpch
    LOGICAL(KIND=lok),                           INTENT(INOUT) :: rzeul
    REAL(KIND=rlk),                              INTENT(INOUT) :: rale_sf
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,INTENT(INOUT) :: rpch_ntrig,  &
&                                                                 rpch_type,   &
&                                                                 rpch_motion
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT) :: rpch_trig
    REAL(KIND=rlk),   DIMENSION(:),  ALLOCATABLE,INTENT(INOUT) :: rpch_ontime, &
&                                                                 rpch_offtime,&
&                                                                 rpch_om,     &
&                                                                 rpch_minvel, &
&                                                                 rpch_maxvel
    TYPE(error_t),                               INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink)                  :: npatch,adv_type
    LOGICAL(KIND=lok)                  :: zflag,zeul
    REAL(KIND=rlk)                     :: ale_sf
    CHARACTER(LEN=SLEN)                :: str
    INTEGER(KIND=ink),DIMENSION(LN)    :: patch_ntrigger,patch_type,           &
&                                         patch_motion
    INTEGER(KIND=ink),DIMENSION(LN,LN) :: patch_trigger
    REAL(KIND=rlk),   DIMENSION(LN)    :: patch_ontime,patch_offtime,patch_om, &
&                                         patch_minvel,patch_maxvel

    NAMELIST /ALE/ npatch,zeul,adv_type,ale_sf,patch_type,patch_motion,        &
&                  patch_ontime,patch_offtime,patch_om,patch_minvel,           &
&                  patch_maxvel,patch_ntrigger,patch_trigger

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! find namelist
    zflag=utils_kn_findstr('ale',iunit,SLEN)

    IF (zflag) THEN
      ! set defaults
      npatch=rnpch
      zeul=rzeul
      adv_type=radv_type
      ale_sf=rale_sf
      patch_type(:)=rpch_type(1)
      patch_motion(:)=rpch_motion(1)
      patch_ontime(:)=rpch_ontime(1)
      patch_offtime(:)=rpch_offtime(1)
      patch_om(:)=rpch_om(1)
      patch_minvel(:)=rpch_minvel(1)
      patch_maxvel(:)=rpch_maxvel(1)
      patch_ntrigger(:)=rpch_ntrig(1)
      patch_trigger(:,:)=rpch_trig(1,1)
      ! read namelist
      REWIND(UNIT=iunit)
      READ(UNIT=iunit,NML=ALE,IOSTAT=error%ierr,IOMSG=str)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: "//TRIM(str)
        RETURN
      ENDIF
      ! set variables
      IF (npatch.GT.LN) THEN
        error%ierr=FAILURE
        error%serr="ERROR: npatch too large"
        RETURN
      ENDIF
      rnpch=npatch
      rzeul=zeul
      radv_type=adv_type
      rale_sf=ale_sf
      DEALLOCATE(rpch_type,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rpch_type"
        RETURN
      ENDIF
      ALLOCATE(rpch_type(SIZE(patch_type)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rpch_type"
        RETURN
      ENDIF
      rpch_type(:)=patch_type(:)
      DEALLOCATE(rpch_motion,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rpch_motion"
        RETURN    
      ENDIF
      ALLOCATE(rpch_motion(SIZE(patch_motion)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rpch_motion"
        RETURN
      ENDIF
      rpch_motion(:)=patch_motion(:)
      DEALLOCATE(rpch_ontime,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rpch_ontime"
        RETURN    
      ENDIF
      ALLOCATE(rpch_ontime(SIZE(patch_ontime)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rpch_ontime"
        RETURN
      ENDIF
      rpch_ontime(:)=patch_ontime(:)
      DEALLOCATE(rpch_offtime,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rpch_offtime"
        RETURN    
      ENDIF
      ALLOCATE(rpch_offtime(SIZE(patch_offtime)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rpch_offtime"
        RETURN
      ENDIF
      rpch_offtime(:)=patch_offtime(:)
      DEALLOCATE(rpch_om,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rpch_om"
        RETURN    
      ENDIF
      ALLOCATE(rpch_om(SIZE(patch_om)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rpch_om"
        RETURN
      ENDIF
      rpch_om(:)=patch_om(:)
      DEALLOCATE(rpch_minvel,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rpch_minvel"
        RETURN
      ENDIF
      ALLOCATE(rpch_minvel(SIZE(patch_minvel)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rpch_minvel"
        RETURN
      ENDIF
      rpch_minvel(:)=patch_minvel(:)
      DEALLOCATE(rpch_maxvel,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rpch_maxvel"
        RETURN
      ENDIF
      ALLOCATE(rpch_maxvel(SIZE(patch_maxvel)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rpch_maxvel"
        RETURN
      ENDIF
      rpch_maxvel(:)=patch_maxvel(:)
      DEALLOCATE(rpch_ntrig,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rpch_ntrig"
        RETURN
      ENDIF
      ALLOCATE(rpch_ntrig(SIZE(patch_ntrigger)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rpch_ntrig"
        RETURN
      ENDIF
      rpch_ntrig(:)=patch_ntrigger(:)
      DEALLOCATE(rpch_trig,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate rpch_trig"
        RETURN
      ENDIF
      ALLOCATE(rpch_trig(SIZE(patch_trigger,DIM=1),SIZE(patch_trigger,DIM=2)), &
&              STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate rpch_trig"
        RETURN
      ENDIF
      rpch_trig(:,:)=patch_trigger(:,:)
    ENDIF

  END SUBROUTINE setup_ale_nml

  SUBROUTINE setup_utils(iunit,config,error)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)    :: iunit
    TYPE(config_t),   INTENT(INOUT) :: config
    TYPE(error_t),    INTENT(OUT)   :: error

    ! process utility namelists

    CALL setup_cutoff_nml(iunit,config,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    CALL setup_eos_nml(iunit,config,error)
    IF (error%ierr.NE.SUCCESS) RETURN

  END SUBROUTINE setup_utils

  SUBROUTINE setup_utils_rationalise(config,sizes,error)

    ! Argument list
    TYPE(config_t),INTENT(INOUT) :: config
    TYPE(sizes_t), INTENT(IN)    :: sizes
    TYPE(error_t), INTENT(OUT)   :: error

    ! rationalise
    CALL eos_cf_rationalise(config%eos,sizes,error)
    IF (error%ierr.NE.SUCCESS) RETURN
    CALL io_cf_rationalise(config%io,sizes,error)
    IF (error%ierr.NE.SUCCESS) RETURN
    CALL global_cf_rationalise(config%global,error)
    IF (error%ierr.NE.SUCCESS) RETURN

  END SUBROUTINE setup_utils_rationalise

  SUBROUTINE setup_cutoff_nml(iunit,config,error)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)    :: iunit
    TYPE(config_t),   INTENT(INOUT) :: config
    TYPE(error_t),    INTENT(OUT)   :: error
    ! Local
    LOGICAL(KIND=lok)   :: zflag
    CHARACTER(LEN=SLEN) :: str
    REAL(KIND=rlk)      :: ccut,zcut,zerocut,pcut,dencut,accut

    NAMELIST /CUTOFF/ ccut,zcut,zerocut,pcut,dencut,accut

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! find namelist
    zflag=utils_kn_findstr('cutoff',iunit,SLEN)

    IF (zflag) THEN
      ! set defaults
      CALL eos_cf_get(eos=config%eos,ccut=ccut,pcut=pcut,error=error)
      IF (error%ierr.NE.SUCCESS) RETURN
      CALL global_cf_get(global=config%global,zcut=zcut,zerocut=zerocut,       &
&                        dencut=dencut,accut=accut)
      ! read namelist
      REWIND(UNIT=iunit)
      READ(UNIT=iunit,NML=CUTOFF,IOSTAT=error%ierr,IOMSG=str)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: "//TRIM(str)
        RETURN
      ENDIF
      ! set variables
      CALL eos_cf_set(eos=config%eos,ccut=ccut,pcut=pcut,error=error)
      IF (error%ierr.NE.SUCCESS) RETURN
      CALL global_cf_set(global=config%global,zcut=zcut,zerocut=zerocut,       &
&                        dencut=dencut,accut=accut)
    ENDIF

  END SUBROUTINE setup_cutoff_nml

  SUBROUTINE setup_eos_nml(iunit,config,error)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)    :: iunit
    TYPE(config_t),   INTENT(INOUT) :: config
    TYPE(error_t),    INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink),  DIMENSION(LN)   :: ieos_type
    CHARACTER(LEN=SLEN),DIMENSION(LN)   :: eos_type
    REAL(KIND=rlk),     DIMENSION(6,LN) :: eos_param
    INTEGER(KIND=ink)                   :: ii
    LOGICAL(KIND=lok)                   :: zflag
    CHARACTER(LEN=SLEN)                 :: str

    NAMELIST /EOS/ eos_type,eos_param

    ! initialise 
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! find namelist
    zflag=utils_kn_findstr('eos',iunit,SLEN)

    IF (zflag) THEN
      ! set defaults
      CALL eos_cf_get(eos=config%eos,eos_type=ieos_type,eos_param=eos_param,   &
&                     error=error)
      IF (error%ierr.NE.SUCCESS) RETURN
      DO ii=1,LN
        SELECT CASE(ieos_type(ii))
          CASE(-1_ink)
            eos_type(ii)="UNKNOWN"
          CASE(0_ink)
            eos_type(ii)="VOID"
          CASE(1_ink)
            eos_type(ii)="IDEAL GAS"
          CASE(2_ink)
            eos_type(ii)="TAIT"
          CASE(3_ink)
            eos_type(ii)="JWL"
          CASE DEFAULT
            error%ierr=FAILURE
            error%iout=HALT_SINGLE
            error%serr="ERROR: unrecognised EoS type"
            RETURN
        END SELECT
      ENDDO
      ! read namelist
      REWIND(UNIT=iunit)
      READ(UNIT=iunit,NML=EOS,IOSTAT=error%ierr,IOMSG=str)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: "//TRIM(str)
        RETURN
      ENDIF
      ! set variables
      DO ii=1,LN
        CALL utils_kn_convupper(eos_type(ii))
        SELECT CASE(eos_type(ii))
          CASE("UNKNOWN")
            ieos_type(ii)=-1_ink
          CASE("VOID")
            ieos_type(ii)=0_ink
          CASE("IDEAL GAS")
            ieos_type(ii)=1_ink
          CASE("TAIT")
            ieos_type(ii)=2_ink
          CASE("JWL")
            ieos_type(ii)=3_ink
          CASE DEFAULT
            error%ierr=FAILURE
            error%iout=HALT_SINGLE
            error%serr="ERROR: unrecognised EoS type"
            RETURN
        END SELECT
      ENDDO
      CALL eos_cf_set(eos=config%eos,eos_type=ieos_type,eos_param=eos_param,   &
&                     error=error)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF

  END SUBROUTINE setup_eos_nml

END MODULE setup_read_mod 
