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
MODULE inf_io_read_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk,lok
  USE dataAPI_params_mod,ONLY: SUCCESS,HALT_SINGLE
  USE dataAPI_types_mod, ONLY: config_t,runtime_t,error_t,data_t,sizes_t,ale_t
  USE timerAPI_types_mod,ONLY: timer_t
  USE timerAPI_id_mod,   ONLY: TMESHGENID
  USE setup_read_mod,    ONLY: setup_read_command,setup_control_nml,           &
&                              setup_ale_nml,setup_utils
  USE setup_IC_mod,      ONLY: setup_IC_read
  USE setup_mesh_mod,    ONLY: setup_mesh_gen
  USE hydro_cf_mod,      ONLY: hydro_cf_get,hydro_cf_set
  USE time_cf_mod,       ONLY: time_cf_get,time_cf_set
  USE ale_cf_mod,        ONLY: ale_cf_get,ale_cf_set
  USE inf_error_API_mod, ONLY: inf_error_halt

  IMPLICIT NONE

  PRIVATE :: control_nml,ale_nml,open_file,close_file
  PUBLIC  :: inf_io_read

CONTAINS

  SUBROUTINE inf_io_read(sfile,config,runtime,timer,dh)

    ! Argument list
    CHARACTER(LEN=*),             INTENT(INOUT) :: sfile
    TYPE(config_t),               INTENT(INOUT) :: config
    TYPE(runtime_t),              INTENT(INOUT) :: runtime
    TYPE(timer_t),   DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),    DIMENSION(:),INTENT(IN)    :: dh
    ! Local
    INTEGER(KIND=ink) :: iunit
    TYPE(error_t)     :: error

    ! read command line
    CALL setup_read_command(sfile,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF

    ! read input file
    CALL open_file(sfile,iunit,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF
    CALL setup_utils(iunit,config,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF
    CALL setup_IC_read(iunit,config%io,runtime%sizes,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF
    CALL control_nml(iunit,config,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF
    CALL ale_nml(iunit,config%ale,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF
    CALL close_file(iunit,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF

#ifdef LSETUP
    ! generate mesh from input file
    CALL setup_mesh_gen(sfile,config%global,runtime%sizes,timer(TMESHGENID),   &
&                       error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF
#else
    ! read mesh file
#endif

  END SUBROUTINE inf_io_read

  SUBROUTINE control_nml(iunit,config,error)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)    :: iunit
    TYPE(config_t),   INTENT(INOUT) :: config
    TYPE(error_t),    INTENT(OUT)   :: error
    ! Local
    REAL(KIND=rlk)                             :: time_start,time_end,         &
&                                                 dt_initial,dt_g,dt_min,      &
&                                                 dt_max,cfl_sf,div_sf,cvisc1, &
&                                                 cvisc2
    LOGICAL(KIND=lok)                          :: ztq
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE :: kappareg,pmeritreg
    LOGICAL(KIND=lok),DIMENSION(:),ALLOCATABLE :: zdtnotreg,zmidlength

    ! get defaults
    CALL time_cf_get(config%time,time_start=time_start,time_end=time_end,      &
&                    dt_initial=dt_initial,dt_g=dt_g,dt_min=dt_min,            &
&                    dt_max=dt_max)
    CALL hydro_cf_get(config%hydro,cvisc1=cvisc1,cvisc2=cvisc2,cfl_sf=cfl_sf,  &
&                     div_sf=div_sf,kappareg=kappareg,pmeritreg=pmeritreg,     &
&                     ztq=ztq,zmidlength=zmidlength,zdtnotreg=zdtnotreg,       &
&                     error=error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! read namelist
    CALL setup_control_nml(iunit,time_start,time_end,dt_initial,dt_g,dt_min,   &
&                          dt_max,cfl_sf,div_sf,zdtnotreg,zmidlength,ztq,      &
&                          cvisc1,cvisc2,kappareg,pmeritreg,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! set variables
    CALL time_cf_set(config%time,time_start=time_start,time_end=time_end,      &
&                    dt_initial=dt_initial,dt_g=dt_g,dt_min=dt_min,            &
&                    dt_max=dt_max)
    CALL hydro_cf_set(config%hydro,cvisc1=cvisc1,cvisc2=cvisc2,cfl_sf=cfl_sf,  &
&                     div_sf=div_sf,kappareg=kappareg,pmeritreg=pmeritreg,     &
&                     ztq=ztq,zmidlength=zmidlength,zdtnotreg=zdtnotreg,       &
&                     error=error)
    IF (error%ierr.NE.SUCCESS) RETURN

  END SUBROUTINE control_nml

  SUBROUTINE ale_nml(iunit,ale,error)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)    :: iunit
    TYPE(ale_t),      INTENT(INOUT) :: ale
    TYPE(error_t),    INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink)                            :: npatch,adv_type
    LOGICAL(KIND=lok)                            :: zeul
    REAL(KIND=rlk)                               :: ale_sf
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: patch_type,patch_motion,   &
&                                                   patch_ntrigger
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: patch_trigger
    REAL(KIND=rlk),   DIMENSION(:),  ALLOCATABLE :: patch_ontime,patch_offtime,&
&                                                   patch_minvel,patch_maxvel, &
&                                                   patch_om

    ! get defaults
    CALL ale_cf_get(ale,npatch=npatch,adv_type=adv_type,sf=ale_sf,zeul=zeul,   &
&                   patch_type=patch_type,patch_motion=patch_motion,           &
&                   patch_ntrigger=patch_ntrigger,patch_trigger=patch_trigger, &
&                   patch_ontime=patch_ontime,patch_offtime=patch_offtime,     &
&                   patch_minvel=patch_minvel,patch_maxvel=patch_maxvel,       &
&                   patch_om=patch_om,error=error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! read namelist
    CALL setup_ale_nml(iunit,npatch,adv_type,zeul,ale_sf,patch_ntrigger,       &
&                      patch_type,patch_motion,patch_trigger,patch_ontime,     &
&                      patch_offtime,patch_om,patch_minvel,patch_maxvel,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! set variables
    CALL ale_cf_set(ale,npatch=npatch,adv_type=adv_type,sf=ale_sf,zeul=zeul,   &
&                   patch_type=patch_type,patch_motion=patch_motion,           &
&                   patch_ntrigger=patch_ntrigger,patch_trigger=patch_trigger, &
&                   patch_ontime=patch_ontime,patch_offtime=patch_offtime,     &
&                   patch_minvel=patch_minvel,patch_maxvel=patch_maxvel,       &
                    patch_om=patch_om,error=error)
    IF (error%ierr.NE.SUCCESS) RETURN

  END SUBROUTINE ale_nml

  SUBROUTINE open_file(sfile,iunit,error)

    ! Argument list
    CHARACTER(LEN=*), INTENT(IN)  :: sfile
    INTEGER(KIND=ink),INTENT(OUT) :: iunit
    TYPE(error_t),    INTENT(OUT) :: error
    ! Local
    LOGICAL(KIND=lok) :: zopen

    ! Initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! Find unused unit number
    iunit=30_ink
    DO
      INQUIRE(UNIT=iunit,OPENED=zopen)
      IF (.NOT.zopen) EXIT
      iunit=iunit+1_ink
    ENDDO

    ! Open file
    OPEN(UNIT=iunit,FILE=sfile,ACTION='read',STATUS='old',FORM='formatted',    &
&        IOSTAT=error%ierr,IOMSG=error%serr)

  END SUBROUTINE open_file

  SUBROUTINE close_file(iunit,error)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)  :: iunit
    TYPE(error_t),    INTENT(OUT) :: error

    ! Initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! Close file
    CLOSE(UNIT=iunit,IOSTAT=error%ierr,IOMSG=error%serr)

  END SUBROUTINE close_file

END MODULE inf_io_read_mod
