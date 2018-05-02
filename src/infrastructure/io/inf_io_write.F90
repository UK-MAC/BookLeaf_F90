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
MODULE inf_io_write_mod

  USE dataAPI_params_mod,  ONLY: SUCCESS
  USE dataAPI_types_mod,   ONLY: config_t,runtime_t,sizes_t,data_t,error_t
  USE timerAPI_types_mod,  ONLY: timer_t
  USE timer_advance_mod,   ONLY: timer_start,timer_end
  USE inf_error_API_mod,   ONLY: inf_error_halt
  USE hydro_cf_mod,        ONLY: hydro_cf_print
  USE hydro_dr_print_mod,  ONLY: hydro_dr_shortprint
  USE ale_cf_mod,          ONLY: ale_cf_print
  USE time_cf_mod,         ONLY: time_cf_print
  USE io_dr_print_mod,     ONLY: io_dr_header,io_dr_preprocess,io_dr_utils,    &
&                                io_dr_echo,io_dr_spacer,io_dr_print
  USE setup_print_mod,     ONLY: setup_print
#ifdef SILO
  USE init_dr_parallel_mod,ONLY: init_dr_kill
  USE io_dr_silo_mod,      ONLY: io_dr_silodump
#endif

  IMPLICIT NONE

  PUBLIC  :: inf_io_output,inf_io_configurationprint
  PRIVATE :: inf_io_shortprint

CONTAINS

  SUBROUTINE inf_io_shortprint(config,runtime,timer,dh)

    ! Argument list
    TYPE(config_t),              INTENT(IN)    :: config
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(timer_t),               INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh    

    ! Timer
    CALL timer_start(timer)

    ! hydro tables
    CALL hydro_dr_shortprint(config%hydro,runtime,dh)

    ! Timer
    CALL timer_end(timer)

  END SUBROUTINE inf_io_shortprint

  SUBROUTINE inf_io_output(dumpname,config,runtime,timer,dh)

    ! Argument list
    CHARACTER(LEN=*),             INTENT(IN)    :: dumpname
    TYPE(config_t),               INTENT(IN)    :: config
    TYPE(runtime_t),              INTENT(IN)    :: runtime
    TYPE(timer_t),                INTENT(INOUT) :: timer
    TYPE(data_t),    DIMENSION(:),INTENT(INOUT) :: dh
    ! Local
#ifdef SILO    
    TYPE(error_t) :: error
#endif

    ! Short print
    CALL inf_io_shortprint(config,runtime,timer,dh)

    ! visualisation dump
#ifdef SILO
    CALL io_dr_silodump(TRIM(ADJUSTL(dumpname)),config%io,config%comm%world,   &
&                       runtime%sizes,timer,dh,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL io_dr_spacer()
      CALL io_dr_print(error%serr)
      CALL init_dr_kill(error)
    ENDIF
#endif
#ifdef TIO
    CALL io_dr_tiodump(TRIM(ADJUSTL(dumpname))//'.h5')
#endif

  END SUBROUTINE inf_io_output

  SUBROUTINE inf_io_configurationprint(sfile,config,runtime,timer,dh)

    ! Argument list
    CHARACTER(LEN=*),             INTENT(IN)    :: sfile
    TYPE(config_t),               INTENT(IN)    :: config
    TYPE(runtime_t),              INTENT(IN)    :: runtime
    TYPE(timer_t),   DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),    DIMENSION(:),INTENT(IN)    :: dh
    ! Local
    TYPE(error_t) :: error

    IF (config%comm%world%zmproc) THEN
      ! Print header
      CALL io_dr_header(sfile)
      ! Print pre-processing options
      CALL io_dr_preprocess(config%comm%world%nproc,config%comm%nthread)
      ! Echo input file
      CALL io_dr_echo(sfile,error)
      IF (error%ierr.NE.SUCCESS) THEN
        CALL inf_error_halt(config,runtime,timer,dh,error)
      ENDIF
      ! Print time options
      CALL time_cf_print(config%time)
      ! Print hydro options
      CALL hydro_cf_print(config%hydro)
      ! Print ALE options
      CALL ale_cf_print(config%ale)
      ! Print utility options
      CALL io_dr_utils(config)
      ! Print setup options
      CALL setup_print(runtime%sizes,config%io)
    ENDIF

  END SUBROUTINE inf_io_configurationprint

END MODULE inf_io_write_mod
