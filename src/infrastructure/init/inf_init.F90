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
MODULE inf_init_mod

  USE dataAPI_kinds_mod,   ONLY: ink,rlk,lok
  USE dataAPI_types_mod,   ONLY: config_t,error_t,comms_t,runtime_t,data_t,    &
&                                timestep_t,sizes_t
  USE dataAPI_params_mod,  ONLY: SUCCESS
  USE timerAPI_types_mod,  ONLY: timer_t
  USE timerAPI_id_mod,     ONLY: TINITID,TGETEOSIID
  USE timer_advance_mod,   ONLY: timer_end
  USE time_cf_mod,         ONLY: time_cf_defaults,time_cf_rationalise
  USE hydro_cf_mod,        ONLY: hydro_cf_defaults,hydro_cf_rationalise
  USE hydro_dr_init_mod,   ONLY: hydro_dr_init,hydro_dr_initphases
  USE ale_cf_mod,          ONLY: ale_cf_defaults,ale_cf_rationalise
  USE ale_dr_utils_mod,    ONLY: ale_dr_initphases
  USE inf_error_API_mod,   ONLY: inf_error_halt
  USE init_dr_parallel_mod,ONLY: init_dr_mpi,init_dr_omp,init_dr_phasestypes,  &
&                                init_dr_mpi_comm
  USE init_dr_mesh_mod,    ONLY: init_dr_mesh
  USE init_dr_utils_mod,   ONLY: init_dr_utils,init_dr_utilsdefaults,          &
&                                init_dr_state
  USE setup_read_mod,      ONLY: setup_sizes_rationalise,                      &
&                                setup_timestep_rationalise,                   &
&                                setup_utils_rationalise
  USE setup_IC_mod,        ONLY: setup_IC
#ifdef MODY
  USE modify_mod,          ONLY: modify
#endif

  IMPLICIT NONE

  PRIVATE :: inf_init_runtime,inf_init_timestep,inf_init_sizes
  PUBLIC  :: inf_init,inf_init_default,inf_init_parallel,inf_init_rationalise
 
CONTAINS

  SUBROUTINE inf_init(config,runtime,timer,dh)

    ! Argument list
    TYPE(config_t),                          INTENT(INOUT) :: config
    TYPE(runtime_t),                         INTENT(INOUT) :: runtime
    TYPE(timer_t),  DIMENSION(:),            INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: dh
    ! Local
    TYPE(error_t) :: error

    ! initialise mpi communicators
    CALL init_dr_mpi_comm(config%comm)

    ! initialise mesh
    CALL init_dr_mesh(config,runtime,timer,dh,error)
    IF (error%ierr.NE.SUCCESS) CALL inf_error_halt(config,runtime,timer,dh,    &
&                                                  error)

    ! initialise communication phases and collective types
    IF (config%comm%spatial%nproc.GT.1_ink) THEN
      CALL hydro_dr_initphases(config%hydro,dh)
      CALL ale_dr_initphases(config%ale,dh)
      CALL init_dr_phasestypes()
    ENDIF

    ! initialise utils
    CALL init_dr_utils(config,runtime%sizes,dh,error)
    IF (error%ierr.NE.SUCCESS) CALL inf_error_halt(config,runtime,timer,dh,    &
&                                                  error)      

    ! initialise initial conditions
    CALL setup_IC(config%global,config%comm%spatial,runtime%sizes,             &
&                 timer,dh,error)
    IF (error%ierr.NE.SUCCESS) CALL inf_error_halt(config,runtime,timer,dh,    &
&                                                  error)

    ! initialise state
    CALL init_dr_state(config,runtime%sizes,timer(TGETEOSIID),dh)

    ! initialise hydro
    CALL hydro_dr_init(config%hydro,runtime%sizes,dh)

    ! problem specific modifications 
#ifdef MODY
    CALL modify(config,runtime,dh)
#endif

    ! timer
    CALL timer_end(timer(TINITID))

  END SUBROUTINE inf_init

  SUBROUTINE inf_init_default(sfile,config,runtime,timer,dh)

    ! Argument list
    CHARACTER(LEN=*),             INTENT(OUT)   :: sfile
    TYPE(config_t),               INTENT(INOUT) :: config
    TYPE(runtime_t),              INTENT(INOUT) :: runtime
    TYPE(timer_t),   DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),    DIMENSION(:),INTENT(IN)    :: dh    
    ! Local
    TYPE(error_t) :: error

    ! file default
    sfile='control'
    ! utility packages
    CALL init_dr_utilsdefaults(config,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF
    ! time
    CALL time_cf_defaults(config%time)
    ! hydro
    CALL hydro_cf_defaults(config%hydro,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF
    ! ale
    CALL ale_cf_defaults(config%ale,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF
    ! runtime
    CALL inf_init_runtime(runtime)

  END SUBROUTINE inf_init_default

  SUBROUTINE inf_init_parallel(comm)

    ! Argument list
    TYPE(comms_t),INTENT(OUT) :: comm

    ! initialise MPI
    CALL init_dr_mpi(comm)

    ! initialise OpenMP
    CALL init_dr_omp(comm)

  END SUBROUTINE inf_init_parallel

  SUBROUTINE inf_init_rationalise(config,runtime,timer,dh)

    ! Argument list
    TYPE(config_t), POINTER,     INTENT(INOUT) :: config
    TYPE(runtime_t),             INTENT(INOUT) :: runtime
    TYPE(timer_t),  DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh
    ! Local
    TYPE(error_t) :: error

    ! rationalise runtime
    CALL setup_sizes_rationalise(runtime%sizes,error)
    IF (error%ierr.NE.SUCCESS) CALL inf_error_halt(config,runtime,timer,dh,    &
&                                                  error)
    CALL setup_timestep_rationalise(config%time,runtime%timestep)

    ! rationalise config
    CALL setup_utils_rationalise(config,runtime%sizes,error)
    IF (error%ierr.NE.SUCCESS) CALL inf_error_halt(config,runtime,timer,dh,    &
&                                                  error)
    CALL time_cf_rationalise(config%time,config%comm%spatial,config%io,error)
    IF (error%ierr.NE.SUCCESS) CALL inf_error_halt(config,runtime,timer,dh,    &
&                                                  error)
    CALL hydro_cf_rationalise(config%hydro,config%comm%spatial,config%eos,     &
&                             config%io,config%global,runtime%sizes,error)
    IF (error%ierr.NE.SUCCESS) CALL inf_error_halt(config,runtime,timer,dh,    &
&                                                  error)
    CALL ale_cf_rationalise(config%ale,config%time,config%global,              &
&                           config%comm%spatial,config%eos,error)
    IF (error%ierr.NE.SUCCESS) CALL inf_error_halt(config,runtime,timer,dh,    &
&                                                  error)

  END SUBROUTINE inf_init_rationalise

  SUBROUTINE inf_init_runtime(runtime)

    ! Argument list
    TYPE(runtime_t),INTENT(OUT) :: runtime

    ! timestep
    CALL inf_init_timestep(runtime%timestep)

    ! sizes
    CALL inf_init_sizes(runtime%sizes)

  END SUBROUTINE inf_init_runtime

  SUBROUTINE inf_init_timestep(timestep)

    ! Argument list
    TYPE(timestep_t),INTENT(OUT) :: timestep

    timestep%nstep=0_ink
    timestep%idtel=0_ink
    timestep%zcorrector=.FALSE._lok
    timestep%sdt=' INITIAL'
    timestep%mdt='   UNKNOWN'
    timestep%time=-HUGE(1.0_rlk)
    timestep%dt=-HUGE(1.0_rlk)
    timestep%dts=-HUGE(1.0_rlk)

  END SUBROUTINE inf_init_timestep

  SUBROUTINE inf_init_sizes(sizes)

    ! Argument list
    TYPE(sizes_t),INTENT(OUT) :: sizes

    sizes%nel=-1_ink
    sizes%nel1=-1_ink
    sizes%nel2=-1_ink
    sizes%nnd=-1_ink
    sizes%nnd1=-1_ink
    sizes%nnd2=-1_ink
    sizes%nsz=-1_ink
    sizes%nmat=-1_ink
    sizes%nreg=-1_ink
    sizes%ncp=0_ink
    sizes%nmx=0_ink
    sizes%mcp=0_ink
    sizes%mmx=0_ink

  END SUBROUTINE inf_init_sizes

END MODULE inf_init_mod
