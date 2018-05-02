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
PROGRAM main

  USE dataAPI_params_mod,    ONLY: SLEN,SUCCESS,HALT_ALL
  USE dataAPI_types_mod,     ONLY: config_t,runtime_t,data_t,error_t
  USE timerAPI_types_mod,    ONLY: timer_t
  USE timer_control_mod,     ONLY: timer_init
  USE inf_error_API_mod,     ONLY: inf_error_halt
  USE inf_init_mod,          ONLY: inf_init_parallel,inf_init_default,inf_init,&
&                                  inf_init_rationalise
  USE inf_io_write_mod,      ONLY: inf_io_configurationprint
  USE inf_io_read_mod,       ONLY: inf_io_read
  USE inf_control_solver_mod,ONLY: inf_control_solver
  USE io_dr_print_mod,       ONLY: io_dr_banner
  USE check_dr_validate_mod, ONLY: check_dr_validate

  IMPLICIT NONE

  ! command line
  CHARACTER(LEN=SLEN)                      :: sfile
  ! data
  TYPE(config_t), POINTER                  :: config
  TYPE(runtime_t)                          :: runtime
  TYPE(data_t),   DIMENSION(:),ALLOCATABLE :: dh
  TYPE(timer_t),  DIMENSION(:),ALLOCATABLE :: timer
  TYPE(error_t)                            :: error

! ###################
! Config
! ###################

  ALLOCATE(config)

! ###################
! Parallelism
! ###################

  CALL inf_init_parallel(config%comm)

! ###################
! TIMERS
! ###################

! start timers
  CALL timer_init(timer,error)
  IF (error%ierr.NE.SUCCESS) THEN
    CALL inf_error_halt(config,runtime,timer,dh,error)
  ENDIF

! ###################
! BANNER
! ###################

! welcome banner
  CALL io_dr_banner(config%comm%world)

! ###################
! DEFAULTS
! ###################

! initialise input
  CALL inf_init_default(sfile,config,runtime,timer,dh)

! ###################
! INPUT
! ###################

! read input
  CALL inf_io_read(sfile,config,runtime,timer,dh)

! rationalise input
  CALL inf_init_rationalise(config,runtime,timer,dh)

! print input
  CALL inf_io_configurationprint(sfile,config,runtime,timer,dh)

! ###################
! INITIALISATION
! ###################

! main initialisation
  CALL inf_init(config,runtime,timer,dh)

! ###################
! SOLVER
! ###################

  CALL inf_control_solver(config,runtime,timer,dh)

! ###################
! FINISH
! ###################

  CALL check_dr_validate(sfile,config,runtime,dh,error)
  IF (error%ierr.NE.SUCCESS) THEN
    CALL inf_error_halt(config,runtime,timer,dh,error)
  ENDIF
  error%serr="End time reached, terminating cleanly"
  error%iout=HALT_ALL
  CALL inf_error_halt(config,runtime,timer,dh,error)

END PROGRAM main
