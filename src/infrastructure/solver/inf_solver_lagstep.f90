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
MODULE inf_solver_lagstep_mod

  USE dataAPI_types_mod,             ONLY: config_t,runtime_t,data_t,error_t
  USE dataAPI_params_mod,            ONLY: SUCCESS
  USE timerAPI_types_mod,            ONLY: timer_t
  USE timerAPI_id_mod,               ONLY: TLAGSTEPID,TGETENERGYID,            &
&                                          TGETGEOMETRYLID
  USE timer_advance_mod,             ONLY: timer_start,timer_end
  USE inf_solver_getacceleration_mod,ONLY: inf_solver_getacceleration
  USE inf_solver_getforce_mod,       ONLY: inf_solver_getforce
  USE inf_solver_getenergy_mod,      ONLY: inf_solver_getenergy
  USE inf_error_API_mod,             ONLY: inf_error_halt
  USE hydro_dr_getstate_mod,         ONLY: hydro_dr_getstate
  USE hydro_dr_geteos_mod,           ONLY: hydro_dr_geteos
  USE hydro_dr_set_mod,              ONLY: hydro_dr_setpredictor,              &
&                                          hydro_dr_setcorrector  
  USE time_dr_set_mod,               ONLY: time_dr_setpredictor,               &
&                                          time_dr_setcorrector

  IMPLICIT NONE

  PRIVATE :: inf_solver_getstate
  PUBLIC  :: inf_solver_lagstep

CONTAINS

  SUBROUTINE inf_solver_lagstep(config,runtime,timer,dh)

    ! Argument list
    TYPE(config_t),              INTENT(IN)    :: config
    TYPE(runtime_t),             INTENT(INOUT) :: runtime
    TYPE(timer_t),  DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    ! Timer
    CALL timer_start(timer(TLAGSTEPID))

    ! ##############
    ! Predictor
    ! ##############
    CALL inf_solver_setpredictor(runtime,dh)
    ! Force
    CALL inf_solver_getforce(config,runtime,timer,dh)
    ! State
    CALL inf_solver_getstate(config,runtime,timer,dh)

    ! ###############
    ! Corrector
    ! ###############
    CALL inf_solver_setcorrector(runtime,dh)
    ! Force
    CALL inf_solver_getforce(config,runtime,timer,dh)
    ! Acceleration
    CALL inf_solver_getacceleration(config,runtime,timer,dh)
    ! State
    CALL inf_solver_getstate(config,runtime,timer,dh)

    ! Timing data
    CALL timer_end(timer(TLAGSTEPID))

  END SUBROUTINE inf_solver_lagstep

  SUBROUTINE inf_solver_getstate(config,runtime,timer,dh)

    ! Argument list
    TYPE(config_t),              INTENT(IN)    :: config
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(timer_t),  DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh
    ! Local
    TYPE(error_t) :: error

    ! Update hydro state
    CALL hydro_dr_getstate(runtime,timer(TGETGEOMETRYLID),dh,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF
    !# Missing code here that can`t be merged
    ! Update internal energy
    CALL inf_solver_getenergy(config,runtime,timer(TGETENERGYID),dh)
    !# Missing code here that can`t be merged
    ! Update pressure and sound-speed
    CALL hydro_dr_geteos(config%hydro,runtime%sizes,timer,dh)
    !# Missing code here that can`t be merged

  END SUBROUTINE inf_solver_getstate

  SUBROUTINE inf_solver_setpredictor(runtime,dh)

    ! Argument list
    TYPE(runtime_t),             INTENT(INOUT) :: runtime
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    ! set timestep info
    CALL time_dr_setpredictor(runtime%timestep)
    ! set hydro
    CALL hydro_dr_setpredictor(runtime%sizes,dh)

  END SUBROUTINE inf_solver_setpredictor

  SUBROUTINE inf_solver_setcorrector(runtime,dh)

    ! Argument list
    TYPE(runtime_t),             INTENT(INOUT) :: runtime
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    ! set timestep info
    CALL time_dr_setcorrector(runtime%timestep)
    ! set hydro
    CALL hydro_dr_setcorrector(runtime%sizes,dh)

  END SUBROUTINE inf_solver_setcorrector

END MODULE inf_solver_lagstep_mod
