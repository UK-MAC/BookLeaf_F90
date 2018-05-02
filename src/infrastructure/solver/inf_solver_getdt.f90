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
MODULE inf_solver_getdt_mod

  USE dataAPI_types_mod,  ONLY: config_t,runtime_t,error_t,dt_t,data_t
  USE dataAPI_params_mod, ONLY: SUCCESS
  USE timerAPI_types_mod, ONLY: timer_t
  USE timerAPI_id_mod,    ONLY: TGETDTID
  USE inf_error_API_mod,  ONLY: inf_error_halt
  USE time_dr_advance_mod,ONLY: time_dr_calc,time_dr_end
  USE hydro_dr_getdt_mod, ONLY: hydro_dr_getdt
  USE ale_dr_getdt_mod,   ONLY: ale_dr_getdt

  IMPLICIT NONE

  PUBLIC :: inf_solver_getdt

CONTAINS

  SUBROUTINE inf_solver_getdt(config,runtime,timer,dh)

    ! Argument list
    TYPE(config_t),              INTENT(IN)    :: config
    TYPE(runtime_t),             INTENT(INOUT) :: runtime
    TYPE(timer_t),  DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh
    ! Local
    TYPE(dt_t),   POINTER :: first,current
    TYPE(error_t)         :: error

    ! Initialise
    NULLIFY(first,current)

    ! Global timestep calculation
    CALL time_dr_calc(config%time,runtime%timestep,timer(TGETDTID),first,      &
&                     current)

    ! Hydro
    CALL hydro_dr_getdt(config%hydro,runtime%sizes,timer,dh,current,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF

    ! ALE
    IF (config%ale%zon) CALL ale_dr_getdt(config%ale,runtime%sizes,dh,current)

    !# Missing code here that can't be merged

    ! End timestep calculation
    CALL time_dr_end(config%time,runtime,timer,dh,first,current,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF

  END SUBROUTINE inf_solver_getdt

END MODULE inf_solver_getdt_mod
