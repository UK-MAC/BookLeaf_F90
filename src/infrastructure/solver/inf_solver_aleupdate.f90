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
MODULE inf_solver_aleupdate_mod

  USE dataAPI_types_mod, ONLY: config_t,runtime_t,error_t,data_t
  USE dataAPI_params_mod,ONLY: SUCCESS
  USE timerAPI_types_mod,ONLY: timer_t
  USE timerAPI_id_mod,   ONLY: TALEUPDATEID
  USE timer_advance_mod, ONLY: timer_start,timer_end
  USE ale_dr_update_mod, ONLY: ale_dr_hydroupdate 
  USE inf_error_API_mod, ONLY: inf_error_halt

  IMPLICIT NONE

  PUBLIC :: inf_solver_aleupdate

CONTAINS

  SUBROUTINE inf_solver_aleupdate(config,runtime,timer,dh)

    ! Argument list
    TYPE(config_t),              INTENT(IN)    :: config
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(timer_t),  DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh
    ! Local
    TYPE(error_t) :: error

    ! Timer
    CALL timer_start(timer(TALEUPDATEID))

    ! Hydro update
    CALL ale_dr_hydroupdate(config%ale,runtime%sizes,timer,dh,error)
    IF (error%ierr.NE.SUCCESS) THEN
      CALL inf_error_halt(config,runtime,timer,dh,error)
    ENDIF

    ! Timing data
    CALL timer_end(timer(TALEUPDATEID))

  END SUBROUTINE inf_solver_aleupdate

END MODULE inf_solver_aleupdate_mod
