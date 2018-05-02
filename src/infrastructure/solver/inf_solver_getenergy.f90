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
MODULE inf_solver_getenergy_mod

  USE dataAPI_types_mod,     ONLY: config_t,runtime_t,data_t
  USE timerAPI_types_mod,    ONLY: timer_t
  USE timer_advance_mod,     ONLY: timer_start,timer_end
  USE hydro_dr_getenergy_mod,ONLY: hydro_dr_getenergy

  IMPLICIT NONE

  PUBLIC :: inf_solver_getenergy

CONTAINS

  SUBROUTINE inf_solver_getenergy(config,runtime,timer,dh)

    ! Argument list
    TYPE(config_t),              INTENT(IN)    :: config
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(timer_t),               INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    ! Timer
    CALL timer_start(timer)

    !# Missing code here that can't be merged

    ! hydro internal energy update
    CALL hydro_dr_getenergy(config%hydro,runtime,dh)

    !# Missing code here that can't be merged

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE inf_solver_getenergy

END MODULE inf_solver_getenergy_mod
