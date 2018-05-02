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
MODULE inf_solver_getacceleration_mod

  USE dataAPI_types_mod,           ONLY: config_t,runtime_t,data_t
  USE timerAPI_types_mod,          ONLY: timer_t
  USE timerAPI_id_mod,             ONLY: TCOMMLID,TGETACCELERATIONID
  USE timer_advance_mod,           ONLY: timer_start,timer_end
  USE hydro_dr_getacceleration_mod,ONLY: hydro_dr_initacceleration,            &
&                                        hydro_dr_scatteracceleration,         &
&                                        hydro_dr_getacceleration,             &
&                                        hydro_dr_applyacceleration

  IMPLICIT NONE

  PUBLIC :: inf_solver_getacceleration

CONTAINS

  SUBROUTINE inf_solver_getacceleration(config,runtime,timer,dh)

    ! Argument list
    TYPE(config_t),              INTENT(IN)    :: config
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(timer_t),  DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    ! Timer
    CALL timer_start(timer(TGETACCELERATIONID))

    ! Initialisation
    CALL hydro_dr_initacceleration(config%hydro%comm,runtime%sizes,            &
&                                  timer(TCOMMLID),dh)

    ! Scatter to nodes
    CALL hydro_dr_scatteracceleration(config%hydro,runtime%sizes,dh)

    ! Calculate acceleration
    CALL hydro_dr_getacceleration(config%hydro,runtime%sizes,dh)
    
    !# Missing code here that can't be merged

    ! Calculate average velocity
    CALL hydro_dr_applyacceleration(runtime,dh)

    !# Missing code here that can't be merged

    ! Timing data
    CALL timer_end(timer(TGETACCELERATIONID))

  END SUBROUTINE inf_solver_getacceleration

END MODULE inf_solver_getacceleration_mod
