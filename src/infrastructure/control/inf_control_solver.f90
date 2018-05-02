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
MODULE inf_control_solver_mod

  USE inf_io_write_mod,      ONLY: inf_io_output
  USE inf_solver_getdt_mod,  ONLY: inf_solver_getdt
  USE inf_solver_lagstep_mod,ONLY: inf_solver_lagstep
  USE inf_solver_alestep_mod,ONLY: inf_solver_alestep
  USE dataAPI_types_mod,     ONLY: config_t,runtime_t,data_t
  USE time_dr_utils_mod,     ONLY: time_dr_printcycle,time_finish
  USE timerAPI_types_mod,    ONLY: timer_t
  USE timerAPI_id_mod,       ONLY: TSOLVERID,TSTEPIOID,TIOID
  USE timer_advance_mod,     ONLY: timer_start,timer_startgrind,timer_endgrind

  IMPLICIT NONE

  PUBLIC :: inf_control_solver

CONTAINS

  SUBROUTINE inf_control_solver(config,runtime,timer,dh)

    ! Argument list
    TYPE(config_t),              INTENT(IN)    :: config
    TYPE(runtime_t),             INTENT(INOUT) :: runtime
    TYPE(timer_t),  DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh
    ! Local
    TYPE(timer_t) :: tcycle

    ! Timer
    CALL timer_start(timer(TSOLVERID))

    ! Output
    CALL inf_io_output("initial_dump",config,runtime,timer(TIOID),dh)

    ! Time integration loop
    loop:DO
      CALL timer_startgrind(tcycle)
      ! calculate timestep
      CALL inf_solver_getdt(config,runtime,timer,dh)
      !# Missing code here that can't be merged
      ! lagrangian step
      CALL inf_solver_lagstep(config,runtime,timer,dh)
      ! ale step
      CALL inf_solver_alestep(config,runtime,timer,dh)
      !# Missing code here that can't be merged
      CALL timer_endgrind(runtime%sizes,tcycle)
      CALL time_dr_printcycle(config%comm%world,runtime%timestep,tcycle,       &
&                             timer(TSTEPIOID))
      ! test for end of calculation
      IF (time_finish(runtime%timestep,config%time)) EXIT loop
      !# Missing code here that can't be merged
    ENDDO loop

  END SUBROUTINE inf_control_solver

END MODULE inf_control_solver_mod
