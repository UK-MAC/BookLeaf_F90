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
SUBROUTINE inf_error_halt(config,runtime,timer,dh,error)

  USE dataAPI_kinds_mod,   ONLY: lok
  USE dataAPI_types_mod,   ONLY: config_t,error_t,runtime_t,data_t
  USE dataAPI_params_mod,  ONLY: HALT_ALL,HALT_SINGLE,SUCCESS
  USE init_dr_parallel_mod,ONLY: init_dr_kill
  USE io_dr_print_mod,     ONLY: io_dr_spacer,io_dr_print
  USE timerAPI_types_mod,  ONLY: timer_t
  USE timerAPI_id_mod,     ONLY: TIOID
  USE timer_advance_mod,   ONLY: timer_endall
  USE timer_control_mod,   ONLY: timer_finish
  USE timer_print_mod,     ONLY: timer_print
  USE inf_io_write_mod,    ONLY: inf_io_output

  IMPLICIT NONE

  ! Argument list
  TYPE(config_t),                          INTENT(IN)    :: config
  TYPE(runtime_t),                         INTENT(IN)    :: runtime
  TYPE(timer_t),  DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: timer
  TYPE(data_t),   DIMENSION(:),            INTENT(INOUT) :: dh  
  TYPE(error_t),                           INTENT(IN)    :: error
  ! Local
  LOGICAL(KIND=lok) :: zout
  TYPE(error_t)     :: err

  zout=((error%iout.EQ.HALT_ALL).AND.(config%comm%world%zmproc)).OR.           &
&      (error%iout.EQ.HALT_SINGLE)

  ! spacer
  IF (zout) CALL io_dr_spacer()

  ! echo message
  IF (zout) CALL io_dr_print(error%serr) 

  ! Output
  IF (error%iout.EQ.HALT_ALL) THEN
    CALL inf_io_output("final_dump",config,runtime,timer(TIOID),dh)
  ENDIF

  ! halt timers
  CALL timer_endall(timer)

  ! print timers
  IF (zout) CALL timer_print(config,timer)

  ! end timers
  CALL timer_finish(timer,err)
  IF (err%ierr.NE.SUCCESS) THEN
    IF (zout) CALL io_dr_print(err%serr)
  ENDIF

  ! spacer
  IF (zout) CALL io_dr_spacer()

  ! end program
  CALL init_dr_kill(error)

END SUBROUTINE inf_error_halt
