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
MODULE time_dr_utils_mod

  USE dataAPI_types_mod, ONLY: timestep_t,time_t,comm_t
  USE dataAPI_kinds_mod, ONLY: lok
  USE dataAPI_params_mod,ONLY: OSTREAM
  USE timerAPI_types_mod,ONLY: timer_t
  USE timer_advance_mod, ONLY: timer_start,timer_end

  IMPLICIT NONE

  PUBLIC :: time_dr_printcycle,time_finish

CONTAINS

  SUBROUTINE time_dr_printcycle(comm,timestep,tcycle,timer)

    ! Argument list
    TYPE(comm_t),    INTENT(IN)    :: comm
    TYPE(timestep_t),INTENT(IN)    :: timestep
    TYPE(timer_t),   INTENT(IN)    :: tcycle
    TYPE(timer_t),   INTENT(INOUT) :: timer

    ! Binding
    IF (.NOT.comm%zmproc) RETURN

    ! Timer
    CALL timer_start(timer)

    ! Write out cycle information
    WRITE(OSTREAM,'(" step=",i7,"  el=",i9,1X,a10," dt=",1pe16.9,'             &
&    //'"  time=",1pe16.9,"  grind=",1pe8.1,"  timer=",1pe16.9," s",1X,a8)')   &
&    timestep%nstep,timestep%idtel,timestep%mdt,timestep%dt,timestep%time,     &
&    tcycle%start,tcycle%time,timestep%sdt

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE time_dr_printcycle

  PURE LOGICAL(KIND=lok) FUNCTION time_finish(timestep,time)

    ! Argument list
    TYPE(timestep_t),INTENT(IN) :: timestep
    TYPE(time_t),    INTENT(IN) :: time

    ! Initialise
    time_finish=.FALSE._lok

    ! Check for end time
    IF (timestep%time.GE.time%time_end) time_finish=.TRUE._lok

  END FUNCTION time_finish

END MODULE time_dr_utils_mod
