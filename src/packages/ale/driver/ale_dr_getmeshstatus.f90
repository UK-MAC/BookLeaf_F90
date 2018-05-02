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
MODULE ale_dr_getmeshstatus_mod

  USE dataAPI_types_mod,       ONLY: ale_t,sizes_t,data_t
  USE dataAPI_alestepid_mod,   ONLY: indstatusid
  USE timerAPI_types_mod,      ONLY: timer_t
  USE timer_advance_mod,       ONLY: timer_start,timer_end
  USE ale_kn_getmeshstatus_mod,ONLY: ale_kn_getmeshstatus

  IMPLICIT NONE

  PUBLIC :: ale_dr_getmeshstatus

CONTAINS

  SUBROUTINE ale_dr_getmeshstatus(ale,sizes,timer,dh)

    ! Argument list
    TYPE(ale_t),               INTENT(IN)    :: ale
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(timer_t),             INTENT(INOUT) :: timer
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh    

    ! Timer
    CALL timer_start(timer)

    ! Select mesh to be moved
    CALL ale_kn_getmeshstatus(sizes%nnd,ale%zeul,dh(indstatusid)%iaddr)

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE ale_dr_getmeshstatus

END MODULE ale_dr_getmeshstatus_mod
