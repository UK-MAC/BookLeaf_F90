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
MODULE timer_advance_mod

  USE dataAPI_kinds_mod,  ONLY: ink,rlk,lok
  USE dataAPI_types_mod,  ONLY: sizes_t
  USE timerAPI_types_mod, ONLY: timer_t
  USE typhon_API_mod,     ONLY: TYPH_gettime

  IMPLICIT NONE

  PUBLIC :: timer_start,timer_end,timer_endall,timer_startgrind,timer_endgrind

CONTAINS

  SUBROUTINE timer_start(timer)

    ! Argument list
    TYPE(timer_t),INTENT(INOUT) :: timer

    IF (timer%zactive) RETURN
    timer%start=TYPH_gettime()
    timer%zactive=.TRUE._lok

  END SUBROUTINE timer_start

  SUBROUTINE timer_end(timer)

    ! Argument list
    TYPE(timer_t),INTENT(INOUT) :: timer

    timer%time=timer%time+(TYPH_gettime()-timer%start)
    timer%zactive=.FALSE._lok

  END SUBROUTINE timer_end

  SUBROUTINE timer_endall(timer)

    ! Argument list
    TYPE(timer_t),DIMENSION(:),INTENT(INOUT) :: timer
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=LBOUND(timer,DIM=1,KIND=ink),UBOUND(timer,DIM=1,KIND=ink)
      IF (timer(ii)%zactive) CALL timer_end(timer(ii))
    ENDDO

  END SUBROUTINE timer_endall

  SUBROUTINE timer_startgrind(timer)

    ! Argument list
    TYPE(timer_t),INTENT(INOUT) :: timer

    timer%time=TYPH_gettime()

  END SUBROUTINE timer_startgrind

  SUBROUTINE timer_endgrind(sizes,timer)

    ! Argument list
    TYPE(sizes_t),INTENT(IN)    :: sizes
    TYPE(timer_t),INTENT(INOUT) :: timer

    timer%time=TYPH_gettime()-timer%time
    timer%start=timer%time*1.0e6_rlk/sizes%nel

  END SUBROUTINE timer_endgrind

END MODULE timer_advance_mod
