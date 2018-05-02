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
MODULE timer_print_mod

  USE dataAPI_kinds_mod, ONLY: rlk,ink
  USE dataAPI_types_mod, ONLY: config_t
  USE dataAPI_params_mod,ONLY: OSTREAM
  USE timerAPI_types_mod,ONLY: timer_t
  USE timerAPI_id_mod,   ONLY: TTOTALID

  IMPLICIT NONE

  PUBLIC :: timer_print

CONTAINS

  SUBROUTINE timer_print(config,timer)

    ! Argument list
    TYPE(config_t),             INTENT(IN) :: config
    TYPE(timer_t), DIMENSION(:),INTENT(IN) :: timer

    ! local
    REAL(KIND=rlk)               :: fac
    CHARACTER(LEN=25),PARAMETER  :: ft='(a35,1X,e13.6,1X,f7.3,a2)'
    INTEGER(KIND=ink)            :: ii

    fac=100.0_rlk/timer(TTOTALID)%time

    DO ii=LBOUND(timer,DIM=1,KIND=ink),UBOUND(timer,DIM=1,KIND=ink)
      IF (timer(ii)%time.GT.config%global%zerocut) THEN
        WRITE(OSTREAM,ft) timer(ii)%string,timer(ii)%time,timer(ii)%time*fac,  &
&                         ' %'
      ENDIF
    ENDDO

  END SUBROUTINE timer_print  

END MODULE timer_print_mod
