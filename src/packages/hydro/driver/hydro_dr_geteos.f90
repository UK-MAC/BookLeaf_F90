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
MODULE hydro_dr_geteos_mod

  USE dataAPI_types_mod, ONLY: hydro_t,sizes_t,data_t
  USE timerAPI_types_mod,ONLY: timer_t
  USE timerAPI_id_mod,   ONLY: TGETEOSLID
  USE eos_dr_geteos_mod, ONLY: eos_dr_geteos

  IMPLICIT NONE

  PUBLIC :: hydro_dr_geteos

CONTAINS

  SUBROUTINE hydro_dr_geteos(hydro,sizes,timer,dh)

    ! Argument list
    TYPE(hydro_t),             INTENT(IN)    :: hydro
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(timer_t),DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    ! update eos
    CALL eos_dr_geteos(hydro%eos,sizes,timer(TGETEOSLID),dh)

  END SUBROUTINE hydro_dr_geteos

END MODULE hydro_dr_geteos_mod  
