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
MODULE ale_dr_update_mod

  USE dataAPI_types_mod,        ONLY: ale_t,sizes_t,data_t,error_t
  USE geometry_dr_volume_mod,   ONLY: geometry_dr_getgeometry
  USE density_dr_getdensity_mod,ONLY: density_dr_getdensity
  USE eos_dr_geteos_mod,        ONLY: eos_dr_geteos
  USE timerAPI_types_mod,       ONLY: timer_t
  USE timerAPI_id_mod,          ONLY: TGETGEOMETRYAID,TGETEOSAID

  IMPLICIT NONE

  PUBLIC :: ale_dr_hydroupdate

CONTAINS

  SUBROUTINE ale_dr_hydroupdate(ale,sizes,timer,dh,error)

    ! Argument list
    TYPE(ale_t),               INTENT(IN)    :: ale
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(timer_t),DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),             INTENT(OUT)   :: error

    ! update geometry
    CALL geometry_dr_getgeometry(sizes,timer(TGETGEOMETRYAID),dh,error)

    ! update density to be consistent with geometry
    CALL density_dr_getdensity(sizes,dh)

    ! update EoS
    CALL eos_dr_geteos(ale%eos,sizes,timer(TGETEOSAID),dh)

  END SUBROUTINE ale_dr_hydroupdate

END MODULE ale_dr_update_mod
