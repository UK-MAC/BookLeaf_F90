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
MODULE hydro_dr_getstate_mod

  USE dataAPI_types_mod,        ONLY: runtime_t,data_t,error_t
  USE dataAPI_params_mod,       ONLY: SUCCESS
  USE timerAPI_types_mod,       ONLY: timer_t
  USE geometry_dr_position_mod, ONLY: geometry_dr_getvertex
  USE geometry_dr_volume_mod,   ONLY: geometry_dr_getgeometry
  USE density_dr_getdensity_mod,ONLY: density_dr_getdensity

  IMPLICIT NONE

  PUBLIC :: hydro_dr_getstate

CONTAINS

  SUBROUTINE hydro_dr_getstate(runtime,timer,dh,error)

    ! Argument list
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(timer_t),               INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),               INTENT(OUT)   :: error

    ! Update co-ordinates
    CALL geometry_dr_getvertex(runtime,dh)

    ! Update geometry and iso-parametric terms
    CALL geometry_dr_getgeometry(runtime%sizes,timer,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! Update density
    CALL density_dr_getdensity(runtime%sizes,dh)

  END SUBROUTINE hydro_dr_getstate

END MODULE hydro_dr_getstate_mod  
