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
MODULE time_dr_set_mod

  USE dataAPI_types_mod,ONLY: timestep_t
  USE dataAPI_kinds_mod,ONLY: lok,rlk

  IMPLICIT NONE

  PUBLIC :: time_dr_setpredictor,time_dr_setcorrector

CONTAINS

  SUBROUTINE time_dr_setpredictor(timestep)

    ! Argument list
    TYPE(timestep_t),INTENT(INOUT) :: timestep

    ! set timestep info
    timestep%zcorrector=.FALSE._lok
    timestep%dts=0.5_rlk*timestep%dt

  END SUBROUTINE time_dr_setpredictor

  SUBROUTINE time_dr_setcorrector(timestep)

    ! Argument list
    TYPE(timestep_t),INTENT(INOUT) :: timestep

    ! set timestep info
    timestep%zcorrector=.TRUE._lok
    timestep%dts=timestep%dt

  END SUBROUTINE time_dr_setcorrector

END MODULE time_dr_set_mod
