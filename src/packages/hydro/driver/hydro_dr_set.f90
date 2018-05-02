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
MODULE hydro_dr_set_mod

  USE dataAPI_types_mod,    ONLY: sizes_t,data_t
  USE dataAPI_id_mod,       ONLY: elenergyid
  USE dataAPI_lagstepid_mod,ONLY: elenergy0id
  USE utils_dr_copy_mod,    ONLY: utils_dr_copy

  IMPLICIT NONE

  PUBLIC :: hydro_dr_setpredictor,hydro_dr_setcorrector

CONTAINS

  SUBROUTINE hydro_dr_setpredictor(sizes,dh)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    ! store internal energy
    CALL utils_dr_copy(sizes%nel,elenergyid,elenergy0id,dh)

  END SUBROUTINE hydro_dr_setpredictor

  SUBROUTINE hydro_dr_setcorrector(sizes,dh)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    ! restore internal energy
    CALL utils_dr_copy(sizes%nel,elenergy0id,elenergyid,dh)

  END SUBROUTINE hydro_dr_setcorrector

END MODULE hydro_dr_set_mod
