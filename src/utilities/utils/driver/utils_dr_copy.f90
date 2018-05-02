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
MODULE utils_dr_copy_mod

  USE utils_kn_copy_mod,ONLY: utils_kn_copy
  USE dataAPI_kinds_mod,ONLY: ink
  USE dataAPI_types_mod,ONLY: data_t

  IMPLICIT NONE

  PUBLIC :: utils_dr_copy

CONTAINS

  SUBROUTINE utils_dr_copy(dlength,origid,copyid,dh)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: dlength,origid,copyid
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh

    CALL utils_kn_copy(dlength,dh(origid)%raddr,dh(copyid)%raddr)

  END SUBROUTINE utils_dr_copy

END MODULE utils_dr_copy_mod
