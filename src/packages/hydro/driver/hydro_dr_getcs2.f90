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
MODULE hydro_dr_getcs2_mod

  USE dataAPI_kinds_mod,   ONLY: ink
  USE dataAPI_types_mod,   ONLY: sizes_t,data_t
  USE dataAPI_id_mod,      ONLY: elcs2id,elviscid,cpcs2id,cpviscid,frmassid
  USE utils_dr_average_mod,ONLY: utils_dr_average
  USE utils_kn_math_mod,   ONLY: utils_kn_sum

  IMPLICIT NONE

  PUBLIC :: hydro_dr_getcs2

CONTAINS

  SUBROUTINE hydro_dr_getcs2(sizes,dh)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    ! sum sound-speed
    CALL utils_kn_sum(sizes%nel,dh(elviscid)%raddr,dh(elcs2id)%raddr)
    IF (sizes%ncp.GT.0_ink) THEN
      CALL utils_dr_average(sizes,frmassid,cpcs2id,cpviscid,elcs2id,dh)
    ENDIF

  END SUBROUTINE hydro_dr_getcs2

END MODULE hydro_dr_getcs2_mod
