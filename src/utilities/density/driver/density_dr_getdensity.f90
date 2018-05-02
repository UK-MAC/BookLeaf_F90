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
MODULE density_dr_getdensity_mod

  USE dataAPI_kinds_mod,ONLY: ink
  USE dataAPI_types_mod,ONLY: sizes_t,data_t
  USE dataAPI_id_mod,   ONLY: elmassid,elvolumeid,eldensityid,cpmassid,        &
&                             cpvolumeid,cpdensityid
  USE utils_kn_math_mod,ONLY: utils_kn_divide

  IMPLICIT NONE

  PUBLIC :: density_dr_getdensity

CONTAINS

  SUBROUTINE density_dr_getdensity(sizes,dh)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    ! density update
    CALL utils_kn_divide(sizes%nel,dh(elmassid)%raddr,dh(elvolumeid)%raddr,    &
&                        dh(eldensityid)%raddr)
    IF (sizes%ncp.GT.0_ink) THEN
      CALL utils_kn_divide(sizes%ncp,dh(cpmassid)%raddr,dh(cpvolumeid)%raddr,  &
&                          dh(cpdensityid)%raddr)
    ENDIF

  END SUBROUTINE density_dr_getdensity

END MODULE density_dr_getdensity_mod
