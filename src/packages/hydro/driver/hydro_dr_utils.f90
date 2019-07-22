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
MODULE hydro_dr_utils_mod

  USE dataAPI_types_mod,   ONLY: sizes_t,data_t
  USE dataAPI_id_mod,      ONLY: eldensityid,elenergyid,elpressureid,          &
&                                elvolumeid,elmassid,frvolumeid,frmassid,      &
&                                cpdensityid,cpenergyid,cppressureid,          &
&                                cpvolumeid,cpmassid
  USE dataAPI_kinds_mod,   ONLY: ink
  USE utils_dr_average_mod,ONLY: utils_dr_mxaverage,utils_dr_mxsum

  IMPLICIT NONE

  PUBLIC :: hydro_dr_averagestate

CONTAINS

  SUBROUTINE hydro_dr_averagestate(sizes,dh)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    IF (sizes%ncp.LE.0_ink) RETURN

    CALL utils_dr_mxaverage(sizes,frvolumeid,cpdensityid,eldensityid,dh)
    CALL utils_dr_mxaverage(sizes,frmassid,cpenergyid,elenergyid,dh)
    CALL utils_dr_mxaverage(sizes,frvolumeid,cppressureid,elpressureid,dh)
    CALL utils_dr_mxsum(sizes,cpvolumeid,elvolumeid,dh)
    CALL utils_dr_mxsum(sizes,cpmassid,elmassid,dh)

  END SUBROUTINE hydro_dr_averagestate

END MODULE hydro_dr_utils_mod
