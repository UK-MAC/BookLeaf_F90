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
MODULE utils_dr_average_mod

  USE utils_kn_average_mod,ONLY: utils_kn_mxaverage,utils_kn_mxaveragecn,      &
&                                utils_kn_mxsum
  USE dataAPI_kinds_mod,   ONLY: ink
  USE dataAPI_types_mod,   ONLY: sizes_t,data_t
  USE dataAPI_id_mod,      ONLY: imxelid,imxfcpid,imxncpid

  IMPLICIT NONE

  PUBLIC :: utils_dr_mxaverage,utils_dr_mxaveragecn,utils_dr_mxsum

CONTAINS

  SUBROUTINE utils_dr_mxaverage(sizes,frid,mxid,elid,dh)

    ! Argument list
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    INTEGER(KIND=ink),             INTENT(IN)    :: frid,mxid,elid
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh

    CALL utils_kn_mxaverage(sizes%ncp,sizes%nmx,sizes%nel,dh(imxelid)%iaddr,   &
&                           dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,             &
&                           dh(frid)%raddr,dh(mxid)%raddr,dh(elid)%raddr)

  END SUBROUTINE utils_dr_mxaverage

  SUBROUTINE utils_dr_mxaveragecn(sizes,frid,mxid,elid,dh)

    ! Argument list
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    INTEGER(KIND=ink),             INTENT(IN)    :: frid,mxid,elid
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh

    CALL utils_kn_mxaveragecn(sizes%ncp,sizes%nmx,sizes%nel,dh(imxelid)%iaddr, &
&                             dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,           &
&                             dh(frid)%raddr,dh(mxid)%raddr,dh(elid)%raddr)

  END SUBROUTINE utils_dr_mxaveragecn

  SUBROUTINE utils_dr_mxsum(sizes,mxid,elid,dh)

    ! Argument list
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    INTEGER(KIND=ink),             INTENT(IN)    :: mxid,elid
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh

    CALL utils_kn_mxsum(sizes%ncp,sizes%nmx,sizes%nel,dh(imxelid)%iaddr,       &
&                       dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,dh(mxid)%raddr,  &
&                       dh(elid)%raddr)

  END SUBROUTINE utils_dr_mxsum

END MODULE utils_dr_average_mod
