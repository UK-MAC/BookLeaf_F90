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

  USE utils_kn_average_mod,ONLY: utils_kn_average11,utils_kn_average22,        &
&                                utils_kn_average12
  USE dataAPI_kinds_mod,   ONLY: ink
  USE dataAPI_types_mod,   ONLY: sizes_t,data_t
  USE dataAPI_id_mod,      ONLY: imxelid,imxfcpid,imxncpid

  IMPLICIT NONE

  INTERFACE utils_dr_average
    MODULE PROCEDURE utils_dr_average11,utils_dr_average12,utils_dr_average22
  END INTERFACE utils_dr_average

  PRIVATE :: utils_dr_average11,utils_dr_average12,utils_dr_average22
  PUBLIC  :: utils_dr_average

CONTAINS

  SUBROUTINE utils_dr_average11(sizes,frid,mxid,elid,dh)

    ! Argument list
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    INTEGER(KIND=ink),             INTENT(IN)    :: frid,mxid,elid
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh

    CALL utils_kn_average11(sizes%ncp,sizes%nmx,sizes%nel,dh(imxelid)%iaddr,   &
&                           dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,             &
&                           dh(frid)%raddr,dh(mxid)%raddr,dh(elid)%raddr)

  END SUBROUTINE utils_dr_average11

  SUBROUTINE utils_dr_average22(sizes,frid,mx1id,mx2id,el1id,el2id,dh)

    ! Argument list
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    INTEGER(KIND=ink),             INTENT(IN)    :: frid,mx1id,mx2id,el1id,    &
&                                                   el2id
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh

    CALL utils_kn_average22(sizes%ncp,sizes%nmx,sizes%nel,dh(imxelid)%iaddr,   &
&                           dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,             &
&                           dh(frid)%raddr,dh(mx1id)%raddr,dh(mx2id)%raddr,    &
&                           dh(el1id)%raddr,dh(el2id)%raddr)

  END SUBROUTINE utils_dr_average22

  SUBROUTINE utils_dr_average12(sizes,frid,mx1id,mx2id,elid,dh)

    ! Argument list
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    INTEGER(KIND=ink),             INTENT(IN)    :: frid,mx1id,mx2id,elid
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh

    CALL utils_kn_average12(sizes%ncp,sizes%nmx,sizes%nel,dh(imxelid)%iaddr,   &
&                           dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,             &
&                           dh(frid)%raddr,dh(mx1id)%raddr,dh(mx2id)%raddr,    &
&                           dh(elid)%raddr)

  END SUBROUTINE utils_dr_average12

END MODULE utils_dr_average_mod
