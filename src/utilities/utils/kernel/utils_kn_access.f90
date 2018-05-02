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
MODULE utils_kn_access_mod

  USE dataAPI_kinds_mod,ONLY: ink,rlk

  IMPLICIT NONE

  INTERFACE utils_kn_get
    MODULE PROCEDURE utils_kn_geti,utils_kn_getr
  END INTERFACE utils_kn_get

  INTERFACE utils_kn_set
    MODULE PROCEDURE utils_kn_seti,utils_kn_setr
  END INTERFACE

  PRIVATE :: utils_kn_geti,utils_kn_getr,utils_kn_seti,utils_kn_setr
  PUBLIC  :: utils_kn_get,utils_kn_set

CONTAINS

  PURE FUNCTION utils_kn_geti(indx,nsz,array) RESULT(ires)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN) :: indx,nsz
    INTEGER(KIND=ink),DIMENSION(nsz),INTENT(IN) :: array
    ! Result
    INTEGER(KIND=ink) :: ires

    ires=array(indx)

  END FUNCTION utils_kn_geti

  PURE FUNCTION utils_kn_getr(indx,nsz,array) RESULT(rres)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN) :: indx,nsz
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(IN) :: array
    ! Result
    REAL(KIND=rlk) :: rres

    rres=array(indx)

  END FUNCTION utils_kn_getr

  SUBROUTINE utils_kn_seti(indx,nsz,ival,array)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: indx,nsz,ival
    INTEGER(KIND=ink),DIMENSION(nsz),INTENT(INOUT) :: array

    array(indx)=ival

  END SUBROUTINE utils_kn_seti

  SUBROUTINE utils_kn_setr(indx,nsz,rval,array)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: indx,nsz
    REAL(KIND=rlk),                  INTENT(IN)    :: rval
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(INOUT) :: array

    array(indx)=rval

  END SUBROUTINE utils_kn_setr

END MODULE utils_kn_access_mod
