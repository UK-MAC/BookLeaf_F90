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
MODULE utils_kn_copy_mod

  USE dataAPI_kinds_mod,ONLY: ink,rlk

  IMPLICIT NONE

  INTERFACE utils_kn_copy
    MODULE PROCEDURE utils_kn_copyr1,utils_kn_copyr2,utils_kn_copyi1
  END INTERFACE utils_kn_copy

  PRIVATE :: utils_kn_copyr1,utils_kn_copyr2,utils_kn_copyi1
  PUBLIC  :: utils_kn_copy

CONTAINS

  SUBROUTINE utils_kn_copyr1(nsz,right,left)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nsz
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(IN)  :: right
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(OUT) :: left
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=1,nsz
      left(ii)=right(ii)
    ENDDO

  END SUBROUTINE utils_kn_copyr1

  SUBROUTINE utils_kn_copyr2(nsz,right1,right2,left1,left2)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nsz
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(IN)  :: right1,right2
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(OUT) :: left1,left2
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=1,nsz
      left1(ii)=right1(ii)
      left2(ii)=right2(ii)
    ENDDO

  END SUBROUTINE utils_kn_copyr2

  SUBROUTINE utils_kn_copyi1(nsz,right,left)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nsz
    INTEGER(KIND=ink),DIMENSION(nsz),INTENT(IN)  :: right
    INTEGER(KIND=ink),DIMENSION(nsz),INTENT(OUT) :: left
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=1,nsz
      left(ii)=right(ii)
    ENDDO

  END SUBROUTINE utils_kn_copyi1

END MODULE utils_kn_copy_mod
