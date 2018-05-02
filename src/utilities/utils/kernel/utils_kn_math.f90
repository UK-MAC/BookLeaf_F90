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
MODULE utils_kn_math_mod

  USE dataAPI_kinds_mod,ONLY: ink,rlk

  IMPLICIT NONE

  PUBLIC :: utils_kn_divide,utils_kn_sum,utils_kn_multiply

CONTAINS

  SUBROUTINE utils_kn_divide(nsz,numerator,denominator,answer)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nsz
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(IN)  :: numerator,denominator
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(OUT) :: answer
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=1,nsz
      answer(ii)=numerator(ii)/denominator(ii)
    ENDDO

  END SUBROUTINE utils_kn_divide

  SUBROUTINE utils_kn_sum(nsz,right,answer)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nsz
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(IN)    :: right
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(INOUT) :: answer
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=1,nsz
      answer(ii)=answer(ii)+right(ii)
    ENDDO

  END SUBROUTINE utils_kn_sum

  SUBROUTINE utils_kn_multiply(nsz,factor,answer)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nsz
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(IN)    :: factor
    REAL(KIND=rlk),   DIMENSION(nsz),INTENT(INOUT) :: answer
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=1,nsz
      answer(ii)=answer(ii)*factor(ii)
    ENDDO

  END SUBROUTINE utils_kn_multiply

END MODULE utils_kn_math_mod
