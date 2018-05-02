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
MODULE utils_kn_bc_mod

  USE dataAPI_kinds_mod,ONLY: ink,rlk

  IMPLICIT NONE

  PUBLIC  :: utils_kn_bc

CONTAINS

  SUBROUTINE utils_kn_bc(nnd,rcut,indtype,ndu,ndv) 

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nnd
    REAL(KIND=rlk),                  INTENT(IN)    :: rcut
    INTEGER(KIND=ink),DIMENSION(nnd),INTENT(IN)    :: indtype
    REAL(KIND=rlk),   DIMENSION(nnd),INTENT(INOUT) :: ndu,ndv
    ! Local
    INTEGER(KIND=ink) :: ind
    REAL(KIND=rlk)    :: w1,w2

    w1=rcut*rcut
    DO ind=1,nnd
      SELECT CASE(indtype(ind))
        CASE DEFAULT
        CASE(-1_ink)
          ndu(ind)=0.0_rlk
        CASE(-2_ink)
          ndv(ind)=0.0_rlk
        CASE(-3_ink)
          ndu(ind)=0.0_rlk
          ndv(ind)=0.0_rlk
      END SELECT
      w2=ndu(ind)*ndu(ind)+ndv(ind)*ndv(ind)
      IF (w2.LT.w1) THEN
        ndu(ind)=0.0_rlk
        ndv(ind)=0.0_rlk
      ENDIF
    ENDDO

  END SUBROUTINE utils_kn_bc

END MODULE utils_kn_bc_mod
