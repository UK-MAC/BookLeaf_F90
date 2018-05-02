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
MODULE hydro_kn_getclosure_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN

  IMPLICIT NONE

  PUBLIC :: hydro_kn_getclosure

CONTAINS

  SUBROUTINE hydro_kn_getclosure()

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nmx,nmat
    REAL(KIND=rlk),                  INTENT(IN)    :: 
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(IN) :: mxpressure
    ! Local
    INTEGER(KIND=ink)                 :: imx,ipair
    REAL(KIND=rlk)                    :: p1,p2
    REAL(KIND=rlk),   DIMENSION(nmat) :: pstar,dfrvolume

    DO imx=1,nmx
      imxcp(imx)
      pstar=0.0_rlk
      dfrvolume=0.0_rlk
      DO ipair=1,npair
        p1=mxpressure(i1)
        p2=mxpressure(i2)
        IF (p1.GT.p2) THEN
        ELSE
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE hydro_kn_getclosure

END MODULE hydro_kn_getclosure_mod
