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
MODULE utils_kn_average_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN

  IMPLICIT NONE

  PUBLIC :: utils_kn_mxsum,utils_kn_mxaverage,utils_kn_mxaveragecn

CONTAINS

  SUBROUTINE utils_kn_mxsum(ncp,nmx,nel,imxel,imxfcp,imxncp,mxarray,elarray)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: ncp,nmx,nel
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(IN)    :: imxel,imxfcp,imxncp
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(IN)    :: mxarray
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(INOUT) :: elarray
    ! Local
    INTEGER(KIND=ink) :: imx,iel,icp,ii
    REAL(KIND=rlk)    :: w1

    DO imx=1,nmx
      w1=0.0_rlk
      icp=imxfcp(imx)
      DO ii=1,imxncp(imx)
        w1=w1+mxarray(icp)
        icp=icp+1_ink
      ENDDO
      iel=imxel(imx)
      elarray(iel)=w1
    ENDDO

  END SUBROUTINE utils_kn_mxsum

  SUBROUTINE utils_kn_mxaverage(ncp,nmx,nel,imxel,imxfcp,imxncp,mxfraction,    &
&                               mxarray,elarray)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: ncp,nmx,nel
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(IN)    :: imxel,imxfcp,imxncp
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(IN)    :: mxfraction,mxarray
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(INOUT) :: elarray
    ! Local
    INTEGER(KIND=ink) :: imx,iel,icp,ii
    REAL(KIND=rlk)    :: w1

    DO imx=1,nmx
      w1=0.0_rlk
      icp=imxfcp(imx)
      DO ii=1,imxncp(imx)
        w1=w1+mxfraction(icp)*mxarray(icp)
        icp=icp+1_ink
      ENDDO
      iel=imxel(imx)
      elarray(iel)=w1
    ENDDO

  END SUBROUTINE utils_kn_mxaverage

  SUBROUTINE utils_kn_mxaveragecn(ncp,nmx,nel,imxel,imxfcp,imxncp,mxfraction,  &
&                                 mxarray,elarray)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: ncp,nmx,nel
    INTEGER(KIND=ink),DIMENSION(nmx),      INTENT(IN)    :: imxel,imxfcp,imxncp
    REAL(KIND=rlk),   DIMENSION(ncp),      INTENT(IN)    :: mxfraction
    REAL(KIND=rlk),   DIMENSION(NCORN,ncp),INTENT(IN)    :: mxarray
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(INOUT) :: elarray
    ! Local
    INTEGER(KIND=ink)                  :: imx,iel,icp,ii
    REAL(KIND=rlk),   DIMENSION(NCORN) :: w1

    DO imx=1,nmx
      w1(1:NCORN)=0.0_rlk
      icp=imxfcp(imx)
      DO ii=1,imxncp(imx)
        w1(1:NCORN)=w1(1:NCORN)+mxfraction(icp)*mxarray(1:NCORN,icp)
        icp=icp+1_ink
      ENDDO
      iel=imxel(imx)
      elarray(1:NCORN,iel)=w1(1:NCORN)
    ENDDO

  END SUBROUTINE utils_kn_mxaveragecn

END MODULE utils_kn_average_mod
