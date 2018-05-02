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

  PUBLIC :: utils_kn_average11,utils_kn_average22,utils_kn_average12

CONTAINS

  SUBROUTINE utils_kn_average11(ncp,nmx,nel,imxel,imxcp,imxncp,mxfraction,     &
&                               mxarray,elarray)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: ncp,nmx,nel
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(IN)    :: imxel,imxcp,imxncp
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(IN)    :: mxfraction,mxarray
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(INOUT) :: elarray
    ! Local
    INTEGER(KIND=ink) :: imx,iel,icp,ii
    REAL(KIND=rlk)    :: w1

    DO imx=1,nmx
      w1=0.0_rlk
      icp=imxcp(imx)
      DO ii=1,imxncp(imx)
        w1=w1+mxfraction(icp)*mxarray(icp)
        icp=icp+1_ink
      ENDDO
      iel=imxel(imx)
      elarray(iel)=w1
    ENDDO

  END SUBROUTINE utils_kn_average11

  SUBROUTINE utils_kn_average22(ncp,nmx,nel,imxel,imxcp,imxncp,mxfraction,     &
&                               mxarray1,mxarray2,elarray1,elarray2)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: ncp,nmx,nel
    INTEGER(KIND=ink),DIMENSION(nmx),      INTENT(IN)    :: imxel,imxcp,imxncp
    REAL(KIND=rlk),   DIMENSION(ncp),      INTENT(IN)    :: mxfraction
    REAL(KIND=rlk),   DIMENSION(NCORN,ncp),INTENT(IN)    :: mxarray1,mxarray2
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(INOUT) :: elarray1,elarray2
    ! Local
    INTEGER(KIND=ink)                  :: imx,iel,icp,ii
    REAL(KIND=rlk),   DIMENSION(NCORN) :: w1,w2

    DO imx=1,nmx
      w1(1:NCORN)=0.0_rlk
      w2(1:NCORN)=0.0_rlk
      icp=imxcp(imx)
      DO ii=1,imxncp(imx)
        w1(1:NCORN)=w1(1:NCORN)+mxfraction(icp)*mxarray1(1:NCORN,icp)
        w2(1:NCORN)=w2(1:NCORN)+mxfraction(icp)*mxarray2(1:NCORN,icp)
      ENDDO
      iel=imxel(imx)
      elarray1(1:NCORN,iel)=w1(1:NCORN)
      elarray2(1:NCORN,iel)=w2(1:NCORN)
    ENDDO

  END SUBROUTINE utils_kn_average22

  SUBROUTINE utils_kn_average12(ncp,nmx,nel,imxel,imxcp,imxncp,mxfraction,     &
&                               mxarray1,mxarray2,elarray)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: ncp,nmx,nel
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(IN)    :: imxel,imxcp,imxncp
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(IN)    :: mxfraction,mxarray1,     &
&                                                     mxarray2
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(INOUT) :: elarray
    ! Local
    INTEGER(KIND=ink) :: imx,iel,icp,ii
    REAL(KIND=rlk)    :: w1

    DO imx=1,nmx
      w1=0.0_rlk
      icp=imxcp(imx)
      DO ii=1,imxncp(imx)
        w1=w1+mxfraction(icp)*(mxarray1(icp)+mxarray2(icp))
      ENDDO
      iel=imxel(imx)
      elarray(iel)=w1
    ENDDO

  END SUBROUTINE utils_kn_average12

END MODULE utils_kn_average_mod
