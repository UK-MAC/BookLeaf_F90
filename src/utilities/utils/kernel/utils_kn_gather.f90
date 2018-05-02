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
MODULE utils_kn_gather_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN

  IMPLICIT NONE

  INTERFACE utils_kn_mxgather
    MODULE PROCEDURE utils_kn_mxgather1,utils_kn_mxgather4,utils_kn_mxgathercn
  END INTERFACE utils_kn_mxgather

  PRIVATE :: utils_kn_mxgather1,utils_kn_mxgather4,utils_kn_mxgathercn
  PUBLIC  :: utils_kn_mxgather,utils_kn_cngather

CONTAINS

  SUBROUTINE utils_kn_cngather(n1,n2,indx,aa,bb)

    ! Argument list
    INTEGER(KIND=ink),                    INTENT(IN)  :: n1,n2
    INTEGER(KIND=ink),DIMENSION(NCORN,n1),INTENT(IN)  :: indx
    REAL(KIND=rlk),   DIMENSION(n2),      INTENT(IN)  :: aa
    REAL(KIND=rlk),   DIMENSION(NCORN,n1),INTENT(OUT) :: bb
    ! Local
    INTEGER(KIND=ink) :: ii,jj

    DO ii=1,n1
      DO jj=1,NCORN
        bb(jj,ii)=aa(indx(jj,ii))
      ENDDO
    ENDDO

  END SUBROUTINE utils_kn_cngather

  SUBROUTINE utils_kn_mxgather1(nel,nmx,ncp,imxel,imxfcp,imxncp,elarray,       &
&                               mxarray)
  
    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nel,nmx,ncp
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(IN)  :: imxel,imxfcp,imxncp
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(IN)  :: elarray
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(OUT) :: mxarray
    ! Local
    INTEGER(KIND=ink) :: imx,icp,ii
    REAL(KIND=rlk)    :: w1

    DO imx=1,nmx
      w1=elarray(imxel(imx))
      icp=imxfcp(imx)
      DO ii=1,imxncp(imx)
        mxarray(icp)=w1
        icp=icp+1_ink
      ENDDO
    ENDDO

  END SUBROUTINE utils_kn_mxgather1

  SUBROUTINE utils_kn_mxgather4(nel,nmx,ncp,imxel,imxfcp,imxncp,elarray1,      &
&                               elarray2,elarray3,elarray4,mxarray1,mxarray2,  &
&                               mxarray3,mxarray4)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nel,nmx,ncp
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(IN)  :: imxel,imxfcp,imxncp
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(IN)  :: elarray1,elarray2,elarray3,&
&                                                   elarray4
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(OUT) :: mxarray1,mxarray2,mxarray3,&
&                                                   mxarray4
    ! Local
    INTEGER(KIND=ink) :: iel,imx,icp,ii
    REAL(KIND=rlk)    :: w1,w2,w3,w4

    DO imx=1,nmx
      iel=imxel(imx)
      w1=elarray1(iel)
      w2=elarray2(iel)
      w3=elarray3(iel)
      w4=elarray4(iel)
      icp=imxfcp(imx)
      DO ii=1,imxncp(imx)
        mxarray1(icp)=w1
        mxarray2(icp)=w2
        mxarray3(icp)=w3
        mxarray4(icp)=w4
        icp=icp+1_ink
      ENDDO
    ENDDO

  END SUBROUTINE utils_kn_mxgather4

  SUBROUTINE utils_kn_mxgathercn(nel,nmx,ncp,imxel,imxfcp,imxncp,elarray,      &
&                                mxarray)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nel,nmx,ncp
    INTEGER(KIND=ink),DIMENSION(nmx),      INTENT(IN)  :: imxel,imxfcp,imxncp
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)  :: elarray
    REAL(KIND=rlk),   DIMENSION(NCORN,ncp),INTENT(OUT) :: mxarray
    ! Local
    INTEGER(KIND=ink)                  :: imx,icp,ii
    REAL(KIND=rlk),   DIMENSION(NCORN) :: w1

    DO imx=1,nmx
      w1(:)=elarray(:,imxel(imx))
      icp=imxfcp(imx)
      DO ii=1,imxncp(imx)
        mxarray(:,icp)=w1(:)
        icp=icp+1_ink
      ENDDO
    ENDDO

  END SUBROUTINE utils_kn_mxgathercn

END MODULE utils_kn_gather_mod
