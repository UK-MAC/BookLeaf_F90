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
MODULE geometry_kn_volume_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN,NFACE,SUCCESS

  IMPLICIT NONE

  PUBLIC  :: geometry_kn_getiso,geometry_kn_getvolume,geometry_kn_checkvolume, &
&            geometry_kn_getfluxvolume

CONTAINS

  SUBROUTINE geometry_kn_getiso(nel,cnx,cny,a1,a2,a3,b1,b2,b3,cnwt)

    ! Argument list
    INTEGER(KIND=ink),                  INTENT(IN)  :: nel
    REAL(KIND=rlk),DIMENSION(NCORN,nel),INTENT(IN)  :: cnx,cny
    REAL(KIND=rlk),DIMENSION(nel),      INTENT(OUT) :: a1,a2,a3,b1,b2,b3
    REAL(KIND=rlk),DIMENSION(NCORN,nel),INTENT(OUT) :: cnwt
    ! Local
    INTEGER(KIND=ink)           :: iel
    REAL(KIND=rlk),   PARAMETER :: ONEBYNINE=1.0_rlk/9.0_rlk

    ! Calculate iso-parametric terms
    DO iel=1,nel
      a1(iel)=0.25_rlk*(-cnx(1,iel)+cnx(2,iel)+cnx(3,iel)-cnx(4,iel))
      a2(iel)=0.25_rlk*( cnx(1,iel)-cnx(2,iel)+cnx(3,iel)-cnx(4,iel))
      a3(iel)=0.25_rlk*(-cnx(1,iel)-cnx(2,iel)+cnx(3,iel)+cnx(4,iel))
      b1(iel)=0.25_rlk*(-cny(1,iel)+cny(2,iel)+cny(3,iel)-cny(4,iel))
      b2(iel)=0.25_rlk*( cny(1,iel)-cny(2,iel)+cny(3,iel)-cny(4,iel))
      b3(iel)=0.25_rlk*(-cny(1,iel)-cny(2,iel)+cny(3,iel)+cny(4,iel))
      cnwt(1,iel)=ONEBYNINE*                                                   &
&                 ((3.0_rlk*b3(iel)-b2(iel))*(3.0_rlk*a1(iel)-a2(iel))         &
&                 -(3.0_rlk*a3(iel)-a2(iel))*(3.0_rlk*b1(iel)-b2(iel)))
      cnwt(2,iel)=ONEBYNINE*                                                   &
&                 ((3.0_rlk*b3(iel)+b2(iel))*(3.0_rlk*a1(iel)-a2(iel))         &
&                 -(3.0_rlk*a3(iel)+a2(iel))*(3.0_rlk*b1(iel)-b2(iel)))
      cnwt(3,iel)=ONEBYNINE*                                                   &
&                 ((3.0_rlk*b3(iel)+b2(iel))*(3.0_rlk*a1(iel)+a2(iel))         &
                  -(3.0_rlk*a3(iel)+a2(iel))*(3.0_rlk*b1(iel)+b2(iel)))
      cnwt(4,iel)=ONEBYNINE*                                                   &
&                 ((3.0_rlk*b3(iel)-b2(iel))*(3.0_rlk*a1(iel)+a2(iel))         &
                  -(3.0_rlk*a3(iel)-a2(iel))*(3.0_rlk*b1(iel)+b2(iel)))
    ENDDO

  END SUBROUTINE geometry_kn_getiso

  SUBROUTINE geometry_kn_getvolume(nsize,a1,a3,b1,b3,volume)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: nsize
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(IN)  :: a1,a3,b1,b3
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(OUT) :: volume
    ! Local
    INTEGER(KIND=ink) :: ii

    ! Calculate volume
    DO ii=1,nsize
      volume(ii)=4.0_rlk*(a1(ii)*b3(ii)-a3(ii)*b1(ii))
    ENDDO

  END SUBROUTINE geometry_kn_getvolume

  SUBROUTINE geometry_kn_checkvolume(nel,val,elvolume,ierr)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nel
    REAL(KIND=rlk),                  INTENT(IN)  :: val
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(IN)  :: elvolume
    INTEGER(KIND=ink),               INTENT(OUT) :: ierr
    ! Local
    INTEGER(KIND=ink)                            :: iel

    ! initialise 
    ierr=SUCCESS

    ! check volume
    DO iel=1,nel
      IF (elvolume(iel).LT.val) ierr=-iel
    ENDDO

  END SUBROUTINE geometry_kn_checkvolume

  SUBROUTINE geometry_kn_getfluxvolume(nnd,nel,cut,ielnd,ndx0,ndy0,ndx1,ndy1,  &
&                                      fcdv)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nnd,nel
    REAL(KIND=rlk),                        INTENT(IN)  :: cut
    INTEGER(KIND=ink),DIMENSION(NCORN,nel),INTENT(IN)  :: ielnd
    REAL(KIND=rlk),   DIMENSION(nnd),      INTENT(IN)  :: ndx0,ndy0,ndx1,ndy1
    REAL(KIND=rlk),   DIMENSION(NFACE,nel),INTENT(OUT) :: fcdv
    ! Local
    INTEGER(KIND=ink) :: iel,jj,jp,n1,n2
    REAL(KIND=rlk)    :: x1,x2,x3,x4,y1,y2,y3,y4,a1,a3,b1,b3

    ! initialise
    fcdv=0.0_rlk

    ! construct volumes
    DO iel=1,nel
      DO jj=1,NFACE
        jp=MOD(jj,NCORN)+1_ink
        n1=ielnd(jj,iel)
        n2=ielnd(jp,iel)
        x1=ndx0(n1)
        x2=ndx0(n2)
        y1=ndy0(n1)
        y2=ndy0(n2)
        x3=ndx1(n2)
        x4=ndx1(n1)
        y3=ndy1(n2)
        y4=ndy1(n1)
        a1=(-x1-x4)+(x3+x2)
        a3=(-x1+x4)+(x3-x2)
        b1=(-y1-y4)+(y3+y2)
        b3=(-y1+y4)+(y3-y2)
        fcdv(jj,iel)=0.25_rlk*(a1*b3-a3*b1)
        IF (fcdv(jj,iel).LT.cut) fcdv(jj,iel)=0.0_rlk
      ENDDO
    ENDDO

  END SUBROUTINE geometry_kn_getfluxvolume

END MODULE geometry_kn_volume_mod
