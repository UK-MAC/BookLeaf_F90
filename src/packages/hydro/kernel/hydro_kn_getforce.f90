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
MODULE hydro_kn_getforce_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN

  IMPLICIT NONE

  PUBLIC :: hydro_kn_getforceP,hydro_kn_getforceQ,hydro_kn_getforceHG,         &
&           hydro_kn_getforceSP  

CONTAINS

  SUBROUTINE hydro_kn_getforceP(nel,elpressure,a1,a3,b1,b3,cnfx,cnfy)

    ! Argument list
    INTEGER(KIND=ink),                  INTENT(IN)  :: nel
    REAL(KIND=rlk),DIMENSION(nel),      INTENT(IN)  :: elpressure,a1,a3,b1,b3
    REAL(KIND=rlk),DIMENSION(NCORN,nel),INTENT(OUT) :: cnfx,cnfy
    ! Local
    INTEGER(KIND=ink)                               :: iel
    REAL(KIND=rlk)                                  :: w1

    ! Pressure force
    DO iel=1,nel
      w1=elpressure(iel)
      cnfx(1,iel)=w1*(-b3(iel)+b1(iel))
      cnfx(2,iel)=w1*( b3(iel)+b1(iel))
      cnfx(3,iel)=w1*( b3(iel)-b1(iel))
      cnfx(4,iel)=w1*(-b3(iel)-b1(iel))
      cnfy(1,iel)=w1*( a3(iel)-a1(iel))
      cnfy(2,iel)=w1*(-a3(iel)-a1(iel))
      cnfy(3,iel)=w1*(-a3(iel)+a1(iel))
      cnfy(4,iel)=w1*( a3(iel)+a1(iel))
    ENDDO

  END SUBROUTINE hydro_kn_getforceP

  SUBROUTINE hydro_kn_getforceQ(nel,cnviscx,cnviscy,cnfx,cnfy)

    ! Argument list
    INTEGER(KIND=ink),                  INTENT(IN)    :: nel
    REAL(KIND=rlk),DIMENSION(NCORN,nel),INTENT(IN)    :: cnviscx,cnviscy
    REAL(KIND=rlk),DIMENSION(NCORN,nel),INTENT(INOUT) :: cnfx,cnfy
    ! Local
    INTEGER(KIND=ink)                                 :: iel,jj

    DO iel=1,nel
      DO jj=1,NCORN
        cnfx(jj,iel)=cnfx(jj,iel)+cnviscx(jj,iel)
        cnfy(jj,iel)=cnfy(jj,iel)+cnviscy(jj,iel)
      ENDDO
    ENDDO

  END SUBROUTINE hydro_kn_getforceQ

  SUBROUTINE hydro_kn_getforceHG(nel,dt,kappareg,ielreg,eldensity,elarea,cnu,  &
&                                cnv,cnfx,cnfy)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: nel
    REAL(KIND=rlk),                        INTENT(IN)    :: dt
    REAL(KIND=rlk),   DIMENSION(:),        INTENT(IN)    :: kappareg
    INTEGER(KIND=ink),DIMENSION(nel),      INTENT(IN)    :: ielreg
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN)    :: eldensity,elarea
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)    :: cnu,cnv
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(INOUT) :: cnfx,cnfy
    ! Local
    INTEGER(KIND=ink)                                    :: iel,ireg
    REAL(KIND=rlk)                                       :: w1,w2,w3,w4

    ! hourglass restoring force
    w4=1.0_rlk/dt
    DO iel=1,nel
      w2=cnu(1,iel)-cnu(2,iel)+cnu(3,iel)-cnu(4,iel)
      w3=cnv(1,iel)-cnv(2,iel)+cnv(3,iel)-cnv(4,iel)
      ireg=ielreg(iel)
      w1=-kappareg(ireg)*eldensity(iel)*elarea(iel)*w4
      w2=w1*w2
      w3=w1*w3
      cnfx(1,iel)=cnfx(1,iel)+w2
      cnfx(2,iel)=cnfx(2,iel)-w2
      cnfx(3,iel)=cnfx(3,iel)+w2
      cnfx(4,iel)=cnfx(4,iel)-w2
      cnfy(1,iel)=cnfy(1,iel)+w3
      cnfy(2,iel)=cnfy(2,iel)-w3
      cnfy(3,iel)=cnfy(3,iel)+w3
      cnfy(4,iel)=cnfy(4,iel)-w3
    ENDDO

  END SUBROUTINE hydro_kn_getforceHG

  SUBROUTINE hydro_kn_getforceSP(nel,pmeritreg,ielreg,eldensity,elcs2,cnx,cny, &
&                                spmass,cnfx,cnfy)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: nel
    REAL(KIND=rlk),   DIMENSION(:),        INTENT(IN)    :: pmeritreg
    INTEGER(KIND=ink),DIMENSION(nel),      INTENT(IN)    :: ielreg
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN)    :: eldensity,elcs2    
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)    :: cnx,cny,spmass
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(INOUT) :: cnfx,cnfy
    ! Local
    INTEGER(KIND=ink)                        :: iel,ireg,j1,j2
    REAL(KIND=rlk)                           :: w1,w2,w3,w4,w5,w6,x1,x2,x3,x4, &
&                                               y1,y2,y3,y4
    REAL(KIND=rlk),   DIMENSION(NCORN,NCORN) :: lfx,lfy

    !# Missing code here that can't be merged
    DO iel=1,nel
      ! info
      ireg=ABS(ielreg(iel))
      w1=pmeritreg(ireg)
      ! centroid
      x3=0.25_rlk*(cnx(1,iel)+cnx(2,iel)+cnx(3,iel)+cnx(4,iel))
      y3=0.25_rlk*(cny(1,iel)+cny(2,iel)+cny(3,iel)+cny(4,iel))
      ! initialise local force
      lfx=0.0_rlk
      lfy=0.0_rlk
      ! loop over sub-elements
      DO j1=1,NCORN
        ! construct sub-volumes
        x1=cnx(j1,iel)
        y1=cny(j1,iel)
        j2=MOD(j1,NCORN)+1_ink
        x2=0.5_rlk*(x1+cnx(j2,iel))
        y2=0.5_rlk*(y1+cny(j2,iel))
        j2=MOD(j1+2,NCORN)+1_ink
        x4=0.5_rlk*(x1+cnx(j2,iel))
        y4=0.5_rlk*(y1+cny(j2,iel))
        !# Missing code here that can't be merged
        w3=0.25_rlk*(-x1+x2+x3-x4)
        w4=0.25_rlk*(-x1-x2+x3+x4)
        w5=0.25_rlk*(-y1+y2+y3-y4)
        w6=0.25_rlk*(-y1-y2+y3+y4)
        w2=4.0_rlk*(w3*w6-w4*w5)
        ! calculate change in pressure
        w2=spmass(j1,iel)/w2
        w2=w2-eldensity(iel)
        w2=elcs2(iel)*w2
        ! add to forces
        lfx(j1,1)=w2*( w5-w6)
        lfx(j1,2)=w2*( w5+w6)
        lfx(j1,3)=w2*(-w5+w6)
        lfx(j1,4)=w2*(-w5-w6)
        lfy(j1,1)=w2*(-w3+w4)
        lfy(j1,2)=w2*(-w3-w4)
        lfy(j1,3)=w2*( w3-w4)
        lfy(j1,4)=w2*( w3+w4)
      ENDDO
      ! distribute forces
      w2=0.5_rlk*(lfx(1,4)+lfx(4,2))
      w3=0.5_rlk*(lfx(1,2)+lfx(2,4))
      w4=0.5_rlk*(lfx(2,2)+lfx(3,4))
      w5=0.5_rlk*(lfx(4,4)+lfx(3,2))
      w6=0.25_rlk*(lfx(1,3)+lfx(2,3)+lfx(3,3)+lfx(4,3))
      cnfx(1,iel)=cnfx(1,iel)+w1*(lfx(1,1)+w2+w3+w6)
      cnfx(2,iel)=cnfx(2,iel)+w1*(lfx(2,1)+w4+w3+w6)
      cnfx(3,iel)=cnfx(3,iel)+w1*(lfx(3,1)+w4+w5+w6)
      cnfx(4,iel)=cnfx(4,iel)+w1*(lfx(4,1)+w2+w5+w6)
      w2=0.5_rlk*(lfy(1,4)+lfy(4,2))
      w3=0.5_rlk*(lfy(1,2)+lfy(2,4))
      w4=0.5_rlk*(lfy(2,2)+lfy(3,4))
      w5=0.5_rlk*(lfy(4,4)+lfy(3,2))
      w6=0.25_rlk*(lfy(1,3)+lfy(2,3)+lfy(3,3)+lfy(4,3))
      cnfy(1,iel)=cnfy(1,iel)+w1*(lfy(1,1)+w2+w3+w6)
      cnfy(2,iel)=cnfy(2,iel)+w1*(lfy(2,1)+w4+w3+w6)
      cnfy(3,iel)=cnfy(3,iel)+w1*(lfy(3,1)+w4+w5+w6)
      cnfy(4,iel)=cnfy(4,iel)+w1*(lfy(4,1)+w2+w5+w6)
    ENDDO

  END SUBROUTINE hydro_kn_getforceSP

END MODULE hydro_kn_getforce_mod  
