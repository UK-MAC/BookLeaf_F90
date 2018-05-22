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
MODULE hydro_kn_edgeviscosity_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN,NFACE

  IMPLICIT NONE

  PUBLIC :: hydro_kn_initeviscosity,hydro_kn_limiteviscosity,                  &
&           hydro_kn_geteviscosity,hydro_kn_getcpeviscosity

CONTAINS

  SUBROUTINE hydro_kn_initeviscosity(nel,cnx,cny,cnu,cnv,elvisc,dx,dy,du,dv,   &
&                                    edviscx,edviscy)

    ! Argument list
    INTEGER(KIND=ink),                  INTENT(IN)  :: nel
    REAL(KIND=rlk),DIMENSION(NCORN,nel),INTENT(IN)  :: cnx,cny,cnu,cnv
    REAL(KIND=rlk),DIMENSION(nel),      INTENT(OUT) :: elvisc
    REAL(KIND=rlk),DIMENSION(NFACE,nel),INTENT(OUT) :: dx,dy,du,dv,edviscx,    &
&                                                      edviscy    
    ! Local
    INTEGER(KIND=ink)                               :: iel

    ! initialisation and gradient construction
    DO iel=1,nel
      elvisc(iel)=0.0_rlk
      edviscx(1,iel)=0.0_rlk
      edviscx(2,iel)=0.0_rlk
      edviscx(3,iel)=0.0_rlk
      edviscx(4,iel)=0.0_rlk
      edviscy(1,iel)=0.0_rlk
      edviscy(2,iel)=0.0_rlk
      edviscy(3,iel)=0.0_rlk
      edviscy(4,iel)=0.0_rlk
      du(1,iel)=cnu(2,iel)-cnu(1,iel)
      du(2,iel)=cnu(3,iel)-cnu(2,iel)
      du(3,iel)=cnu(4,iel)-cnu(3,iel)
      du(4,iel)=cnu(1,iel)-cnu(4,iel)
      dv(1,iel)=cnv(2,iel)-cnv(1,iel)
      dv(2,iel)=cnv(3,iel)-cnv(2,iel)
      dv(3,iel)=cnv(4,iel)-cnv(3,iel)
      dv(4,iel)=cnv(1,iel)-cnv(4,iel)
      dx(1,iel)=cnx(2,iel)-cnx(1,iel)
      dx(2,iel)=cnx(3,iel)-cnx(2,iel)
      dx(3,iel)=cnx(4,iel)-cnx(3,iel)
      dx(4,iel)=cnx(1,iel)-cnx(4,iel)
      dy(1,iel)=cny(2,iel)-cny(1,iel)
      dy(2,iel)=cny(3,iel)-cny(2,iel)
      dy(3,iel)=cny(4,iel)-cny(3,iel)
      dy(4,iel)=cny(1,iel)-cny(4,iel)
    ENDDO

  END SUBROUTINE hydro_kn_initeviscosity

  SUBROUTINE hydro_kn_limiteviscosity(nel,nelg,nndg,zerocut,cvisc1,cvisc2,      &
&                                     indtype,ielel,ielnd,ielfc,eldensity,elcs2,&
&                                     du,dv,dx,dy,scratch,edviscx,edviscy,      &
&                                     elvisc)

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: nel,nelg,nndg
    REAL(KIND=rlk),                         INTENT(IN)    :: cvisc1,cvisc2,    &
&                                                            zerocut    
    INTEGER(KIND=ink),DIMENSION(nndg),      INTENT(IN)    :: indtype
    INTEGER(KIND=ink),DIMENSION(NFACE,nelg),INTENT(IN)    :: ielel,ielfc
    INTEGER(KIND=ink),DIMENSION(NCORN,nelg),INTENT(IN)    :: ielnd    
    REAL(KIND=rlk),   DIMENSION(nelg),      INTENT(IN)    :: eldensity,elcs2
    REAL(KIND=rlk),   DIMENSION(NFACE,nelg),INTENT(IN)    :: du,dv,dx,dy
    REAL(KIND=rlk),   DIMENSION(NFACE,nelg),INTENT(OUT)   :: scratch,edviscx,  &
&                                                            edviscy
    REAL(KIND=rlk),   DIMENSION(nelg),      INTENT(INOUT) :: elvisc
    ! Local
    INTEGER(KIND=ink) :: iside,is1,is2,iel,in1,in2,ins,ic1,ic2
    REAL(KIND=rlk)    :: w1,w2,w3,w4,den,uhat,vhat,xhat,yhat

    DO iside=1,NFACE/2_ink
      is1=MOD(iside+2_ink,NFACE)+1_ink
      is2=iside+1_ink
      DO iel=1,nel
        ! connectivity
        in1=ielel(iside,iel)
        in2=ielel(iside+2,iel)
        ! edge 1
        w1=du(is1,iel)
        w2=dv(is1,iel)
        w3=dx(is1,iel)
        w4=dy(is1,iel)
        den=SQRT(w1*w1+w2*w2)
        den=1.0_rlk/MAX(den,zerocut)
        uhat=w1*den
        vhat=w2*den
        den=SQRT(w3*w3+w4*w4)
        den=1.0_rlk/MAX(den,zerocut)
        xhat=w3*den
        yhat=w4*den
        den=w3*xhat+w4*yhat
        w1=(w1*uhat+w2*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w1=1.0_rlk/SIGN(MAX(ABS(w1),zerocut),w1)
        ins=ielfc(iside,iel)
        ins=MOD(ins,NFACE)+1_ink
        den=dx(ins,in1)*xhat+dy(ins,in1)*yhat
        w2=(du(ins,in1)*uhat+dv(ins,in1)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        scratch(1,iel)=w2*w1
        ins=ielfc(iside+2_ink,iel)
        ins=MOD(ins+2_ink,NFACE)+1_ink
        den=dx(ins,in2)*xhat+dy(ins,in2)*yhat
        w3=(du(ins,in2)*uhat+dv(ins,in2)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        scratch(2,iel)=w3*w1
        ! edge 2
        w1=du(is2,iel)
        w2=dv(is2,iel)
        w3=dx(is2,iel)
        w4=dy(is2,iel)
        den=SQRT(w1*w1+w2*w2)
        den=1.0_rlk/MAX(den,zerocut)
        uhat=w1*den
        vhat=w2*den
        den=SQRT(w3*w3+w4*w4)
        den=1.0_rlk/MAX(den,zerocut)
        xhat=w3*den
        yhat=w4*den
        den=w3*xhat+w4*yhat
        w1=(w1*uhat+w2*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        w1=1.0_rlk/SIGN(MAX(ABS(w1),zerocut),w1)
        ins=ielfc(iside,iel)
        ins=MOD(ins+2_ink,NFACE)+1_ink
        den=dx(ins,in1)*xhat+dy(ins,in1)*yhat
        w2=(du(ins,in1)*uhat+dv(ins,in1)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        scratch(3,iel)=w2*w1
        ins=ielfc(iside+2_ink,iel)
        ins=MOD(ins,NFACE)+1_ink
        den=dx(ins,in2)*xhat+dy(ins,in2)*yhat
        w3=(du(ins,in2)*uhat+dv(ins,in2)*vhat)/SIGN(MAX(ABS(den),zerocut),den)
        scratch(4,iel)=w3*w1
      ENDDO
      ! BC
      ins=iside+2_ink
      DO iel=1,nel
        in1=ielel(iside,iel)
        in2=ielel(ins,iel)
        IF (in1.EQ.iel) THEN
          ic1=ielnd(iside,iel)
          ic2=ielnd(MOD(iside,NFACE)+1_ink,iel)
          IF (((indtype(ic1).LT.0_ink).AND.(indtype(ic2).LT.0_ink)).AND.       &
&          (in2.NE.iel)) THEN
            scratch(1,iel)=1.0_rlk
            scratch(3,iel)=1.0_rlk
          ELSE
            scratch(1,iel)=0.0_rlk
            scratch(3,iel)=0.0_rlk
          ENDIF
        ENDIF
        IF (in2.EQ.iel) THEN
          ic1=ielnd(ins,iel)
          ic2=ielnd(MOD(ins,NFACE)+1_ink,iel)
          IF (((indtype(ic1).LT.0_ink).AND.(indtype(ic2).LT.0_ink)).AND.       &
&          (in1.NE.iel)) THEN
            scratch(2,iel)=1.0_rlk
            scratch(4,iel)=1.0_rlk
          ELSE
            scratch(2,iel)=0.0_rlk
            scratch(4,iel)=0.0_rlk
          ENDIF
        ENDIF
      ENDDO
      ! Apply limiter
      DO iel=1,nel
        ins=ielel(is1,iel)
        w1=cvisc1*SQRT(0.5_rlk*(elcs2(iel)+elcs2(ins)))
        w2=scratch(1,iel)
        w3=scratch(2,iel)
        w2=MIN(0.5_rlk*(w2+w3),2.0_rlk*w2,2.0_rlk*w3,1.0_rlk)
        w2=MAX(0.0_rlk,w2)
        w3=du(is1,iel)
        w4=dv(is1,iel)
        w4=SQRT(w3*w3+w4*w4)
        w3=0.5_rlk*(1.0_rlk-w2)*(eldensity(iel)+eldensity(ins))*(w1+cvisc2*w4)
        edviscx(is1,iel)=w3
        edviscy(is1,iel)=w3
        w4=2.0_rlk*w4/MAX(eldensity(iel),zerocut)
        elvisc(iel)=MAX(elvisc(iel),w3*w4)
        ins=ielel(is2,iel)
        w1=cvisc1*SQRT(0.5_rlk*(elcs2(iel)+elcs2(ins)))
        w2=scratch(3,iel)
        w3=scratch(4,iel)
        w2=MIN(0.5_rlk*(w2+w3),2.0_rlk*w2,2.0_rlk*w3,1.0_rlk)
        w2=MAX(0.0_rlk,w2)
        w3=du(is2,iel)
        w4=dv(is2,iel)
        w4=SQRT(w3*w3+w4*w4)
        w3=0.5_rlk*(1.0_rlk-w2)*(eldensity(iel)+eldensity(ins))*(w1+cvisc2*w4)
        edviscx(is2,iel)=w3
        edviscy(is2,iel)=w3
        w4=2.0_rlk*w4/MAX(eldensity(iel),zerocut)
        elvisc(iel)=MAX(elvisc(iel),w3*w4)
      ENDDO
    ENDDO

  END SUBROUTINE hydro_kn_limiteviscosity

  SUBROUTINE hydro_kn_geteviscosity(nel,zerocut,cnx,cny,cnu,cnv,cnviscx,cnviscy)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: nel
    REAL(KIND=rlk),                        INTENT(IN)    :: zerocut
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)    :: cnx,cny,cnu,cnv
    REAL(KIND=rlk),   DIMENSION(NFACE,nel),INTENT(INOUT) :: cnviscx,cnviscy
    ! Local
    INTEGER(KIND=ink)                  :: iside,ins,iel
    REAL(KIND=rlk)                     :: w1,w2,w3,w4,w5,w6,w7,w8,den,xhat,    &
&                                         yhat,uhat,vhat
    REAL(KIND=rlk),   DIMENSION(NFACE) :: edviscx,edviscy 

    DO iel=1,nel
      DO iside=1,NFACE
        ins=MOD(iside,NFACE)+1_ink
        w1=cnx(iside,iel)
        w2=cnx(ins,iel)
        w3=0.5_rlk*(w1+w2)
        w1=w2-w1
        w2=0.25_rlk*(cnx(1,iel)+cnx(2,iel)+cnx(3,iel)+cnx(4,iel))
        w4=cny(iside,iel)
        w5=cny(ins,iel)
        w6=0.5_rlk*(w4+w5)
        w4=w5-w4
        w5=0.25_rlk*(cny(1,iel)+cny(2,iel)+cny(3,iel)+cny(4,iel))
        w7=SQRT((w2-w3)*(w2-w3)+(w5-w6)*(w5-w6))
        w8=SQRT(w1*w1+w4*w4)
        den=1.0_rlk/w7
        xhat=(w5-w6)*den
        yhat=(w3-w2)*den
        den=1.0_rlk/w8
        w1=w1*den
        w2=w4*den
        w3=xhat*w1+yhat*w2
        den=-SIGN(1.0_rlk,w3)*w7
        xhat=xhat*den
        yhat=yhat*den
        uhat=cnu(ins,iel)-cnu(iside,iel)
        vhat=cnv(ins,iel)-cnv(iside,iel)
        w5=SQRT((uhat*uhat)+(vhat*vhat))
        w6=uhat*xhat+vhat*yhat
        den=w6/MAX(w5,zerocut)
        edviscx(iside)=cnviscx(iside,iel)*uhat*den
        edviscy(iside)=cnviscy(iside,iel)*vhat*den
        ! apply cut-off
        IF ((w5.LE.zerocut).OR.(w6.LE.zerocut).OR.(w7.LE.zerocut).OR.          &
&           (w8.LE.zerocut)) THEN
          edviscx(iside)=0.0_rlk
          edviscy(iside)=0.0_rlk
        ENDIF
      ENDDO
      ! convert from edge to corner
      cnviscx(1,iel)=edviscx(1)-edviscx(4)
      cnviscx(2,iel)=edviscx(2)-edviscx(1)
      cnviscx(3,iel)=edviscx(3)-edviscx(2)
      cnviscx(4,iel)=edviscx(4)-edviscx(3)
      cnviscy(1,iel)=edviscy(1)-edviscy(4)
      cnviscy(2,iel)=edviscy(2)-edviscy(1)
      cnviscy(3,iel)=edviscy(3)-edviscy(2)
      cnviscy(4,iel)=edviscy(4)-edviscy(3)
    ENDDO

  END SUBROUTINE hydro_kn_geteviscosity

  SUBROUTINE hydro_kn_getcpeviscosity(ncp,cvisc1,cvisc2,cpdensity,cpcs2,phi,   &
&                                     cpvisc)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: ncp
    REAL(KIND=rlk),                        INTENT(IN)  :: cvisc1,cvisc2
    REAL(KIND=rlk),   DIMENSION(ncp),      INTENT(IN)  :: cpdensity,cpcs2
    REAL(KIND=rlk),   DIMENSION(NFACE,ncp),INTENT(IN)  :: phi
    REAL(KIND=rlk),   DIMENSION(ncp),      INTENT(OUT) :: cpvisc
    ! Local
    INTEGER(KIND=ink) :: icp,iside
    REAL(KIND=rlk)    :: w1,w2

!    DO icp=1,ncp
!      cpvisc(icp)=0.0_rlk
!      DO iside=1,NFACE
!        w1=(1.0_rlk-phi(iside,icp))*cpdensity(icp)*(cvisc1*SQRT(cpcs2(icp))+   &
!&                                                   cvisc2*)
!        edviscx(iside)=w1
!        edviscy(iside)=w1
!        w2=2.0_rlk*/MAX(cpdensity(icp),zerocut)
!        cpvisc(icp)=MAX(cpvisc(icp),w1*w2)
!      ENDDO
!    ENDDO

  END SUBROUTINE hydro_kn_getcpeviscosity

END MODULE hydro_kn_edgeviscosity_mod
