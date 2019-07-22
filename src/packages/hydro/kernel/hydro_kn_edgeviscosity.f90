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

  PUBLIC :: hydro_kn_initedgeviscosity,hydro_kn_limitedgeviscosity,            &
&           hydro_kn_getedgeviscosity

CONTAINS

  SUBROUTINE hydro_kn_initedgeviscosity(nel,cnx,cny,cnu,cnv,elvisc,dx,dy,du,dv,&
&                                       edviscx,edviscy)

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

  END SUBROUTINE hydro_kn_initedgeviscosity

  SUBROUTINE hydro_kn_limitedgeviscosity(nel,nelg,nndg,zerocut,indtype,ielel,   &
&                                        ielnd,ielfc,du,dv,dx,dy,scratch,       &
&                                        limiter)

    ! Argument list
    INTEGER(KIND=ink),                      INTENT(IN)    :: nel,nelg,nndg
    REAL(KIND=rlk),                         INTENT(IN)    :: zerocut    
    INTEGER(KIND=ink),DIMENSION(nndg),      INTENT(IN)    :: indtype
    INTEGER(KIND=ink),DIMENSION(NFACE,nelg),INTENT(IN)    :: ielel,ielfc
    INTEGER(KIND=ink),DIMENSION(NCORN,nelg),INTENT(IN)    :: ielnd    
    REAL(KIND=rlk),   DIMENSION(NFACE,nelg),INTENT(IN)    :: du,dv,dx,dy
    REAL(KIND=rlk),   DIMENSION(NFACE,nelg),INTENT(OUT)   :: scratch,limiter
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
      ! Construct limiter
      DO iel=1,nel
        ins=ielel(is1,iel)
        w2=scratch(1,iel)
        w3=scratch(2,iel)
        w2=MIN(0.5_rlk*(w2+w3),2.0_rlk*w2,2.0_rlk*w3,1.0_rlk)
        w2=MAX(0.0_rlk,w2)
        limiter(is1,iel)=w2
        ins=ielel(is2,iel)
        w2=scratch(3,iel)
        w3=scratch(4,iel)
        w2=MIN(0.5_rlk*(w2+w3),2.0_rlk*w2,2.0_rlk*w3,1.0_rlk)
        w2=MAX(0.0_rlk,w2)
        limiter(is2,iel)=w2
      ENDDO
    ENDDO

  END SUBROUTINE hydro_kn_limitedgeviscosity

  SUBROUTINE hydro_kn_getedgeviscosity(nsize,zerocut,cvisc1,cvisc2,cnx,cny,du, &
&                                      dv,density,cs2,limiter,cnviscx,cnviscy, &
&                                      cvisc)

    ! Argument list
    INTEGER(KIND=ink),                       INTENT(IN)  :: nsize
    REAL(KIND=rlk),                          INTENT(IN)  :: zerocut,cvisc1,    &
&                                                           cvisc2      
    REAL(KIND=rlk),   DIMENSION(NCORN,nsize),INTENT(IN)  :: cnx,cny
    REAL(KIND=rlk),   DIMENSION(NFACE,nsize),INTENT(IN)  :: du,dv,limiter
    REAL(KIND=rlk),   DIMENSION(nsize),      INTENT(IN)  :: density,cs2
    REAL(KIND=rlk),   DIMENSION(NFACE,nsize),INTENT(OUT) :: cnviscx,cnviscy
    REAL(KIND=rlk),   DIMENSION(nsize),      INTENT(OUT) :: cvisc
    ! Local
    INTEGER(KIND=ink)                  :: iside,ins,ii
    REAL(KIND=rlk)                     :: xmean,x1,x2,xmid,xsdiff,sxnorm,edgex,&
&                                         ymean,y1,y2,ymid,ysdiff,synorm,edgey,&
&                                         w1,w2,w3,side_off,invside_off,       &
&                                         side_len,invside_len,dotprod,magdv
    REAL(KIND=rlk),   DIMENSION(NFACE) :: edviscx,edviscy 

    DO ii=1,nsize
      xmean=0.25_rlk*(cnx(1,ii)+cnx(2,ii)+cnx(3,ii)+cnx(4,ii))
      ymean=0.25_rlk*(cny(1,ii)+cny(2,ii)+cny(3,ii)+cny(4,ii))
      DO iside=1,NFACE
        ! apply limiter
        w1=cvisc1*SQRT(cs2(ii))
        magdv=SQRT(du(iside,ii)*du(iside,ii)+dv(iside,ii)*dv(iside,ii))
        w2=(1.0_rlk-limiter(iside,ii))*density(ii)*(w1+cvisc2*magdv)
        edviscx(iside)=w2
        edviscy(iside)=w2
        w3=2.0_rlk*w2*magdv/density(ii)
        cvisc(ii)=MAX(cvisc(ii),w3)
        ! calculate mesh forces
        ins=MOD(iside,NFACE)+1_ink
        x1=cnx(iside,iel)
        x2=cnx(ins,iel)
        xmid=0.5_rlk*(x1+x2)
        xsdiff=x2-x1
        y1=cny(iside,iel)
        y2=cny(ins,iel)
        ymid=0.5_rlk*(y1+y2)
        ysdiff=y2-y1
        ! define vector S which is normal to the line joining edge mid-point
        ! to cell centroid for edge triangular sub-cell.
        ! Calculate normal from tangent
        side_off=SQRT((xmean-xmid)*(xmean-xmid)+(ymean-ymid)*(ymean-ymid))
        invside_off=1.0_rlk/side_off
        sxnorm=(ymean-ymid)*invside_off
        synorm=(xmid-xmean)*invside_off
        ! check direction against edge
        side_len=SQRT(xsdiff*xsdiff+ysdiff*ysdiff)
        invside_len=1.0_rlk/side_len
        edgex=xsdiff*invside_len
        edgey=ysdiff*invside_len
        ! form dot product between vector S and edge
        dotprod=sxnorm*edgex+synorm*edgey
        w1=-SIGN(1.0_rlk,dotprod)*side_off
        sxnorm=sxnorm*w1
        synorm=synorm*w1
        ! calculate unit vector for change in velocity along edge
        dotprod=du(iside,ii)*sxnorm+dv(iside,ii)*synorm
        ! multiply by unit vector of delta v
        w1=dotprod/MAX(magdv,zerocut)
        edviscx(iside)=edviscx(iside)*du(iside,ii)*w1
        edviscy(iside)=edviscy(iside)*dv(iside,ii)*w1
        ! apply cut-off
        IF ((magdv.LE.zerocut).OR.(dotprod.LE.zerocut).OR.                     &
&           (side_off.LE.zerocut).OR.(side_len.LE.zerocut)) THEN
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

  END SUBROUTINE hydro_kn_getedgeviscosity

END MODULE hydro_kn_edgeviscosity_mod
