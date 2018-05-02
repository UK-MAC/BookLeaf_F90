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
MODULE ale_kn_advectors_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk,lak
  USE dataAPI_params_mod,ONLY: NCORN,NFACE

  IMPLICIT NONE

  PUBLIC :: ale_kn_fluxelvl,ale_kn_fluxndvl,ale_kn_sumflux,ale_kn_updateel,    &
&           ale_kn_updatend

CONTAINS

  SUBROUTINE ale_kn_fluxelvl(id1,id2,ilsize,iasize,ielel,ielfc,cnbasis,        &
&                            fcdbasis,elvar,fcflux)

    ! Argument list
    INTEGER(KIND=ink),                        INTENT(IN)  :: id1,id2,ilsize,   &
&                                                            iasize
    INTEGER(KIND=ink),DIMENSION(NFACE,iasize),INTENT(IN)  :: ielel,ielfc
    REAL(KIND=rlk),   DIMENSION(NCORN,iasize),INTENT(IN)  :: cnbasis,fcdbasis
    REAL(KIND=rlk),   DIMENSION(iasize),      INTENT(IN)  :: elvar
    REAL(KIND=rlk),   DIMENSION(NFACE,iasize),INTENT(OUT) :: fcflux
    ! Local
    INTEGER(KIND=ink) :: i1,i2,j1,j2,iel,iel1,iel2
    REAL(KIND=rlk)    :: r1,r2,r3,r4,w1,w2,w3,w4,w5,w6,w7,w8,rv,grad

    ! initialise
    fcflux=0.0_rlk

    ! construct flux
    DO i1=id1,id2
      i2=i1+2_ink
      DO iel=1,ilsize
        iel2=ielel(i2,iel)
        j2=ielfc(i2,iel)
        j1=MOD(i2,NCORN)+1_ink
        r3=cnbasis(i2,iel)+cnbasis(j1,iel)
        j1=MOD(j2,NCORN)+1_ink
        w5=r3+cnbasis(j2,iel2)+cnbasis(j1,iel2)
        iel1=ielel(i1,iel)
        j2=ielfc(i1,iel)
        j1=i1+1_ink
        r4=cnbasis(i1,iel)+cnbasis(j1,iel)
        j1=MOD(j2,NCORN)+1_ink
        w6=r4+cnbasis(j2,iel1)+cnbasis(j1,iel1)
        rv=elvar(iel)
        r1=fcdbasis(i1,iel)
        r2=fcdbasis(i2,iel)
        w1=rv-elvar(iel2)
        w2=elvar(iel1)-rv
        w3=ABS(w1)
        w4=ABS(w2)
        w7=SIGN(1.0_rlk,w2)
        w8=(w1*w6*w6+w2*w5*w5)/(w5*w6*(w5+w6))
        grad=w7*MIN(ABS(w8),w3/w5,w4/w6)
        IF (w1*w2.LE.0.0_rlk) grad=0.0_rlk
        r1=r1*(rv+grad*(r3-0.5_rlk*r1))
        r2=r2*(rv-grad*(r4-0.5_rlk*r2))
        fcflux(i1,iel)=r1
        fcflux(i2,iel)=r2
      ENDDO
    ENDDO

  END SUBROUTINE ale_kn_fluxelvl  

  SUBROUTINE ale_kn_fluxndvl(ilsize,iasize,ielel,ielfc,cnbasis,cndbasis,cnvar, &
&                            cnflux)

    ! Argument list
    INTEGER(KIND=ink),                        INTENT(IN)  :: iasize,ilsize
    INTEGER(KIND=ink),DIMENSION(NFACE,iasize),INTENT(IN)  :: ielel,ielfc
    REAL(KIND=rlk),   DIMENSION(NCORN,iasize),INTENT(IN)  :: cnbasis,cndbasis, &
&                                                            cnvar
    REAL(KIND=rlk),   DIMENSION(NCORN,iasize),INTENT(OUT) :: cnflux
    ! Local
    INTEGER(KIND=ink) :: ilfcl,ilfcr,icn,iel,iell,ielr,ifcl,ifcr,ii,ilndl,     &
&                        ilndr,ilnndl,ilnndr
    REAL(KIND=rlk)    :: w1,w2,w3,w4,w5,w6,w7,w8,rd,grad,rv

    ! initialise
    cnflux=0.0_rlk

    ! construct flux
    DO ilfcl=1,2
      ilfcr=ilfcl+2_ink
      DO icn=1,2
        ilndl=ilfcl+icn-1_ink
        ilndr=MOD(ilfcr-icn+1,NCORN)+1_ink
        DO iel=1,ilsize
          rd=0.0_rlk
          iell=ielel(ilfcl,iel)
          ielr=ielel(ilfcr,iel)
          ifcl=ielfc(ilfcl,iel)
          ifcr=ielfc(ilfcr,iel)
          ii=ilfcl+2_ink*(icn-1_ink)
          IF (cndbasis(ii,iel).GT.0.0_rlk) THEN
            ilnndl=MOD(ifcl+icn,NCORN)+1_ink
            ilnndr=MOD(ifcl-icn+1,NCORN)+1_ink
            rv=cnvar(iLndL,iel)
            rd=cnbasis(ilndl,iel)-0.5_rlk*cndbasis(ii,iel)
            w5=cnbasis(ilndl,iel)+cnbasis(ilndr,iel)
            w6=cnbasis(ilnndl,iell)+cnbasis(ilnndr,iell)
            w1=rv-cnvar(ilnndl,iell)
            w2=cnvar(ilndr,iel)-rv
            w3=ABS(w1)
            w4=ABS(w2)
            w7=SIGN(1.0_rlk,w2)
            w8=(w2*w6*w6+w1*w5*w5)/(w5*w6*(w5+w6))
            grad=w7*MIN(ABS(w8),w3/w6,w4/w5)
            IF (w1*w2.LE.0.0_rlk) grad=0.0_rlk
            rd=cndbasis(ii,iel)*(rv+grad*rd)
          ENDIF
          IF (cndbasis(ii,iel).LT.0.0_rlk) THEN
            ilnndl=MOD(ifcr+icn-2,NCORN)+1_ink
            ilnndr=MODULO(ifcr-icn-1,NCORN)+1_ink
            rv=cnvar(ilndr,iel)
            rd=cnbasis(ilndr,iel)+0.5_rlk*cndbasis(ii,iel)
            w5=cnbasis(ilndl,iel)+cnbasis(ilndr,iel)
            w6=cnbasis(ilnndl,ielr)+cnbasis(ilnndr,ielr)
            w1=rv-cnvar(ilndl,iel)
            w2=cnvar(ilnndr,ielr)-rv
            w3=ABS(w1)
            w4=ABS(w2)
            w7=SIGN(1.0_rlk,w2)
            w8=(w1*w6*w6+w2*w5*w5)/(w5*w6*(w5+w6))
            grad=-w7*MIN(ABS(w8),w3/w5,w4/w6)
            IF (w1*w2.LE.0.0_rlk) grad=0.0_rlk
            rd=cndbasis(ii,iel)*(rv+grad*rd)
          ENDIF
          cnflux(ilndl,iel)=cnflux(ilndl,iel)-rd
          cnflux(ilndr,iel)=cnflux(ilndr,iel)+rd
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE ale_kn_fluxndvl

  SUBROUTINE ale_kn_updateel(id1,id2,ilsize,iasize,ielel,ielfc,elbase0,elbase1,&
&                            cut,fcflux,elflux,elvar)

    ! Argument list
    INTEGER(KIND=ink),                        INTENT(IN)    :: id1,id2,ilsize, &
&                                                              iasize
    INTEGER(KIND=ink),DIMENSION(NFACE,iasize),INTENT(IN)    :: ielel,ielfc
    REAL(KIND=rlk),   DIMENSION(iasize),      INTENT(IN)    :: elbase0,elbase1,&
&                                                              cut
    REAL(KIND=rlk),   DIMENSION(NFACE,iasize),INTENT(IN)    :: fcflux
    REAL(KIND=rlk),   DIMENSION(iasize),      INTENT(OUT)   :: elflux
    REAL(KIND=rlk),   DIMENSION(iasize),      INTENT(INOUT) :: elvar
    ! Local
    INTEGER(KIND=ink) :: iel

    ! calculate total flux
    CALL ale_kn_sumflux(id1,id2,ilsize,iasize,ielel,ielfc,fcflux,elflux)

    ! update variable  
    DO iel=1,ilsize
      IF (elbase1(iel).GT.cut(iel)) THEN
        elvar(iel)=(elvar(iel)*elbase0(iel)+elflux(iel))/elbase1(iel)
      ENDIF
    ENDDO

  END SUBROUTINE ale_kn_updateel  

  SUBROUTINE ale_kn_updatend(iusize,icsize,insize,ielnd,ndbase0,ndbase1,cut,   &
&                            zactive,cnflux,ndflux,ndvar)

    ! Argument list
    INTEGER(KIND=ink),                        INTENT(IN)    :: insize,iusize,  &
&                                                              icsize
    INTEGER(KIND=ink),DIMENSION(NCORN,iCSize),INTENT(IN)    :: ielnd
    REAL(KIND=rlk),   DIMENSION(insize),      INTENT(IN)    :: ndbase0,ndbase1,&
&                                                              cut
    LOGICAL(KIND=lak),DIMENSION(insize),      INTENT(IN)    :: zactive
    REAL(KIND=rlk),   DIMENSION(NCORN,iCSize),INTENT(IN)    :: cnflux
    REAL(KIND=rlk),   DIMENSION(insize),      INTENT(OUT)   :: ndflux
    REAL(KIND=rlk),   DIMENSION(insize),      INTENT(INOUT) :: ndvar
    ! Local
    INTEGER(KIND=ink) :: iel,ind,ii

    ! construct total flux
    ndflux=0.0_rlk
    DO iel=1,icsize
      DO ii=1,NCORN
        ind=ielnd(ii,iel)
        ndflux(ind)=ndflux(ind)+cnflux(ii,iel)
      ENDDO
    ENDDO

    ! update variable
    DO ind=1,iusize
      IF (zactive(ind).AND.(ndbase1(ind).GT.cut(ind))) THEN
        ndvar(ind)=(ndvar(ind)*ndbase0(ind)+ndflux(ind))/ndbase1(ind)
      ENDIF
    ENDDO

  END SUBROUTINE ale_kn_updatend

  SUBROUTINE ale_kn_sumflux(id1,id2,ilsize,iasize,ielel,ielfc,fcflux,elflux)

    ! Argument list
    INTEGER(KIND=ink),                        INTENT(IN)    :: id1,id2,ilsize, &
&                                                              iasize
    INTEGER(KIND=ink),DIMENSION(NFACE,iasize),INTENT(IN)    :: ielel,ielfc
    REAL(KIND=rlk),   DIMENSION(NFACE,iasize),INTENT(IN)    :: fcflux
    REAL(KIND=rlk),   DIMENSION(iasize),      INTENT(OUT)   :: elflux
    ! Local
    INTEGER(KIND=ink) :: i1,i2,j1,j2,iel,iel1,iel2
    REAL(KIND=rlk)    :: w1,w2

    elflux=0.0_rlk
    DO i1=id1,id2
      i2=i1+2_ink
      DO iel=1,ilsize
        iel1=ielel(i1,iel)
        iel2=ielel(i2,iel)
        j1=ielfc(i1,iel)
        j2=ielfc(i2,iel)
        w1=fcflux(j1,iel1)
        w2=fcflux(j2,iel2)
        IF (iel1.EQ.iel) w1=0.0_rlk
        IF (iel2.EQ.iel) w2=0.0_rlk
        elflux(iel)=elflux(iel)-fcflux(i1,iel)-fcflux(i2,iel)+w1+w2
      ENDDO
    ENDDO

  END SUBROUTINE ale_kn_sumflux

END MODULE ale_kn_advectors_mod
