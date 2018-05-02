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
MODULE hydro_kn_tensorviscosity_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN,NFACE

  IMPLICIT NONE

  INTEGER(KIND=ink),              PARAMETER,PRIVATE :: NQ=9_ink
  REAL(KIND=rlk),                 PARAMETER,PRIVATE :: DN1DX0=-0.25_rlk,       &
&                                                      DN2DX0=0.25_rlk,        &
&                                                      DN3DX0=0.25_rlk,        &
&                                                      DN4DX0=-0.25_rlk,       &
&                                                      DN1DE0=-0.25_rlk,       &
&                                                      DN2DE0=-0.25_rlk,       &
&                                                      DN3DE0=0.25_rlk,        &
&                                                      DN4DE0=0.25_rlk
  REAL(KIND=rlk),   DIMENSION(NQ),PARAMETER,PRIVATE :: XI =[-1.0_rlk,-1.0_rlk, &
&  -1.0_rlk,0.0_rlk,0.0_rlk,0.0_rlk,1.0_rlk,1.0_rlk,1.0_rlk],                  &
&                                                      ETA=[-1.0_rlk,0.0_rlk,  &
&  1.0_rlk,-1.0_rlk,0.0_rlk,1.0_rlk,-1.0_rlk,0.0_rlk,1.0_rlk],                 &
&                                                      WGT=[2.0_rlk/3.0_rlk,   &
&  5.0_rlk/3.0_rlk,2.0_rlk/3.0_rlk,5.0_rlk/3.0_rlk,8.0_rlk/3.0_rlk,            &
&  5.0_rlk/3.0_rlk,2.0_rlk/3.0_rlk,5.0_rlk/3.0_rlk,2.0_rlk/3.0_rlk],           &
&                                                      DN1DXI =[-0.5_rlk,      &
&  -0.25_rlk,0.0_rlk,-0.5_rlk,-0.25_rlk,0.0_rlk,-0.5_rlk,-0.25_rlk,0.0_rlk],   &
&                                                      DN2DXI =[0.5_rlk,       &
&  0.25_rlk,0.0_rlk,0.5_rlk,0.25_rlk,0.0_rlk,0.5_rlk,0.25_rlk,0.0_rlk],        &
&                                                      DN3DXI =[0.0_rlk,       &
&  0.25_rlk,0.5_rlk,0.0_rlk,0.25_rlk,0.5_rlk,0.0_rlk,0.25_rlk,0.5_rlk],        &
&                                                      DN4DXI =[0.0_rlk,       &
&  -0.25_rlk,-0.5_rlk,0.0_rlk,-0.25_rlk,-0.5_rlk,0.0_rlk,-0.25_rlk,-0.5_rlk],  &
&                                                      DN1DETA=[-0.5_rlk,      &
&  -0.5_rlk,-0.5_rlk,-0.25_rlk,-0.25_rlk,-0.25_rlk,0.0_rlk,0.0_rlk,0.0_rlk],   &
&                                                      DN2DETA=[0.0_rlk,       &
&  0.0_rlk,0.0_rlk,-0.25_rlk,-0.25_rlk,-0.25_rlk,-0.5_rlk,-0.5_rlk,-0.5_rlk],  &
&                                                      DN3DETA=[0.0_rlk,       &
&  0.0_rlk,0.0_rlk,0.25_rlk,0.25_rlk,0.25_rlk,0.5_rlk,0.5_rlk,0.5_rlk],        &
&                                                      DN4DETA=[0.5_rlk,       &
&  0.5_rlk,0.5_rlk,0.25_rlk,0.25_rlk,0.25_rlk,0.0_rlk,0.0_rlk,0.0_rlk]

  PUBLIC :: hydro_kn_calctviscosity,hydro_kn_gettviscosity

CONTAINS

  SUBROUTINE hydro_kn_calctviscosity(nel,a1,a2,a3,b1,b2,b3,cnu,cnv,viscx,viscy)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nel
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN)  :: a1,a2,a3,b1,b2,b3
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)  :: cnu,cnv
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(OUT) :: viscx,viscy
    ! Local
    INTEGER(KIND=ink) :: iel,iq
    REAL(KIND=rlk)    :: qx1,qy1,qx2,qy2,qx3,qy3,qx4,qy4,ji11,ji12,ji21,ji22,  &
&                        detj,sx1,sy1,sx2,sy2,sx3,sy3,sx4,sy4,u1,u2,u3,u4,v1,  &
&                        v2,v3,v4

    DO iel=1,nel
      qx1=0.0_rlk
      qy1=0.0_rlk
      qx2=0.0_rlk
      qy2=0.0_rlk
      qx3=0.0_rlk
      qy3=0.0_rlk
      qx4=0.0_rlk
      qy4=0.0_rlk
      u1=cnu(1,iel)
      u2=cnu(2,iel)
      u3=cnu(3,iel)
      u4=cnu(4,iel)
      v1=cnv(1,iel)
      v2=cnv(2,iel)
      v3=cnv(3,iel)
      v4=cnv(4,iel)
      DO iq=1,NQ
        ji11=  b3(iel)+b2(iel)*XI(iq)
        ji12=-(b1(iel)+b2(iel)*ETA(iq))
        ji21=-(a3(iel)+a2(iel)*XI(iq))
        ji22=  a1(iel)+a2(iel)*ETA(iq)
        detj=ji22*ji11-ji12*ji21
        sx1=ji11*DN1DXI(iq)+ji12*DN1DETA(iq)
        sy1=ji21*DN1DXI(iq)+ji22*DN1DETA(iq)
        sx2=ji11*DN2DXI(iq)+ji12*DN2DETA(iq)
        sy2=ji21*DN2DXI(iq)+ji22*DN2DETA(iq)
        sx3=ji11*DN3DXI(iq)+ji12*DN3DETA(iq)
        sy3=ji21*DN3DXI(iq)+ji22*DN3DETA(iq)
        sx4=ji11*DN4DXI(iq)+ji12*DN4DETA(iq)
        sy4=ji21*DN4DXI(iq)+ji22*DN4DETA(iq)
        detj=-WGT(iq)/detj
        ji11=sx1*sx1+sy1*sy1
        ji12=sx1*sx2+sy1*sy2
        ji21=sx1*sx3+sy1*sy3
        ji22=sx1*sx4+sy1*sy4
        qx1=(ji11*u1+ji12*u2+ji21*u3+ji22*u4)*detj+qx1
        qy1=(ji11*v1+ji12*v2+ji21*v3+ji22*v4)*detj+qy1
        ji11=sx2*sx1+sy2*sy1
        ji12=sx2*sx2+sy2*sy2
        ji21=sx2*sx3+sy2*sy3
        ji22=sx2*sx4+sy2*sy4
        qx2=(ji11*u1+ji12*u2+ji21*u3+ji22*u4)*detj+qx2
        qy2=(ji11*v1+ji12*v2+ji21*v3+ji22*v4)*detj+qy2
        ji11=sx3*sx1+sy3*sy1
        ji12=sx3*sx2+sy3*sy2
        ji21=sx3*sx3+sy3*sy3
        ji22=sx3*sx4+sy3*sy4
        qx3=(ji11*u1+ji12*u2+ji21*u3+ji22*u4)*detj+qx3
        qy3=(ji11*v1+ji12*v2+ji21*v3+ji22*v4)*detj+qy3
        ji11=sx4*sx1+sy4*sy1
        ji12=sx4*sx2+sy4*sy2
        ji21=sx4*sx3+sy4*sy3
        ji22=sx4*sx4+sy4*sy4
        qx4=(ji11*u1+ji12*u2+ji21*u3+ji22*u4)*detj+qx4
        qy4=(ji11*v1+ji12*v2+ji21*v3+ji22*v4)*detj+qy4
      ENDDO
      ji11= b3(iel)
      ji12=-b1(iel)
      ji21=-a3(iel)
      ji22= a1(iel)
      sx1=ji11*DN1DX0+ji12*DN1DE0
      sy1=ji21*DN1DX0+ji22*DN1DE0
      sx2=ji11*DN2DX0+ji12*DN2DE0
      sy2=ji21*DN2DX0+ji22*DN2DE0
      sx3=ji11*DN3DX0+ji12*DN3DE0
      sy3=ji21*DN3DX0+ji22*DN3DE0
      sx4=ji11*DN4DX0+ji12*DN4DE0
      sy4=ji21*DN4DX0+ji22*DN4DE0
    ENDDO

  END SUBROUTINE hydro_kn_calctviscosity

  SUBROUTINE hydro_kn_gettviscosity(nsize,cvisc1,cvisc2,avisc,zerocut,cs2,vv,  &
&                                   psi0d,cc,ll,visc,viscx,viscy)

    ! Argument list
    INTEGER(KIND=ink),                         INTENT(IN)    :: nsize
    REAL(KIND=rlk),                            INTENT(IN)    :: cvisc1,cvisc2, &
&                                                               avisc,zerocut
    REAL(KIND=rlk),   DIMENSION(nsize),        INTENT(IN)    :: cs2,vv,psi0d
    REAL(KIND=rlk),   DIMENSION(NFACE/2,nsize),INTENT(IN)    :: cc,ll
    REAL(KIND=rlk),   DIMENSION(nsize),        INTENT(INOUT) :: visc
    REAL(KIND=rlk),   DIMENSION(NCORN,nsize),  INTENT(INOUT) :: viscx,viscy
    ! Local
    INTEGER(KIND=ink)                    :: ii
    REAL(KIND=rlk)                       :: cs,w1
    REAL(KIND=rlk),   DIMENSION(NFACE/2) :: psi1,psi2,mu

    DO ii=1,nsize
      cs=cvisc1*SQRT(cs2(ii))
      w1=psi0d(ii)
      psi1(:)=0.0_rlk
      IF (cc(1,ii).LT.0.0_rlk) psi1(1)=1.0_rlk
      IF (cc(2,ii).LT.0.0_rlk) psi1(2)=1.0_rlk
      psi2(:)=1.0_rlk/(1.0_rlk+avisc*vv(ii)/MIN(cc(:,ii),zerocut))
      mu(:)=psi1(:)*ll(:,ii)*(psi2(:)*cs+cvisc2*ll(:,ii)*ABS(cc(:,ii)))
      visc(ii)=w1*(mu(1)*ABS(cc(1,ii))+mu(2)*ABS(cc(2,ii)))
      w1=w1*(mu(1)+mu(2))
      viscx(:,ii)=w1*viscx(:,ii)
      viscy(:,ii)=w1*viscy(:,ii)
    ENDDO

  END SUBROUTINE hydro_kn_gettviscosity

END MODULE hydro_kn_tensorviscosity_mod
