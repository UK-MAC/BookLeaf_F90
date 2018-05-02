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
MODULE hydro_kn_getacceleration_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN

  IMPLICIT NONE

  PUBLIC :: hydro_kn_getacceleration,hydro_kn_scatteracceleration,             &
&           hydro_kn_applyacceleration

CONTAINS

  SUBROUTINE hydro_kn_scatteracceleration(nel,nnd,zerocut,ielsort,ielnd,       &
&                                         eldensity,cnwt,cnmass,ndarea,ndmass, &
&                                         cnfx,cnfy,ndudot,ndvdot)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nel,nnd
    REAL(KIND=rlk),                        INTENT(IN)  :: zerocut
    INTEGER(KIND=ink),DIMENSION(nel),      INTENT(IN)  :: ielsort
    INTEGER(KIND=ink),DIMENSION(NCORN,nel),INTENT(IN)  :: ielnd
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN)  :: eldensity
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)  :: cnfx,cnfy,cnwt,cnmass
    REAL(KIND=rlk),   DIMENSION(nnd),      INTENT(OUT) :: ndarea,ndudot,ndmass,&
&                                                         ndvdot
    ! Local
    INTEGER(KIND=ink) :: iel,ind,ii,jj,kk

    DO ind=1,nnd
      ndmass(ind)=0.0_rlk
      ndarea(ind)=0.0_rlk
      ndudot(ind)=0.0_rlk
      ndvdot(ind)=0.0_rlk
    ENDDO

    DO jj=1,NCORN
      DO kk=1,nel
        iel=ielsort(kk)
        ind=ielnd(jj,iel)
        IF (cnmass(jj,iel).GT.zerocut) THEN
          ndmass(ind)=ndmass(ind)+cnmass(jj,iel)
        ELSE
          ii=jj-1_ink
          IF (ii.EQ.0_ink) ii=4_ink
          IF (cnmass(ii,iel).GT.zerocut) THEN
            ndmass(ind)=ndmass(ind)+cnmass(ii,iel)
          ELSE
            ndmass(ind)=ndmass(ind)+eldensity(iel)*cnwt(jj,iel)
          ENDIF
        ENDIF
        ndarea(ind)=ndarea(ind)+cnwt(jj,iel)
        ndudot(ind)=ndudot(ind)+cnfx(jj,iel)
        ndvdot(ind)=ndvdot(ind)+cnfy(jj,iel)
      ENDDO
    ENDDO

  END SUBROUTINE hydro_kn_scatteracceleration

  SUBROUTINE hydro_kn_getacceleration(nnd,dencut,zerocut,ndarea,ndmass,ndudot, &
&                                     ndvdot)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nnd
    REAL(KIND=rlk),                  INTENT(IN)    :: dencut,zerocut
    REAL(KIND=rlk),   DIMENSION(nnd),INTENT(IN)    :: ndarea
    REAL(KIND=rlk),   DIMENSION(nnd),INTENT(INOUT) :: ndmass,ndudot,ndvdot
    ! Local
    INTEGER(KIND=ink) :: ind
    REAL(KIND=rlk)    :: w1

    DO ind=1,nnd
      w1=dencut*ndarea(ind)
      IF (ndmass(ind).GT.w1) THEN
        ndudot(ind)=ndudot(ind)/ndmass(ind)
        ndvdot(ind)=ndvdot(ind)/ndmass(ind)
      ELSE
        ndudot(ind)=0.0_rlk
        ndvdot(ind)=0.0_rlk
        ndmass(ind)=MAX(zerocut,w1)
      ENDIF
    ENDDO
    
  END SUBROUTINE hydro_kn_getacceleration

  SUBROUTINE hydro_kn_applyacceleration(nnd,dt,ndubar,ndvbar,ndu,ndv)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nnd
    REAL(KIND=rlk),                  INTENT(IN)    :: dt
    REAL(KIND=rlk),   DIMENSION(nnd),INTENT(INOUT) :: ndubar,ndvbar,ndu,ndv
    ! Local
    INTEGER(KIND=ink) :: ind
    REAL(KIND=rlk)    :: w1,w2,dt05

    dt05=0.5_rlk*dt
    DO ind=1,nnd
      w1=ndu(ind)
      w2=ndv(ind)
      ndu(ind)=w1+dt*ndubar(ind)
      ndv(ind)=w2+dt*ndvbar(ind)
      ndubar(ind)=w1+dt05*ndubar(ind)
      ndvbar(ind)=w2+dt05*ndvbar(ind)
    ENDDO

  END SUBROUTINE hydro_kn_applyacceleration

END MODULE hydro_kn_getacceleration_mod
