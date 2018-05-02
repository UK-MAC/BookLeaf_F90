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
MODULE ale_kn_advect_mod

  USE dataAPI_kinds_mod, ONLY: ink,lak,rlk
  USE dataAPI_params_mod,ONLY: NCORN,NFACE

  IMPLICIT NONE

  PUBLIC :: ale_kn_updatebasisel,ale_kn_initbasisnd,ale_kn_calcbasisnd,        &
&           ale_kn_fluxbasisnd,ale_kn_massbasisnd,ale_kn_cutbasisnd,           &
&           ale_kn_activend

CONTAINS

  SUBROUTINE ale_kn_updatebasisel(nel,zerocut,dencut,totv,totm,cutv,cutm,elvpr,&
&                                 elmpr,eldpr,elv,elm,eld)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nel
    REAL(KIND=rlk),                  INTENT(IN)    :: zerocut,dencut
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(IN)    :: totv,totm
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(OUT)   :: cutv,cutm,elvpr,elmpr,   &
&                                                     eldpr
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(INOUT) :: elv,elm,eld
    ! Local
    INTEGER(KIND=ink) :: iel
  
    ! update element basis 
    DO iel=1,nel
      ! store basis variables
      elvpr(iel)=elv(iel)
      elmpr(iel)=elm(iel)
      eldpr(iel)=eld(iel)
      ! construct cut-off's
      cutv(iel)=zerocut
      cutm(iel)=elvpr(iel)*dencut
      ! volume
      elv(iel)=elv(iel)+totv(iel)
      ! mass
      elm(iel)=elm(iel)+totm(iel)
      ! density
      eld(iel)=elm(iel)/elv(iel)
    ENDDO

  END SUBROUTINE ale_kn_updatebasisel

  SUBROUTINE ale_kn_initbasisnd(nnd,ndv0,ndv1,ndm0)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nnd
    REAL(KIND=rlk),   DIMENSION(nnd),INTENT(OUT) :: ndv0,ndv1,ndm0
    ! Local
    INTEGER(KIND=ink) :: ind

    ! initialise basis
    DO ind=1,nnd
      ndv0(ind)=0.0_rlk
      ndv1(ind)=0.0_rlk
      ndm0(ind)=0.0_rlk
    ENDDO

  END SUBROUTINE ale_kn_initbasisnd

  SUBROUTINE ale_kn_calcbasisnd(nel,nnd,ielnd,ielsort,elv0,elv1,cnm1,ndv0,ndv1, &
&                               ndm0,cnm0)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: nel,nnd
    INTEGER(KIND=ink),DIMENSION(NCORN,nel),INTENT(IN)    :: ielnd
    INTEGER(KIND=ink),DIMENSION(nel),      INTENT(IN)    :: ielsort
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN)    :: elv0,elv1
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)    :: cnm1
    REAL(KIND=rlk),   DIMENSION(nnd),      INTENT(INOUT) :: ndv0,ndv1,ndm0
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(OUT)   :: cnm0
    ! Local
    INTEGER(KIND=ink) :: ii,iel,ind
    REAL(KIND=rlk)    :: w1,w2,w3

    ! construct pre/post nodal volumes and pre nodal/corner mass
    DO ii=1,nel
      iel=ielsort(ii)
      w1=0.25_rlk*elv0(iel)
      w2=0.25_rlk*elv1(iel)
      w3=cnm1(1,iel)
      cnm0(1,iel)=w3
      ind=ielnd(1,iel)
      ndv0(ind)=ndv0(ind)+w1
      ndv1(ind)=ndv1(ind)+w2
      ndm0(ind)=ndm0(ind)+w3
      w3=cnm1(2,iel)
      cnm0(2,iel)=w3
      ind=ielnd(2,iel)
      ndv0(ind)=ndv0(ind)+w1
      ndv1(ind)=ndv1(ind)+w2
      ndm0(ind)=ndm0(ind)+w3
      w3=cnm1(3,iel)
      cnm0(3,iel)=w3
      ind=ielnd(3,iel)
      ndv0(ind)=ndv0(ind)+w1
      ndv1(ind)=ndv1(ind)+w2
      ndm0(ind)=ndm0(ind)+w3
      w3=cnm1(4,iel)
      cnm0(4,iel)=w3
      ind=ielnd(4,iel)
      ndv0(ind)=ndv0(ind)+w1
      ndv1(ind)=ndv1(ind)+w2
      ndm0(ind)=ndm0(ind)+w3
    ENDDO

  END SUBROUTINE ale_kn_calcbasisnd

  SUBROUTINE ale_kn_fluxbasisnd(id1,id2,nel,ielel,ielsd,ielsort,fcdv,fcdm,cndv, &
&                               cndm,cnflux)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: id1,id2,nel
    INTEGER(KIND=ink),DIMENSION(NFACE,nel),INTENT(IN)  :: ielel,ielsd
    INTEGER(KIND=ink),DIMENSION(nel),      INTENT(IN)  :: ielsort
    REAL(KIND=rlk),   DIMENSION(NFACE,nel),INTENT(IN)  :: fcdv,fcdm
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(OUT) :: cndv,cndm,cnflux
    ! Local
    INTEGER(KIND=ink) :: ii,i1,i2,iel,ie1,ie2,is1,is2
    REAL(KIND=rlk)    :: w1,w2,w3,w4

    ! initialise flux
    DO iel=1,nel
      cnflux(1,iel)=0.0_rlk
      cnflux(2,iel)=0.0_rlk
      cnflux(3,iel)=0.0_rlk
      cnflux(4,iel)=0.0_rlk
    ENDDO

    ! construct volume and mass flux
    DO i1=id1,id2
      i2=i1+2_ink
      DO ii=1,nel
        iel=ielsort(ii)
        ie1=ielel(i1,iel)
        ie2=ielel(i2,iel)
        is1=ielsd(i1,iel)
        is2=ielsd(i2,iel)
        w1=fcdv(is1,ie1)
        w2=fcdv(is2,ie2)
        w3=fcdm(is1,ie1)
        w4=fcdm(is2,ie2)
        IF (ie1.EQ.iel) THEN
          w1=0.0_rlk
          w3=0.0_rlk
        ENDIF
        IF (ie2.EQ.iel) THEN
          w2=0.0_rlk
          w4=0.0_rlk
        ENDIF
        w1=w1-fcdv(i1,iel)
        w2=w2-fcdv(i2,iel)
        w1=0.25_rlk*(w1-w2)
        cndv(i1,iel)=w1
        cndv(i2,iel)=w1
        w1=w3-fcdm(i1,iel)
        w2=w4-fcdm(i2,iel)
        w3=0.25_rlk*(w1-w2)
        cndm(i1,iel)=w3
        cndm(i2,iel)=w3
        w3=0.25_rlk*(w1+w2)
        cnflux(1,iel)=cnflux(1,iel)+w3
        cnflux(2,iel)=cnflux(2,iel)+w3
        cnflux(3,iel)=cnflux(3,iel)+w3
        cnflux(4,iel)=cnflux(4,iel)+w3
      ENDDO
    ENDDO

  END SUBROUTINE ale_kn_fluxbasisnd

  SUBROUTINE ale_kn_massbasisnd(nel,nnd,ielnd,ielsort,cnflux,cnm1,ndm1)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: nel,nnd
    INTEGER(KIND=ink),DIMENSION(NCORN,nel),INTENT(IN)    :: ielnd
    INTEGER(KIND=ink),DIMENSION(nel),      INTENT(IN)    :: ielsort
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)    :: cnflux
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(INOUT) :: cnm1
    REAL(KIND=rlk),   DIMENSION(nnd),      INTENT(INOUT) :: ndm1
    ! Local
    INTEGER(KIND=ink) :: ii,iel,ind

    ! construct post nodal/corner mass
    DO ii=1,nel
      iel=ielsort(ii)
      cnm1(1,iel)=cnm1(1,iel)+cnflux(1,iel)
      cnm1(2,iel)=cnm1(2,iel)+cnflux(2,iel)
      cnm1(3,iel)=cnm1(3,iel)+cnflux(3,iel)
      cnm1(4,iel)=cnm1(4,iel)+cnflux(4,iel)
      ind=ielnd(1,iel)
      ndm1(ind)=ndm1(ind)+cnflux(1,iel)
      ind=ielnd(2,iel)
      ndm1(ind)=ndm1(ind)+cnflux(2,iel)
      ind=ielnd(3,iel)
      ndm1(ind)=ndm1(ind)+cnflux(3,iel)
      ind=ielnd(4,iel)
      ndm1(ind)=ndm1(ind)+cnflux(4,iel)
    ENDDO

  END SUBROUTINE ale_kn_massbasisnd

  SUBROUTINE ale_kn_cutbasisnd(nnd,cut,dencut,ndv0,cutv,cutm)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nnd
    REAL(KIND=rlk),                  INTENT(IN)  :: cut,dencut
    REAL(KIND=rlk),   DIMENSION(nnd),INTENT(IN)  :: ndv0
    REAL(KIND=rlk),   DIMENSION(nnd),INTENT(OUT) :: cutv,cutm
    ! Local
    INTEGER(KIND=ink) :: ind

    ! construct cut-offs
    DO ind=1,nnd
      cutv(ind)=cut
      cutm(ind)=dencut*ndv0(ind)
    ENDDO

  END SUBROUTINE ale_kn_cutbasisnd

  SUBROUTINE ale_kn_activend(nnd,ibc,indstatus,indtype,zactive)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nnd,ibc
    INTEGER(KIND=ink),DIMENSION(nnd),INTENT(IN)  :: indstatus,indtype
    LOGICAL(KIND=lak),DIMENSION(nnd),INTENT(OUT) :: zactive
    ! Local
    INTEGER(KIND=ink) :: ind

    ! Set active flag
    DO ind=1,nnd
      IF ((indstatus(ind).GT.0_ink).AND.(indtype(ind).NE.ibc).AND.             &
&         (indtype(ind).NE.-3_ink)) THEN
        zactive(ind)=.TRUE._lak
      ELSE
        zactive(ind)=.FALSE._lak
      ENDIF
    ENDDO

  END SUBROUTINE ale_kn_activend

END MODULE ale_kn_advect_mod
