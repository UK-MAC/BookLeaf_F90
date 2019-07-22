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
MODULE init_kn_mod

  USE dataAPI_kinds_mod,        ONLY: ink,rlk
  USE dataAPI_params_mod,       ONLY: NCORN,NFACE
  USE utils_kn_connectivity_mod,ONLY: utils_kn_getconn,utils_kn_getsconn,      &
&                                     utils_kn_corrconn

  IMPLICIT NONE

  PUBLIC :: init_kn_cnmass,init_kn_connectivity,init_kn_nodetype,              &
&           init_kn_serialghosts

CONTAINS

  SUBROUTINE init_kn_cnmass(nel,eldensity,cnwt,cnmass)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nel
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN)  :: eldensity
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)  :: cnwt
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(OUT) :: cnmass
    ! Local
    INTEGER(KIND=ink) :: iel

    DO iel=1,nel
      cnmass(1:NCORN,iel)=eldensity(iel)*cnwt(1:NCORN,iel)
    ENDDO

  END SUBROUTINE init_kn_elmass

  SUBROUTINE init_kn_connectivity(nel,ielnd,ielel,ielsd)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nel
    INTEGER(KIND=ink),DIMENSION(NCORN,nel),INTENT(IN)  :: ielnd
    INTEGER(KIND=ink),DIMENSION(NFACE,nel),INTENT(OUT) :: ielel,ielsd

    ielel(:,:)=utils_kn_getconn(nel,ielnd(:,:))
    ielsd(:,:)=utils_kn_getsconn(nel,ielel(:,:))
    CALL utils_kn_corrconn(nel,ielel(:,:),ielsd(:,:))
    
  END SUBROUTINE init_kn_connectivity

  SUBROUTINE init_kn_nodetype(nel,nnd,ielnd,indtype)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: nel,nnd
    INTEGER(KIND=ink),DIMENSION(NCORN,nel),INTENT(IN)    :: ielnd
    INTEGER(KIND=ink),DIMENSION(nnd),      INTENT(INOUT) :: indtype
    ! Local
    INTEGER(KIND=ink)                       :: iel,ii,jj,j1,j2
    INTEGER(KIND=ink),DIMENSION(0:NCORN-1) :: nodes

    DO iel=1,nel
      nodes(0:NCORN-1)=ielnd(1:NCORN,iel)
      IF (COUNT(indtype(nodes).LT.0_ink).EQ.3_ink) THEN
        l1:DO ii=0,NCORN-1
          IF (indtype(nodes(ii)).GT.0_ink) EXIT l1
        ENDDO l1
        ii=MOD(ii+2,NCORN)
        jj=nodes(ii)
        IF (jj.LE.nnd) THEN
          j1=nodes(MOD(ii+1,NCORN))
          j2=nodes(MOD(ii+3,NCORN))
          IF (((indtype(j1).EQ.-2_ink).AND.(indtype(j2).EQ.-1_ink)).OR.        &
&             ((indtype(j2).EQ.-2_ink).AND.(indtype(j1).EQ.-1_ink)))           &
           indtype(jj)=-3_ink
        ENDIF
      ENDIF
    ENDDO

  END SUBROUTINE init_kn_nodetype

  SUBROUTINE init_kn_serialghosts(nel,nel1,nel2,nnd,nnd1,nnd2)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)  :: nel,nnd
    INTEGER(KIND=ink),INTENT(OUT) :: nel1,nel2,nnd1,nnd2

    nel1=nel
    nel2=nel
    nnd1=nnd
    nnd2=nnd

  END SUBROUTINE init_kn_serialghosts

END MODULE init_kn_mod
