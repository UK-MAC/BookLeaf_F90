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
MODULE modify_mod

  USE dataAPI_types_mod, ONLY: config_t,runtime_t,data_t
  USE dataAPI_kinds_mod, ONLY: ink,lok,rlk
  USE dataAPI_params_mod,ONLY: NCORN
  USE dataAPI_id_mod,    ONLY: ielndid,ndxid,ndyid,nduid
  USE typhon_API_mod,    ONLY: TYPH_reduce,TYPH_OP_MIN

  IMPLICIT NONE

  PRIVATE :: saltzmann
  PUBLIC  :: modify

CONTAINS

  SUBROUTINE saltzmann(nel,nnd,comms,zmpi,ielnd,ndy,ndx,ndu)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: nel,nnd,comms
    LOGICAL(KIND=lok),                     INTENT(IN)    :: zmpi
    INTEGER(KIND=ink),DIMENSION(NCORN,nel),INTENT(IN)    :: ielnd
    REAL(KIND=rlk),   DIMENSION(nnd),      INTENT(IN)    :: ndy
    REAL(KIND=rlk),   DIMENSION(nnd),      INTENT(INOUT) :: ndx,ndu
    ! Local
    INTEGER(KIND=ink)                :: iel,ind,ii,ierr
    REAL(KIND=rlk),   PARAMETER      :: TOL=1.0e-6_rlk,DX1=0.01_rlk,           &
&                                       DY1=0.01_rlk
    REAL(KIND=rlk)                   :: pi,x0,y0,w1,w2
    LOGICAL(KIND=lok),DIMENSION(nnd) :: zchanged

    ! Alter mesh positions
    pi=4.0_rlk*ATAN(1.0_rlk)
    zchanged(:)=.FALSE._lok
    DO iel=1,nel
      DO ii=1,NCORN
        ind=ielnd(ii,iel)
        IF (.NOT.zchanged(ind)) THEN
          x0=ndx(ind)
          y0=ndy(ind)
          w1=100.0_rlk*x0+TOL
          w2=100.0_rlk*y0+TOL
          ndx(ind)=w1*DX1+(10.0_rlk-w2)*DY1*SIN(pi*w1*0.01_rlk)
          zchanged(ind)=.TRUE._lok
        ENDIF
      ENDDO
    ENDDO

    ! Set left hand boundary velocity
    w1=ndx(1)
    DO ind=2,nnd
      IF (ndx(ind).LT.w1) w1=ndx(ind)
    ENDDO
    IF (zmpi) THEN
      ierr=TYPH_reduce(w1,RVal=w2,Op=TYPH_OP_MIN,Comm=comms)
      w1=w2
    ENDIF
    w1=w1+TOL
    DO iel=1,nel
      DO ii=1,NCORN
        ind=ielnd(ii,iel)
        IF (ndx(ind).LE.w1) ndu(ind)=1.0_rlk
      ENDDO
    ENDDO

  END SUBROUTINE saltzmann

  SUBROUTINE modify(config,runtime,dh)

    ! Argument list
    TYPE(config_t),              INTENT(IN)    :: config
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    CALL saltzmann(runtime%sizes%nel,runtime%sizes%nnd,                        &
&                  config%comm%spatial%comm,config%comm%zmpi,dh(ielndid)%iaddr,&
&                  dh(ndyid)%raddr,dh(ndxid)%raddr,dh(nduid)%raddr)

  END SUBROUTINE modify

END MODULE modify_mod
