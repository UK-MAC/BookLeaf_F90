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
MODULE hydro_kn_getdt_mod

  USE dataAPI_kinds_mod,     ONLY: ink,lok,rlk
  USE dataAPI_params_mod,    ONLY: NCORN,SUCCESS,FAILURE
  USE geometry_kn_length_mod,ONLY: geometry_kn_dlm,geometry_kn_dln

  IMPLICIT NONE

  PUBLIC :: hydro_kn_getdtcfl,hydro_kn_getdtdiv

CONTAINS

  SUBROUTINE hydro_kn_getdtcfl(nreg,nel,zcut,cfl_sf,zdtnotreg,zmidlength,      &
&                              ielreg,elcs2,cnx,cny,rscratch11,rscratch12,rdt, &
&                              idt,sdt,ierr)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nreg,nel
    REAL(KIND=rlk),                        INTENT(IN)  :: zcut,cfl_sf
    LOGICAL(KIND=lok),DIMENSION(nreg),     INTENT(IN)  :: zdtnotreg,zmidlength    
    INTEGER(KIND=ink),DIMENSION(nel),      INTENT(IN)  :: ielreg
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN)  :: elcs2
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)  :: cnx,cny
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(OUT) :: rscratch11,rscratch12
    INTEGER(KIND=ink),                     INTENT(OUT) :: idt,ierr
    REAL(KIND=rlk),                        INTENT(OUT) :: rdt
    CHARACTER(LEN=8),                      INTENT(OUT) :: sdt
    ! Local
    INTEGER(KIND=ink) :: iel,ireg,ii
    REAL(KIND=rlk)    :: w1

    ! initialise
    ierr=SUCCESS

    ! calculate CFL condition
    DO iel=1,nel
      ireg=ielreg(iel)
      IF (zdtnotreg(ireg)) THEN
        rscratch11(iel)=HUGE(1.0_rlk)
        rscratch12(iel)=TINY(1.0_rlk)
      ELSE
        IF (zmidlength(ireg)) THEN
          w1=MINVAL(geometry_kn_dlm(cnx(:,iel),cny(:,iel)))
        ELSE
          w1=MINVAL(geometry_kn_dln(cnx(:,iel),cny(:,iel),zcut))
        ENDIF
        rscratch11(iel)=w1/elcs2(iel)
        rscratch12(iel)=w1
      ENDIF
    ENDDO

    ! find minimum CFL condition
    ii=1_ink
    DO iel=2,nel
      IF (rscratch11(iel).LT.rscratch11(ii)) ii=iel
    ENDDO
    w1=rscratch11(ii)
    IF (w1.LT.0.0_rlk) THEN
      ierr=FAILURE
      RETURN
    ENDIF

    ! set return variables
    rdt=cfl_sf*SQRT(w1)
    idt=ii
    sdt='     CFL'

  END SUBROUTINE hydro_kn_getdtcfl

  SUBROUTINE hydro_kn_getdtdiv(nel,div_sf,a1,a3,b1,b3,elvolume,cnu,cnv,rdt,idt,&
&                              sdt)  

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nel
    REAL(KIND=rlk),                        INTENT(IN)  :: div_sf
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN)  :: a1,a3,b1,b3,elvolume
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)  :: cnu,cnv
    REAL(KIND=rlk),                        INTENT(OUT) :: rdt
    INTEGER(KIND=ink),                     INTENT(OUT) :: idt
    CHARACTER(LEN=8),                      INTENT(OUT) :: sdt
    ! Local
    INTEGER(KIND=ink) :: iel,ii
    REAL(KIND=rlk)    :: w1,w2

    ! Initialise
    w2=TINY(1.0_rlk)
    ii=0_ink
    ! Calculate divergence timestep control
    DO iel=1,nel
      w1=cnu(1,iel)*(-b3(iel)+b1(iel))+cnv(1,iel)*( a3(iel)-a1(iel))+          &
&        cnu(2,iel)*( b3(iel)+b1(iel))+cnv(2,iel)*(-a3(iel)-a1(iel))+          &
&        cnu(3,iel)*( b3(iel)-b1(iel))+cnv(3,iel)*(-a3(iel)+a1(iel))+          &
&        cnu(4,iel)*(-b3(iel)-b1(iel))+cnv(4,iel)*( a3(iel)+a1(iel))
      w1=ABS(w1)/elvolume(iel)
      IF (w1.GT.w2) THEN
        w2=w1
        ii=iel
      ENDIF
    ENDDO
    ! Set return variables
    rdt=div_sf/w2
    idt=ii
    sdt='     DIV'

  END SUBROUTINE hydro_kn_getdtdiv

END MODULE hydro_kn_getdt_mod
