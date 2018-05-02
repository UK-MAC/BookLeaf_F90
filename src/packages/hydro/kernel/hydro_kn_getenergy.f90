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
MODULE hydro_kn_getenergy_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN

  IMPLICIT NONE

  PUBLIC :: hydro_kn_getenergy

CONTAINS

  SUBROUTINE hydro_kn_getenergy(nel,dt,zerocut,elenergy,elmass,cnfx,cnfy,cnu,  &
&                               cnv)

    ! Argument list
    INTEGER(KIND=ink),                  INTENT(IN)    :: nel
    REAL(KIND=rlk),                     INTENT(IN)    :: dt,zerocut
    REAL(KIND=rlk),DIMENSION(nel),      INTENT(IN)    :: elmass
    REAL(KIND=rlk),DIMENSION(nel),      INTENT(INOUT) :: elenergy
    REAL(KIND=rlk),DIMENSION(NCORN,nel),INTENT(IN)    :: cnfx,cnfy,cnu,cnv
    ! Local
    INTEGER(KIND=ink) :: iel
    REAL(KIND=rlk)    :: w1

    ! FdS internal energy update
    DO iel=1,nel
      w1=cnfx(1,iel)*cnu(1,iel)+cnfy(1,iel)*cnv(1,iel)+                        &
&        cnfx(2,iel)*cnu(2,iel)+cnfy(2,iel)*cnv(2,iel)+                        &
&        cnfx(3,iel)*cnu(3,iel)+cnfy(3,iel)*cnv(3,iel)+                        &
&        cnfx(4,iel)*cnu(4,iel)+cnfy(4,iel)*cnv(4,iel)
      w1=-w1/MAX(elmass(iel),zerocut)
      elenergy(iel)=elenergy(iel)+w1*dt
    ENDDO

  END SUBROUTINE hydro_kn_getenergy

END MODULE hydro_kn_getenergy_mod
