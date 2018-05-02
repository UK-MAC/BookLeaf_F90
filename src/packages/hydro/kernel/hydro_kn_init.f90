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
MODULE hydro_kn_init_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN

  IMPLICIT NONE

  PUBLIC :: hydro_kn_viscinit,hydro_kn_spmassinit

CONTAINS

  SUBROUTINE hydro_kn_viscinit(nel,elvisc,edviscx,edviscy)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nel
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(OUT) :: elvisc
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(OUT) :: edviscx,edviscy
    ! Local
    INTEGER(KIND=ink) :: iel

    DO iel=1,nel
      elvisc(iel)=0.0_rlk
      edviscx(1:NCORN,iel)=0.0_rlk
      edviscy(1:NCORN,iel)=0.0_rlk
    ENDDO

  END SUBROUTINE hydro_kn_viscinit

  SUBROUTINE hydro_kn_spmassinit(nel,eldensity,cnx,cny,spmass)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nel
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN)  :: eldensity
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)  :: cnx,cny
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(OUT) :: spmass
    ! Local
    INTEGER(KIND=ink) :: iel,j1,j2
    REAL(KIND=rlk)    :: x1,x2,x3,x4,y1,y2,y3,y4,w1,w2,w3,w4

    DO iel=1,nel
      x3=0.25_rlk*(cnx(1,iel)+cnx(2,iel)+cnx(3,iel)+cnx(4,iel))
      y3=0.25_rlk*(cny(1,iel)+cny(2,iel)+cny(3,iel)+cny(4,iel))
      DO j1=1,NCORN
        x1=cnx(j1,iel)
        y1=cny(j1,iel)
        j2=MOD(j1,NCORN)+1_ink
        x2=0.5_rlk*(x1+cnx(j2,iel))
        y2=0.5_rlk*(y1+cny(j2,iel))
        j2=MOD(j1+2,NCORN)+1_ink
        x4=0.5_rlk*(x1+cnx(j2,iel))
        y4=0.5_rlk*(y1+cny(j2,iel))
        w1=0.25_rlk*(-x1+x2+x3-x4)
        w2=0.25_rlk*(-x1-x2+x3+x4)
        w3=0.25_rlk*(-y1+y2+y3-y4)
        w4=0.25_rlk*(-y1-y2+y3+y4)
        spmass(j1,iel)=4.0_rlk*eldensity(iel)*(w1*w4-w2*w3)
      ENDDO
    ENDDO

  END SUBROUTINE hydro_kn_spmassinit

END MODULE hydro_kn_init_mod
