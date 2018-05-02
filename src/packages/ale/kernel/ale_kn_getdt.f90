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
MODULE ale_kn_getdt_mod

  USE dataAPI_kinds_mod, ONLY: ink,lok,rlk
  USE dataAPI_params_mod,ONLY: NCORN

  IMPLICIT NONE

  PUBLIC :: ale_kn_getdt

CONTAINS

  SUBROUTINE ale_kn_getdt(nel,zerocut,ale_sf,zeul,cnu,cnv,ellength,rdt,idt,sdt)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)  :: nel
    REAL(KIND=rlk),                        INTENT(IN)  :: zerocut,ale_sf
    LOGICAL(KIND=lok),                     INTENT(IN)  :: zeul
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN)  :: cnu,cnv
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN)  :: ellength
    REAL(KIND=rlk),                        INTENT(OUT) :: rdt
    INTEGER(KIND=ink),                     INTENT(OUT) :: idt
    CHARACTER(LEN=8),                      INTENT(OUT) :: sdt
    ! Local
    INTEGER(KIND=ink) :: iel,ii
    REAL(KIND=rlk)    :: w1,w2

    ! Initialise
    w2=HUGE(1.0_rlk)

    ! Calculate ALE timestep control
    IF (zeul) THEN
      DO iel=1,nel
        w1=MAX(cnu(1,iel)*cnu(1,iel)+cnv(1,iel)*cnv(1,iel),                    &
&              cnu(2,iel)*cnu(2,iel)+cnv(2,iel)*cnv(2,iel),                    &
&              cnu(3,iel)*cnu(3,iel)+cnv(3,iel)*cnv(3,iel),                    &
&              cnu(4,iel)*cnu(4,iel)+cnv(4,iel)*cnv(4,iel))
        w1=ellength(iel)/MAX(w1,zerocut)
        IF (w1.LT.w2) THEN
          w2=w1
          ii=iel
        ENDIF
      ENDDO
    ELSE
      ! Other options
    ENDIF

    ! Set return variables
    rdt=ale_sf*SQRT(w2)
    idt=ii
    sdt='     ALE'

  END SUBROUTINE ale_kn_getdt

END MODULE ale_kn_getdt_mod
