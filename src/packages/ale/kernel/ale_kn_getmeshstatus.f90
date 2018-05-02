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
MODULE ale_kn_getmeshstatus_mod

  USE dataAPI_kinds_mod,ONLY: ink,lok

  IMPLICIT NONE

  PUBLIC :: ale_kn_getmeshstatus

CONTAINS

  SUBROUTINE ale_kn_getmeshstatus(nnd,zeul,indstatus)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nnd
    LOGICAL(KIND=lok),               INTENT(IN)  :: zeul
    INTEGER(KIND=ink),DIMENSION(nnd),INTENT(OUT) :: indstatus
    ! Local
    INTEGER(KIND=ink) :: ind

    IF (zeul) THEN
      DO ind=1,nnd
        indstatus(ind)=2_ink
      ENDDO
    ELSE
      ! Other options
    ENDIF

  END SUBROUTINE ale_kn_getmeshstatus

END MODULE ale_kn_getmeshstatus_mod
