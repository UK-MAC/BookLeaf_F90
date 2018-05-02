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
MODULE geometry_kn_position_mod

  USE dataAPI_kinds_mod,ONLY: ink,rlk

  IMPLICIT NONE

  PUBLIC  :: geometry_kn_getvertex

CONTAINS

  SUBROUTINE geometry_kn_getvertex(nnd,dt,ndx,ndy,ndu,ndv)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nnd
    REAL(KIND=rlk),                  INTENT(IN)    :: dt
    REAL(KIND=rlk),   DIMENSION(nnd),INTENT(IN)    :: ndu,ndv
    REAL(KIND=rlk),   DIMENSION(nnd),INTENT(INOUT) :: ndx,ndy
    ! Local
    INTEGER(KIND=ink) :: ind

    DO ind=1,nnd
      ndx(ind)=ndx(ind)+dt*ndu(ind)
      ndy(ind)=ndy(ind)+dt*ndv(ind)
    ENDDO

  END SUBROUTINE geometry_kn_getvertex

END MODULE geometry_kn_position_mod
