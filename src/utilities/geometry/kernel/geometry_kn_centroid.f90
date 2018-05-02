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
MODULE geometry_kn_centroid_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NDIM

  IMPLICIT NONE

  PUBLIC  :: geometry_kn_getcentroid

CONTAINS

  PURE FUNCTION geometry_kn_getcentroid(nVert,xx,yy) RESULT(centroid)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN) :: nVert
    REAL(KIND=rlk),   DIMENSION(nVert),INTENT(IN) :: xx,yy
    ! Result
    REAL(KIND=rlk),   DIMENSION(NDIM) :: centroid
    ! Local
    INTEGER(KIND=ink) :: i1,i2
    REAL(KIND=rlk)    :: rVol,rr

    ! initialise
    centroid(:)=0.0_rlk
    rVol=0.0_rlk

    ! calulate centroid
    DO i1=1,nVert
      i2=MOD(i1,nVert)+1_ink
      rr=xx(i1)*yy(i2)-xx(i2)*yy(i1)
      rVol=rVol+rr
      centroid(1)=centroid(1)+rr*(xx(i1)+xx(i2))
      centroid(2)=centroid(2)+rr*(yy(i1)+yy(i2))
    ENDDO
    rVol=0.5_rlk*rVol
    centroid(:)=MERGE(centroid(:)/(6.0_rlk*rVol),0.0_rlk,rVol.GT.0.0_rlk)

  END FUNCTION geometry_kn_getcentroid

END MODULE geometry_kn_centroid_mod
