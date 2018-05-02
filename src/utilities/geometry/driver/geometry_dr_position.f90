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
MODULE geometry_dr_position_mod

  USE dataAPI_kinds_mod,       ONLY: rlk
  USE dataAPI_types_mod,       ONLY: runtime_t,data_t
  USE dataAPI_id_mod,          ONLY: ndxid,ndyid,nduid,ndvid
  USE geometry_kn_position_mod,ONLY: geometry_kn_getvertex

  IMPLICIT NONE

  PUBLIC  :: geometry_dr_getvertex

CONTAINS

  SUBROUTINE geometry_dr_getvertex(runtime,dh)

    ! Argument list
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    ! Update vextex positions 
    CALL geometry_kn_getvertex(runtime%sizes%nnd,0.5_rlk*runtime%timestep%dt,  &
&                              dh(ndxid)%raddr,dh(ndyid)%raddr,dh(nduid)%raddr,&
&                              dh(ndvid)%raddr)

  END SUBROUTINE geometry_dr_getvertex

END MODULE geometry_dr_position_mod
