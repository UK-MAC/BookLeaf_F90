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
MODULE ale_dr_getdt_mod

  USE dataAPI_kinds_mod,   ONLY: ink,rlk
  USE dataAPI_types_mod,   ONLY: ale_t,sizes_t,data_t,dt_t
  USE dataAPI_dtstepid_mod,ONLY: cnuid,cnvid,ellengthid
  USE ale_kn_getdt_mod,    ONLY: ale_kn_getdt

  IMPLICIT NONE

  PUBLIC :: ale_dr_getdt

CONTAINS

  SUBROUTINE ale_dr_getdt(ale,sizes,dh,dt)

    ! Argument list
    TYPE(ale_t),               INTENT(IN)    :: ale
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(IN)    :: dh
    TYPE(dt_t),   POINTER,     INTENT(INOUT) :: dt

    ! Initialise
    ALLOCATE(dt%next)
    dt=>dt%next

    ! Calculate ALE timestep control
    CALL ale_kn_getdt(sizes%nel,ale%global%zerocut,ale%sf,ale%zeul,            &
&                     dh(cnuid)%raddr,dh(cnvid)%raddr,dh(ellengthid)%raddr,    &
&                     dt%rdt,dt%idt,dt%sdt)

  END SUBROUTINE ale_dr_getdt

END MODULE ale_dr_getdt_mod
