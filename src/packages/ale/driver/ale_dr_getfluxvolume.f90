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
MODULE ale_dr_getfluxvolume_mod

  USE dataAPI_types_mod,         ONLY: ale_t,runtime_t,data_t
  USE dataAPI_id_mod,            ONLY: ielndid,ndxid,ndyid
  USE dataAPI_alestepid_mod,     ONLY: store2id,store3id,store4id,store5id,    &
&                                      fcdvid
  USE timerAPI_types_mod,        ONLY: timer_t
  USE timer_advance_mod,         ONLY: timer_start,timer_end
  USE ale_kn_getmeshvelocity_mod,ONLY: ale_kn_getmeshvelocity
  USE geometry_kn_position_mod,  ONLY: geometry_kn_getvertex
  USE geometry_kn_volume_mod,    ONLY: geometry_kn_getfluxvolume
  USE utils_kn_copy_mod,         ONLY: utils_kn_copy

  IMPLICIT NONE

  PUBLIC  :: ale_dr_getfluxvolume

CONTAINS

  SUBROUTINE ale_dr_getfluxvolume(ale,runtime,timer,dh)

    ! Argument list
    TYPE(ale_t),                 INTENT(IN)    :: ale
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(timer_t),               INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    ! Timer
    CALL timer_start(timer)

    ! calculate mesh velocity
    CALL ale_kn_getmeshvelocity(runtime%sizes%nnd,ale%zeul,dh(store4id)%raddr, &
&                               dh(store5id)%raddr)

    ! store current position
    CALL utils_kn_copy(runtime%sizes%nnd,dh(ndxid)%raddr,dh(ndyid)%raddr,      &
&                      dh(store2id)%raddr,dh(store3id)%raddr)

    ! construct new position
    CALL geometry_kn_getvertex(runtime%sizes%nnd,runtime%timestep%dt,          &
&                              dh(ndxid)%raddr,dh(ndyid)%raddr,                &
&                              dh(store4id)%raddr,dh(store5id)%raddr)

    ! construct flux volumes
    CALL geometry_kn_getfluxvolume(runtime%sizes%nnd,runtime%sizes%nel,        &
&                                  ale%global%zerocut,dh(ielndid)%iaddr,       &
&                                  dh(store2id)%raddr,dh(store3id)%raddr,      &
&                                  dh(ndxid)%raddr,dh(ndyid)%raddr,            &
&                                  dh(fcdvid)%raddr)

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE ale_dr_getfluxvolume

END MODULE ale_dr_getfluxvolume_mod
