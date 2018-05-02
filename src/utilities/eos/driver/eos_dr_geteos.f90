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
MODULE eos_dr_geteos_mod

  USE eos_kn_geteos_mod, ONLY: eos_kn_geteos
  USE dataAPI_types_mod, ONLY: data_t,eos_t,sizes_t
  USE dataAPI_kinds_mod, ONLY: ink
  USE dataAPI_id_mod,    ONLY: ielmatid,eldensityid,elenergyid,elpressureid,   &
&                              elcs2id,icpmatid,cpdensityid,cpenergyid,        &
&                              cppressureid,cpcs2id
  USE timer_advance_mod, ONLY: timer_start,timer_end
  USE timerAPI_types_mod,ONLY: timer_t

  IMPLICIT NONE

  PUBLIC :: eos_dr_geteos

CONTAINS

  SUBROUTINE eos_dr_geteos(eos,sizes,timer,dh)

    ! Argument list
    TYPE(eos_t),               INTENT(IN)    :: eos
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(timer_t),             INTENT(INOUT) :: timer
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    ! Timer
    CALL timer_start(timer)

    !# Missing code here that can't be merged

    ! Update pressure and sound speed
    CALL eos_kn_geteos(sizes%nel,eos%itype,eos%param,eos%pcut,eos%ccut,        &
&                      dh(ielmatid)%iaddr,dh(eldensityid)%raddr,               &
&                      dh(elenergyid)%raddr,dh(elpressureid)%raddr,            &
&                      dh(elcs2id)%raddr)

    IF (sizes%ncp.GT.0_ink) THEN
      CALL eos_kn_geteos(sizes%ncp,eos%itype,eos%param,eos%pcut,eos%ccut,      &
&                        dh(icpmatid)%iaddr,dh(cpdensityid)%raddr,             &
&                        dh(cpenergyid)%raddr,dh(cppressureid)%raddr,          &
&                        dh(cpcs2id)%raddr)
    ENDIF

    !# Missing code here that can't be merged

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE eos_dr_geteos

END MODULE eos_dr_geteos_mod
