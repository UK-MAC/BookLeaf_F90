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
MODULE ale_cf_types_mod

  USE dataAPI_kinds_mod,ONLY: ink,rlk,lok
  USE global_types_mod, ONLY: global_t
  USE comms_types_mod,  ONLY: comm_t
  USE eos_cf_types_mod, ONLY: eos_t

  IMPLICIT NONE

  TYPE,PUBLIC :: ale_t
    ! Local
    INTEGER(KIND=ink)                            :: npatch,adv_type
    REAL(KIND=rlk)                               :: mintime,maxtime,sf
    LOGICAL(KIND=lok)                            :: zexist,zon,zeul
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: patch_type,patch_motion,   &
&                                                   patch_ntrigger
    REAL(KIND=rlk),   DIMENSION(:),  ALLOCATABLE :: patch_ontime,patch_offtime,&
&                                                   patch_minvel,patch_maxvel, &
&                                                   patch_om
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: patch_trigger
    ! Global
    TYPE(global_t),   POINTER                    :: global
    TYPE(comm_t),     POINTER                    :: comm
    TYPE(eos_t),      POINTER                    :: eos
  END TYPE ale_t

END MODULE ale_cf_types_mod

