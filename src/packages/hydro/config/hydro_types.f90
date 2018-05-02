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
MODULE hydro_cf_types_mod

  USE dataAPI_kinds_mod,ONLY: rlk,lok
  USE global_types_mod, ONLY: global_t
  USE comms_types_mod,  ONLY: comm_t
  USE eos_cf_types_mod, ONLY: eos_t
  USE io_cf_types_mod,  ONLY: io_t

  IMPLICIT NONE

  TYPE,PUBLIC :: hydro_t
    ! Local
    REAL(KIND=rlk)                             :: cvisc1,cvisc2,cfl_sf,div_sf
    LOGICAL(KIND=lok)                          :: zhg,zsp,ztq
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE :: kappareg,pmeritreg
    LOGICAL(KIND=lok),DIMENSION(:),ALLOCATABLE :: zdtnotreg,zmidlength
    ! Global
    TYPE(global_t),   POINTER                  :: global
    TYPE(comm_t),     POINTER                  :: comm
    TYPE(eos_t),      POINTER                  :: eos
    TYPE(io_t),       POINTER                  :: io
  END TYPE hydro_t

END MODULE hydro_cf_types_mod

