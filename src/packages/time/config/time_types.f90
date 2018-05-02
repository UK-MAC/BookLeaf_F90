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
MODULE time_cf_types_mod

  USE dataAPI_kinds_mod,ONLY: rlk
  USE comms_types_mod,  ONLY: comm_t
  USE io_cf_types_mod,  ONLY: io_t

  IMPLICIT NONE

  TYPE,PUBLIC :: time_t
    ! Local
    REAL(KIND=rlk)         :: time_start,time_end,dt_g,dt_min,dt_max,dt_initial
    ! Global
    TYPE(comm_t),  POINTER :: comm
    TYPE(io_t),    POINTER :: io
  END TYPE time_t

END MODULE time_cf_types_mod

