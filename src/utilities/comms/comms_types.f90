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
MODULE comms_types_mod

  USE dataAPI_kinds_mod,ONLY: ink,lok

  TYPE,PUBLIC :: comm_t
    INTEGER(KIND=ink) :: nproc,comm,rank
    LOGICAL(KIND=lok) :: zmproc
  END TYPE comm_t

  TYPE,PUBLIC :: comms_t
    TYPE(comm_t),     POINTER :: world
    TYPE(comm_t),     POINTER :: spatial
    TYPE(comm_t),     POINTER :: replicant
    LOGICAL(KIND=lok)         :: zmpi
    INTEGER(KIND=ink)         :: nthread
  END TYPE comms_t

END MODULE comms_types_mod
