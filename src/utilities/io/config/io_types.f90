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
MODULE io_cf_types_mod

  USE dataAPI_params_mod,ONLY: NLEN

  IMPLICIT NONE

  TYPE,PUBLIC :: io_t
    CHARACTER(LEN=NLEN),DIMENSION(:),ALLOCATABLE :: sregion,smaterial
  END TYPE io_t

END MODULE io_cf_types_mod
