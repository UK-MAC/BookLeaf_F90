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

  USE dataAPI_types_mod, ONLY: dt_t
  USE TYPH_Types_mod
  
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: TYPH_add_reduce_dt,TYPH_reduce_dt

CONTAINS

  INTEGER(KIND=ink) FUNCTION TYPH_add_reduce_dt()

    TYPH_add_reduce_dt=0_ink

  END FUNCTION TYPH_add_reduce_dt

  INTEGER(KIND=ink) FUNCTION TYPH_reduce_dt(Val, Comm)

    TYPE(dt_t),        INTENT(INOUT) :: Val
    INTEGER(KIND=ink), INTENT(IN)    :: Comm

    TYPH_reduce_dt=0_ink

  END FUNCTION TYPH_reduce_dt

END MODULE TYPH_dt_reduce_mod
