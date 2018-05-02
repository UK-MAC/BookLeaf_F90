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
MODULE utils_dr_sort_mod

  USE utils_kn_sort_mod,  ONLY: utils_kn_sortwrapper
  USE utils_kn_access_mod,ONLY: utils_kn_get
  USE dataAPI_kinds_mod,  ONLY: ink
  USE dataAPI_types_mod,  ONLY: error_t,data_t
  USE dataAPI_params_mod, ONLY: SUCCESS,FAILURE,HALT_SINGLE

  IMPLICIT NONE

  PUBLIC :: utils_dr_sortwrapper

CONTAINS

  SUBROUTINE utils_dr_sortwrapper(ilist,isort,error)

    ! Argument list
    TYPE(data_t),     INTENT(IN)  :: ilist
    TYPE(data_t),     INTENT(OUT) :: isort
    TYPE(error_t),    INTENT(OUT) :: error

    error%ierr=SUCCESS
    CALL utils_kn_sortwrapper(ilist%dsize,ilist%iaddr,isort%iaddr)
    IF (utils_kn_get(1,1,isort%iaddr).EQ.-HUGE(1_ink)) THEN
      error%ierr=FAILURE
      error%iout=HALT_SINGLE
      error%serr="ERROR: sort failed"
      RETURN
    ENDIF

  END SUBROUTINE utils_dr_sortwrapper

END MODULE utils_dr_sort_mod
