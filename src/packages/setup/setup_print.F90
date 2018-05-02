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
MODULE setup_print_mod

  USE dataAPI_types_mod, ONLY: sizes_t,io_t
  USE dataAPI_params_mod,ONLY: OSTREAM
  USE setup_IC_mod,      ONLY: setup_IC_print
#ifdef LSETUP
  USE setup_mesh_mod,    ONLY: setup_mesh_print
#endif

  IMPLICIT NONE

  PUBLIC :: setup_print

CONTAINS

  SUBROUTINE setup_print(sizes,io)

    ! Argument list
    TYPE(sizes_t),INTENT(IN) :: sizes
    TYPE(io_t),   INTENT(IN) :: io

#ifdef LSETUP
    ! Print mesh setup
    WRITE(OSTREAM,'(a16)') ' MESHING OPTIONS'
    CALL setup_mesh_print(sizes%nel,sizes%nnd)
    WRITE(OSTREAM,'(a132)') ' ################################################'&
&    //'######################################################################'&
&    //'#############'
#endif
    ! Print initial conditions
    WRITE(OSTREAM,'(a19)') ' INITIAL CONDITIONS'
    CALL setup_IC_print(io)
    WRITE(OSTREAM,'(a132)') ' ################################################'&
&    //'######################################################################'&
&    //'#############'

  END SUBROUTINE setup_print

END MODULE setup_print_mod
