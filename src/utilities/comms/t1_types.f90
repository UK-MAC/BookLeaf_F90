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
module TYPH_Types_mod

  USE dataAPI_kinds_mod,ONLY: INK,RLK,LOK

  implicit none
  
  public
  
  integer(kind=INK), parameter :: TYPH_REAL        = 1
  integer(kind=INK), parameter :: TY_MEM_ALLOC   = 1111
  integer(kind=INK), parameter :: TY_MEM_DEALLOC = 1112
  integer(kind=INK), parameter :: TYPH_GHOSTS_ONE   = 1
  integer(kind=INK), parameter :: TYPH_GHOSTS_TWO   = 2
  integer(kind=INK), parameter :: TYPH_MESH_DIM     = -88

end module TYPH_Types_mod
  
! EOF

