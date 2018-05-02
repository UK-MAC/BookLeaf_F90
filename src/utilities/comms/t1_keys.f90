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
module TYPH_Key_mod

  use TYPH_Types_mod

  implicit none

  integer(kind=INK), parameter :: TYPH_KTYPE_CELL        = 1

contains

  integer(kind=INK) function TYPH_Create_Key_Set(ID, KType, Lmin, Lmax, PartitionID)

    implicit none

    integer(kind=INK), intent(out) :: ID           !    KeySet index
    integer(kind=INK), intent(in)  :: KType        !    Key Type (Node/Cell/Cell Corner)
    integer(kind=INK), intent(in)  :: Lmin         !    smallest ghost layer index
    integer(kind=INK), intent(in)  :: Lmax         !    largest ghost layer index
    integer(kind=INK), intent(in)  :: PartitionID  !    ID of Partition to use with Key

    ID = 1
    TYPH_Create_Key_Set = 0

  end function TYPH_Create_Key_Set

end module TYPH_Key_mod

! EOF
