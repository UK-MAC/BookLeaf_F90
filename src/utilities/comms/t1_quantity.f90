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
module TYPH_Quant_mod

  use TYPH_Register_mod
  use TYPH_Types_mod
  use TYPH_util_mod, only: ty_ErrorCheck,ty_MemCheck
  implicit none

  interface TYPH_Set_Quant_Address
    module procedure                          &
      mUpdateQuant_R8_1D, mUpdateQuant_R8_2D, &
      mUpdateQuant_I4_1D, mUpdateQuant_I4_2D
  end interface

contains

  integer(kind=INK) function TYPH_Add_Quant_To_Phase(PhaseID, QuantID,    &
                      ReceiveQuantID, KeySetID, GhostsMin, GhostsMax)


    implicit none


    integer(kind=INK), intent(in)           :: PhaseID
    integer(kind=INK), intent(in)           :: QuantID
    integer(kind=INK), intent(in), optional :: ReceiveQuantID
    integer(kind=INK), intent(in), optional :: KeySetID
    integer(kind=INK), intent(in), optional :: GhostsMin
    integer(kind=INK), intent(in), optional :: GhostsMax

    TYPH_Add_Quant_To_Phase = 0

  end function TYPH_Add_Quant_To_Phase


  integer(kind=INK) function mUpdateQuant_R8_1D(QuantID, PQuant)

    implicit none

    integer(kind=INK),                      intent(in) :: QuantID
    real(kind=RLK),dimension(:),allocatable,intent(in) :: PQuant

    mUpdateQuant_R8_1D = 0

  end function mUpdateQuant_R8_1D


  integer(kind=INK) function mUpdateQuant_R8_2D(QuantID, PQuant)

    implicit none
    integer(kind=INK),                        intent(in) :: QuantID
    real(kind=RLK),dimension(:,:),allocatable,intent(in) :: PQuant

    mUpdateQuant_R8_2D = 0

  end function mUpdateQuant_R8_2D

  integer(kind=INK) function mUpdateQuant_I4_1D(QuantID, PQuant)

    implicit none

    integer(kind=INK),                          intent(in) :: QuantID
    integer(kind=INK), dimension(:),allocatable,intent(in) :: PQuant

    mUpdateQuant_I4_1D = 0

  end function mUpdateQuant_I4_1D

  integer(kind=INK) function mUpdateQuant_I4_2D(QuantID, PQuant)

    implicit none

    integer(kind=INK),                            intent(in) :: QuantID
    integer(kind=INK), dimension(:,:),allocatable,intent(in) :: PQuant

    mUpdateQuant_I4_2D = 0

  end function mUpdateQuant_I4_2D

end module TYPH_Quant_mod


! EOF


