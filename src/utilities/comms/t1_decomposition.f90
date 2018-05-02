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
module TYPH_Decomposition_mod

  USE TYPH_Types_mod

  implicit none

  public

contains

  integer(kind=INK) function TYPH_Set_Partition_Info( ID, El_Shape, N_Layers, N_El_tot,    &
                                                      N_Nod_tot,El_To_Proc, Nod_To_Proc,   &
                                                      El_Loc_To_Glob, Nod_Loc_To_Glob,     &
                                                      Connectivity, Name )

    implicit none

    integer(kind=INK),                                         intent(out)          :: ID
    integer(kind=INK),                                         intent(in)           :: El_Shape
    integer(kind=INK),                                         intent(in)           :: N_Layers
    integer(kind=INK), dimension(0:),                          intent(in)           :: N_El_tot
    integer(kind=INK), dimension(0:),                          intent(in)           :: N_Nod_tot
    integer(kind=INK), dimension(:,:),                         intent(in)           :: El_To_Proc
    integer(kind=INK), dimension(:,:),                         intent(in)           :: Nod_To_Proc
    integer(kind=INK), dimension(N_El_tot(N_Layers)),          intent(in)           :: El_Loc_To_Glob
    integer(kind=INK), dimension(N_Nod_tot(N_Layers)),         intent(in)           :: Nod_Loc_To_Glob
    integer(kind=INK), dimension(El_Shape,N_El_tot(N_Layers)), intent(in), optional :: Connectivity
    character(len=*),                                          intent(in), optional :: Name

    ID = 1
    TYPH_Set_Partition_Info = 0
    return

  end function TYPH_Set_Partition_Info

end module TYPH_Decomposition_mod

! EOF
