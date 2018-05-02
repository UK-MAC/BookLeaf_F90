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
module TYPH_Register_mod

  use TYPH_Types_mod

  implicit none

  integer(kind=INK), public, parameter :: TYPH_PURE            = 1
  integer(kind=INK), public, parameter :: TYPH_CENTRE_NODE     = 2001
  integer(kind=INK), public, parameter :: TYPH_CENTRE_CELL     = 2002

  private

  public :: TYPH_Start_Register
  public :: TYPH_Finish_Register
  public :: TYPH_Add_Phase
  public :: TYPH_Get_Phase
  public :: TYPH_Add_Quant
  public :: TYPH_Get_Quant
  public :: TYPH_Tag_Quant

contains
  
  integer(kind=INK) function TYPH_Start_Register()

    implicit none

    TYPH_Start_Register = 0

  end function TYPH_Start_Register


  integer(kind=INK) function TYPH_Finish_Register()

    implicit none

    TYPH_Finish_Register = 0

  end function TYPH_Finish_Register


  integer(kind=INK) function TYPH_Add_Phase( PhaseID, Name, NumGhosts, PureOrAux,  &
                                             KeySetID, GhostsMin )

    implicit none

    integer(kind=INK),           intent(out) :: PhaseID
    character(len=*),            intent(in)  :: Name
    integer(kind=INK),           intent(in)  :: NumGhosts
    integer(kind=INK),           intent(in)  :: PureOrAux
    integer(kind=INK), optional, intent(in)  :: KeySetID
    integer(kind=INK), optional, intent(in)  :: GhostsMin

    PhaseID = 1
    TYPH_Add_Phase = 0

  end function TYPH_Add_Phase

  integer(kind=INK) function TYPH_Get_Phase( PhaseID, Name, NumGhosts, PureOrAux, &
                                             KeySetID, GhostsMin )

    implicit none
    integer(kind=INK), intent(in)            :: PhaseID
    character(len=*),  intent(out), optional :: Name
    integer(kind=INK), intent(out), optional :: NumGhosts
    integer(kind=INK), intent(out), optional :: PureOrAux
    integer(kind=INK), intent(out), optional :: KeySetID
    integer(kind=INK), intent(out), optional :: GhostsMin

    if (present(Name)) then
      Name = "Dummy name"
    end if
    if (present(NumGhosts)) then
      NumGhosts = 0
    end if

    if (present(PureOrAux)) then
      PureOrAux = 0
    end if

    if (present(KeySetID)) then
      KeySetID = 1
    endif

    if (present(GhostsMin)) then
      GhostsMin = 0
    endif

    TYPH_Get_Phase = 0

  end function TYPH_Get_Phase


  integer(kind=INK) function TYPH_Add_Quant( QuantID, Name, NumGhosts,       &
       &                                     Datatype, Centring, PureOrAux,  &
       &                                     AuxID, Dims )
    
    implicit none
    integer(kind=INK), intent(out) :: QuantID
    character(len=*),  intent(in)  :: Name
    integer(kind=INK), intent(in)  :: NumGhosts
    integer(kind=INK), intent(in)  :: Datatype
    integer(kind=INK), intent(in)  :: Centring
    integer(kind=INK), intent(in)  :: PureOrAux
    integer(KIND=INK), intent(in), optional :: AuxID
    integer(kind=INK), dimension(:), intent(in), optional :: Dims

    QuantID = 1
    TYPH_Add_Quant = 0

  end function TYPH_Add_Quant


  integer(kind=INK) function TYPH_Get_Quant( QuantID, Name, NumGhosts,       &
       &                                     Datatype, Centring, PureOrAux,  &
       &                                     AuxID )

    implicit none

    integer(kind=INK), intent(in)            :: QuantID
    character(len=*),  intent(out), optional :: Name
    integer(kind=INK), intent(out), optional :: NumGhosts
    integer(kind=INK), intent(out), optional :: Datatype
    integer(kind=INK), intent(out), optional :: Centring
    integer(kind=INK), intent(out), optional :: PureOrAux
    integer(KIND=INK), intent(out), optional :: AuxID

    if (present(Name)) then
      Name = "Dummy Name"
    end if
    if (present(NumGhosts)) then
      NumGhosts = 0
    end if
    if (present(Datatype)) then
      Datatype  = 0
    end if
    if (present(Centring)) then
      Centring  = 0
    end if
    if (present(PureOrAux)) then
      PureOrAux = 0
    end if
    if (present(AuxID)) then
      AuxID = 0
    end if

    TYPH_Get_Quant = 0

  end function TYPH_Get_Quant
  
  integer(kind=INK) function TYPH_Tag_Quant( QuantID, PhaseID )

    implicit none
    integer(kind=INK),  intent(in) :: QuantID
    integer(kind=INK),  intent(in) :: PhaseID

    TYPH_Tag_Quant = 0

  end function TYPH_Tag_Quant

end module TYPH_Register_mod

! EOF

