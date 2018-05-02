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
  use mpi
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

    type (Phase_tp),        pointer         :: iPhase     => null()
    type (Quant_tp),        pointer         :: iQuant     => null()
    type (Quant_tp),        pointer         :: iQuantRecv => null()
    type(V3_comm_Quant_tp), pointer         :: iPQInfo    => null()
    type(V3_comm_Quant_tp), pointer         :: iNewPQInfo => null()

    integer(kind=INK)                       :: irc
    integer(kind=INK)                       :: iAllocStat
    character(len=SUBNAME_LEN)              :: subname = "TYPH_Add_Quant_To_Phase"

    irc = ty_ErrorCheck((PhaseID > 0 .and. QuantID > 0), subname,  &
          ERR_USER, TYPH_ERR_INVALID_ARG, "PhaseID or QuantID is zero")

!   RETURN HERE IF QUANT OR PHASE ID NOT FOUND
    if (irc /= TYPH_SUCCESS) then
      TYPH_Add_Quant_To_Phase = irc
      return
    endif

!   Get the pointer to the PQinfo for this phase
    irc = ty_GetLLPhase(PhaseID, iPhase)

    if (irc == TYPH_SUCCESS) then

      if (.not. associated(iPhase%PQInfo)) then     ! First Quant added
        allocate(iNewPQInfo, stat = iAllocStat)
        irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iNewPQInfo")
        iPhase%PQInfo => iNewPQInfo
        iPQInfo => iPhase%PQInfo
      else                                       ! Find end of list
        iPQInfo => iPhase%PQInfo
        do
          if (.not. associated(iPQInfo%next)) then
            exit
          endif
          iPQInfo => iPQInfo%next
        enddo
        allocate(iPQInfo%next, stat = iAllocStat)
        irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPQInfo%next")
        iPQInfo => iPQInfo%next
      endif

      nullify(iPQInfo%next)

      if (present(GhostsMin)) then
        iPQInfo%ghostsMin = GhostsMin
      else
        iPQInfo%ghostsMin = iPhase%ghostsMin
      endif

      if (present(GhostsMax)) then
        iPQInfo%ghostsMax = GhostsMax
      else
        iPQInfo%ghostsMax = iPhase%ghostsMax
      endif

      irc = ty_GetLLQuant( QuantID, iQuant )

      iPQInfo%quantID        = QuantID
      iPQInfo%receiveQuantID = QuantID
      iQuantRecv            => iQuant

!     Get different receiving quant if required. Check they match

      if (present(ReceiveQuantID)) then
        iPQInfo%receiveQuantID = ReceiveQuantID
        if (QuantID /= ReceiveQuantID) then

          irc = ty_GetLLQuant( ReceiveQuantID, iQuantRecv)

          irc = ty_ErrorCheck((iQuant%centring == iQuantRecv%centring), subname,  &
                ERR_USER, TYPH_ERR_INVALID_ARG, "Centrings do not match")

          irc = ty_ErrorCheck((iQuant%datatype == iQuantRecv%datatype), subname,  &
                ERR_USER, TYPH_ERR_INVALID_ARG, "datatypes do not match")

          irc = ty_ErrorCheck((iQuant%AuxID == iQuantRecv%AuxID), subname,  &
                ERR_USER, TYPH_ERR_INVALID_ARG, "AuxIDs do not match")

          irc = ty_ErrorCheck((iQuant%rank == iQuantRecv%rank), subname,  &
                ERR_USER, TYPH_ERR_INVALID_ARG, "ranks do not match")

          if (iQuant%rank > 1) then
            irc = ty_ErrorCheck(ALL(iQuant%dims == iQuantRecv%dims), subname,  &
                  ERR_USER, TYPH_ERR_INVALID_ARG, "dims do not match")
          endif
        endif
      endif

      irc = ty_ErrorCheck((iPQInfo%ghostsMin <= iQuantRecv%nghosts), subname,  &
            ERR_USER, TYPH_ERR_INVALID_ARG, "Ghost Min > Quant%nghost")

      irc = ty_ErrorCheck((iPQInfo%ghostsMax <= iQuantRecv%nghosts), subname,  &
            ERR_USER, TYPH_ERR_INVALID_ARG, "Ghost Max > Quant%nghost")

      if (present(KeySetID)) then
        iPQInfo%keySetID = KeySetID
      else
        iPQInfo%keySetID = iPhase%keySetID
      endif

      nullify(iPQInfo%keySet)

      iPQInfo%quantSize = 0

!     Add Quant to Phase list

      irc = TYPH_Tag_Quant( QuantID, PhaseID )

    endif

    TYPH_Add_Quant_To_Phase = irc

  end function TYPH_Add_Quant_To_Phase


  integer(kind=INK) function mUpdateQuant_R8_1D(QuantID, PQuant)

    implicit none

    integer(kind=INK),                      intent(in) :: QuantID
    real(kind=RLK),dimension(:),allocatable,intent(in) :: PQuant

    integer(kind=INK)              :: irc
    integer(kind=TMEMK)            :: iAddr
    integer(kind=TMEMK), parameter :: iNULL_ADDR = 0
    type (Quant_tp), pointer       :: iQuant  => null()

    if (.not. allocated(PQuant)) then
      iAddr = iNULL_ADDR
    else
      CALL MPI_GET_ADDRESS(PQuant(1), iAddr, irc)
    endif

!   Get the Quant

    if (ty_IsRegistration()) then
!     Still in registration. use Linked list
      irc = ty_GetLLQuant(QuantID, iQuant)
    else
      irc = ty_GetQuant(QuantID, iQuant)
    endif


    if (irc == TYPH_SUCCESS) then
      if (iAddr==iNULL_ADDR) then
        if (allocated(iQuant%uppbnd)) deallocate(iQuant%uppbnd)
        if (allocated(iQuant%lowbnd)) deallocate(iQuant%lowbnd)
      else
        if (allocated(iQuant%uppbnd)) deallocate(iQuant%uppbnd)
        if (allocated(iQuant%lowbnd)) deallocate(iQuant%lowbnd)
        allocate(iQuant%uppbnd(1))
        allocate(iQuant%lowbnd(1))
        iQuant%uppbnd(1) = UBOUND(PQuant,DIM=1)
        iQuant%lowbnd(1) = LBOUND(PQuant,DIM=1)
      endif

      iQuant%quant_address = iAddr
    endif

    mUpdateQuant_R8_1D = irc
    return
  end function mUpdateQuant_R8_1D


  integer(kind=INK) function mUpdateQuant_R8_2D(QuantID, PQuant)

    implicit none
    integer(kind=INK),                        intent(in) :: QuantID
    real(kind=RLK),dimension(:,:),allocatable,intent(in) :: PQuant

    integer(kind=INK)              :: irc
    integer(kind=TMEMK)            :: iAddr
    integer(kind=TMEMK), parameter :: iNULL_ADDR = 0
    type (Quant_tp), pointer       :: iQuant  => null()

    integer(kind=INK)              :: ii

    if (.not. allocated(PQuant)) then
      iAddr = iNULL_ADDR
    else
      CALL MPI_GET_ADDRESS(PQuant(1,1), iAddr, irc)
    endif

!   Get the Quant

    if (ty_IsRegistration()) then
!     Still in registration. use Linked list
      irc = ty_GetLLQuant(QuantID, iQuant)
    else
      irc = ty_GetQuant(QuantID, iQuant)
    endif

    if (irc == TYPH_SUCCESS) then
      if (iAddr==iNULL_ADDR) then
        if (allocated(iQuant%uppbnd)) deallocate(iQuant%uppbnd)
        if (allocated(iQuant%lowbnd)) deallocate(iQuant%lowbnd)
      else
        if (allocated(iQuant%uppbnd)) deallocate(iQuant%uppbnd)
        if (allocated(iQuant%lowbnd)) deallocate(iQuant%lowbnd)
        allocate(iQuant%uppbnd(2))
        allocate(iQuant%lowbnd(2))
        do ii = 1, 2
          iQuant%uppbnd(ii) = UBOUND(PQuant,DIM=ii)
          iQuant%lowbnd(ii) = LBOUND(PQuant,DIM=ii)
        enddo
      endif
      iQuant%quant_address = iAddr
    endif

    mUpdateQuant_R8_2D = irc
    return

  end function mUpdateQuant_R8_2D

  integer(kind=INK) function mUpdateQuant_I4_1D(QuantID, PQuant)

    implicit none

    integer(kind=INK),                          intent(in) :: QuantID
    integer(kind=INK), dimension(:),allocatable,intent(in) :: PQuant

    integer(kind=INK)              :: irc
    integer(kind=TMEMK)            :: iAddr
    integer(kind=TMEMK), parameter :: iNULL_ADDR = 0
    type (Quant_tp), pointer       :: iQuant  => null()

    if (.not. allocated(PQuant)) then
      iAddr = iNULL_ADDR
    else
      CALL MPI_GET_ADDRESS(PQuant(1), iAddr, irc)
    endif

!   Get the Quant
    if (ty_IsRegistration()) then
!     Still in registration. use Linked list
      irc = ty_GetLLQuant(QuantID, iQuant)
    else
      irc = ty_GetQuant(QuantID, iQuant)
    endif

    if (irc == TYPH_SUCCESS) then
      if (iAddr==iNULL_ADDR) then
        if (allocated(iQuant%uppbnd)) deallocate(iQuant%uppbnd)
        if (allocated(iQuant%lowbnd)) deallocate(iQuant%lowbnd)
      else
        if (allocated(iQuant%uppbnd)) deallocate(iQuant%uppbnd)
        if (allocated(iQuant%lowbnd)) deallocate(iQuant%lowbnd)
        allocate(iQuant%uppbnd(1))
        allocate(iQuant%lowbnd(1))
        iQuant%uppbnd(1) = UBOUND(PQuant,DIM=1)
        iQuant%lowbnd(1) = LBOUND(PQuant,DIM=1)
      endif
      iQuant%quant_address = iAddr
    endif

    mUpdateQuant_I4_1D = irc
    return

  end function mUpdateQuant_I4_1D

  integer(kind=INK) function mUpdateQuant_I4_2D(QuantID, PQuant)

    implicit none

    integer(kind=INK),                           intent(in) :: QuantID
    integer(kind=INK),dimension(:,:),allocatable,intent(in) :: PQuant

    integer(kind=INK)              :: irc
    integer(kind=TMEMK)            :: iAddr
    integer(kind=TMEMK), parameter :: iNULL_ADDR = 0
    type (Quant_tp), pointer       :: iQuant  => null()
    integer(kind=INK)              :: ii

    if (.not. allocated(PQuant)) then
      iAddr = iNULL_ADDR
    else
      CALL MPI_GET_ADDRESS(PQuant(1,1), iAddr, irc)
    endif

!   Get the Quant
    if (ty_IsRegistration()) then
!     Still in registration. use Linked list
      irc = ty_GetLLQuant(QuantID, iQuant)
    else
      irc = ty_GetQuant(QuantID, iQuant)
    endif

    if (irc == TYPH_SUCCESS) then
      if (iAddr==iNULL_ADDR) then
        if (allocated(iQuant%uppbnd)) deallocate(iQuant%uppbnd)
        if (allocated(iQuant%lowbnd)) deallocate(iQuant%lowbnd)
      else
        if (allocated(iQuant%uppbnd)) deallocate(iQuant%uppbnd)
        if (allocated(iQuant%lowbnd)) deallocate(iQuant%lowbnd)
        allocate(iQuant%uppbnd(2))
        allocate(iQuant%lowbnd(2))
        do ii = 1, 2
          iQuant%uppbnd(ii) = UBOUND(PQuant,DIM=ii)
          iQuant%lowbnd(ii) = LBOUND(PQuant,DIM=ii)
        enddo
      endif
      iQuant%quant_address = iAddr
    endif

    mUpdateQuant_I4_2D = irc
    return

  end function mUpdateQuant_I4_2D

end module TYPH_Quant_mod


! EOF


