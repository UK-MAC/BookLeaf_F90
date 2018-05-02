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

  use TYPH_Types_mod
  use TYPH_util_mod

  implicit none

  type Part_Info_tp
    integer(kind=INK)                          :: ID            ! Partition ID
    integer(kind=INK)                          :: NLayers       ! No. Ghost layers
    integer(kind=INK)                          :: NodesPerElem  ! depends on calling code
    integer(kind=INK), dimension(:),   pointer :: NEl_tot         => null() ! 0:nlayers
    integer(kind=INK), dimension(:),   pointer :: NNod_tot        => null() ! 0:nlayers
    integer(kind=INK), dimension(:,:), pointer :: El_To_Proc      => null() ! 2,Nel
    integer(kind=INK), dimension(:,:), pointer :: Nod_To_Proc     => null() ! 2, Nnod
    integer(kind=INK), dimension(:),   pointer :: El_Loc_To_Glob  => null() ! Nel
    integer(kind=INK), dimension(:),   pointer :: Nod_Loc_To_Glob => null() ! Nnod
    integer(kind=INK), dimension(:,:), pointer :: Connectivity    => null() ! NPE,N_el
    character(len=TYPH_STRLEN)                 :: name
  end type Part_Info_tp

  type :: PartLL_tp
    type (Part_Info_tp)                         :: partition
    type (PartLL_tp), pointer                   :: next
  end type PartLL_tp

  type (PartLL_tp), pointer, save :: mPartLL => null()    ! Base of temporaray linked-list (LL)

  integer(kind=INK), save :: mNParts = 0

  public
  private :: mAddLL,mGetLL

contains

  integer(kind=INK) function TYPH_Set_Partition_Info( ID, El_Shape, N_Layers, N_El_tot,    &
                                                      N_Nod_tot,El_To_Proc, Nod_To_Proc,   &
                                                      El_Loc_To_Glob, Nod_Loc_To_Glob,     &
                                                      Connectivity, Name )

    implicit none

    integer(kind=INK),                                       intent(out)          :: ID
    integer(kind=INK),                                       intent(in)           :: El_Shape
    integer(kind=INK),                                       intent(in)           :: N_Layers
    integer(kind=INK),dimension(0:),                         intent(in)           :: N_El_tot
    integer(kind=INK),dimension(0:),                         intent(in)           :: N_Nod_tot
    integer(kind=INK),dimension(:,:),                        intent(in)           :: El_To_Proc
    integer(kind=INK),dimension(:,:),                        intent(in)           :: Nod_To_Proc
    integer(kind=INK),dimension(N_El_tot(N_Layers)),         intent(in)           :: El_Loc_To_Glob
    integer(kind=INK),dimension(N_Nod_tot(N_Layers)),        intent(in)           :: Nod_Loc_To_Glob
    integer(kind=INK),dimension(El_Shape,N_El_tot(N_Layers)),intent(in), optional :: Connectivity
    character(len=*),                                        intent(in), optional :: Name

    integer(kind=INK) :: irc
    integer(kind=INK) :: iAllocStat
    integer(kind=INK) :: iNEl
    integer(kind=INK) :: iNNod

    type (PartLL_tp),    pointer :: iPartLL
    type (Part_Info_tp), pointer :: iPartInfo => null()
    type (MP_tp),        pointer :: iMP  => null()

    iMP      => ty_GetMP()

    irc = mAddLL(mPartLL, mNParts, iPartLL)

    iPartInfo    => iPartLL%partition
    iPartInfo%ID =  mNParts
    ID           =  mNParts

    if (present(Name)) then
      iPartInfo%name = Name
    else
      iPartInfo%name = ""
    end if

    iNel  = N_El_tot(N_Layers)
    iNNod = N_Nod_tot(N_Layers)

    iPartInfo%NodesPerElem = El_Shape
    iPartInfo%NLayers      = N_Layers

    allocate(iPartInfo%NEl_tot(0:N_Layers), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPartInfo%NEl_tot")
    iPartInfo%NEl_tot         = N_El_tot

    allocate(iPartInfo%NNod_tot(0:N_Layers), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPartInfo%NNod_tot")
    iPartInfo%NNod_tot        = N_Nod_tot

    allocate(iPartInfo%El_Loc_To_Glob(iNEl), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPartInfo%El_Loc_To_Glob")
    iPartInfo%El_Loc_To_Glob(1:iNel) = El_Loc_To_Glob(1:iNel)

    allocate(iPartInfo%Nod_Loc_To_Glob(iNNod), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPartInfo%Nod_Loc_To_Glob")
    iPartInfo%Nod_Loc_To_Glob(1:iNNod) = Nod_Loc_To_Glob(1:iNNod)

!   El_To_Proc not needed in serial runs
    if (iMP%size > 1) then
      allocate(iPartInfo%El_To_Proc(2,iNEl), stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPartInfo%El_To_Proc")
      iPartInfo%El_To_Proc(:,1:iNel) = El_To_Proc(:,1:iNel)
    endif

    allocate(iPartInfo%Nod_To_Proc(2,iNNod), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPartInfo%Nod_To_Proc")
    iPartInfo%Nod_To_Proc(:,1:iNNod)  = Nod_To_Proc(:,1:iNNod)

    if (present(Connectivity)) then
      allocate(iPartInfo%Connectivity(iPartInfo%NodesPerElem,iNel), stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPartInfo%Connectivity")

      iPartInfo%Connectivity(:,1:iNel) = Connectivity(:,1:iNel)
    endif

!   Print Warning if current processor owns all cells/nodes, including ghosts!
!   Could be due to slide lines on all proc boundaries so not necessarily an error
    if (.not. any(iPartInfo%El_To_Proc(1,:) /= iMP%rank)) then
      print*," Typhon 3: WARNING: Processor ",iMP%rank
      print*,"                    owns -all- cells in partition, including ghosts,"
      print*,"                    or partition has no ghosts"
    endif

    if (.not. any(iPartInfo%Nod_To_Proc(1,:) /= iMP%rank)) then
      print*," Typhon 3: WARNING: Processor ",iMP%rank
      print*,"                    owns -all- nodes in partition, including ghosts"
      print*,"                    or partition has no ghosts"
    endif

    TYPH_Set_Partition_Info = irc
    return

  end function TYPH_Set_Partition_Info


  integer(kind=INK) function ty_GetPartition(aPartID, aPart)

    implicit none

    integer(kind=INK),            intent(in)  :: aPartID
    type (Part_Info_tp), pointer, intent(out) :: aPart

    type (PartLL_tp),    pointer :: iPartLL
    integer(kind=INK)            :: irc

    irc = mGetLL(mPartLL, mNParts, aPartID, iPartLL)

    aPart => iPartLL%partition

    ty_GetPartition = 0
    return

  end function ty_GetPartition


  integer(kind=INK) function mAddLL(aLL, aN, aNode)

    implicit none

    type (PartLL_tp), pointer           :: aLL       ! intent(in)
    integer(kind=INK), intent(inout)    :: aN
    type (PartLL_tp), pointer           :: aNode     ! intent(in)

    integer(kind=INK) :: is
    integer           :: iAllocStat
    integer(kind=INK) :: irc

    if (aN == 0) then
      ! Allocate head of LL
      allocate(aLL, stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "aLL")
      aNode => aLL
    else
      ! Traverse to end of LL and allocate new end node
      aNode => aLL
      do is = 1, aN - 1      ! do loop not executed when mNParts = 1
        irc = ty_ErrorCheck( associated(aNode%next), "mAddLL", ERR_INT, TYPH_ERR_INT,  &
             &          "Next item in LL is not associated")
        aNode => aNode%next
      end do
      allocate(aNode%next, stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "aNode%next")
      aNode => aNode%next
    end if

    nullify(aNode%next)

    aN = aN + 1

    mAddLL = 0

  end function mAddLL


  integer(kind=INK) function mGetLL(aLL, aN, aID, aNode)

    implicit none

    type (PartLL_tp),  pointer    :: aLL       ! intent(in)
    integer(kind=INK), intent(in) :: aN
    integer(kind=INK), intent(in) :: aID
    type (PartLL_tp),  pointer    :: aNode     ! intent(out)

    integer(kind=INK) :: is
    integer(kind=INK) :: irc

    if (aN == 0) then
      mGetLL = -1
      return
    end if

    ! Traverse to end of LL and allocate new end node
    aNode => aLL
    do is = 2, aID      ! do not executed when aID = 1
      irc = ty_ErrorCheck(associated(aNode%next), "mGetLL", ERR_INT, TYPH_ERR_INT,  &
           &          "Next item in LL is not associated")
      aNode => aNode%next
    end do

    mGetLL = 0

  end function mGetLL

end module TYPH_Decomposition_mod

! EOF
