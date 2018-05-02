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

  use TYPH_Register_mod, only : TYPH_CENTRE_NODE,TYPH_CENTRE_CELL 
  use TYPH_Types_mod
  use TYPH_Collect_mod
  use TYPH_util_mod
  use TYPH_Decomposition_mod, only: Part_Info_tp

  implicit none

  integer(kind=INK), parameter :: TYPH_KTYPE_CELL        = 1
  integer(kind=INK), parameter :: TYPH_KTYPE_NODE        = 2
  integer(kind=INK), parameter :: TYPH_KTYPE_CELL_CORNER = 3

  type Key_Set_Array_tp
    type(Key_Set_tp), pointer :: KeySet => null()
  end type Key_Set_Array_tp

  integer(kind=INK),      save                  :: mNKeySets = 0
  type(Key_Set_Array_tp), dimension(:), pointer :: KeySetArray => null()

  integer(kind=INK),      save                  :: mMaxKeySize = 0

  type (Part_Info_tp), pointer :: mPartInfo => null()  ! Partition info
  type (MP_tp),        pointer :: mMP  => null()  ! MPI info

  integer(kind=INK), dimension(:),   pointer   :: mToGlob       => null()
  integer(kind=INK), dimension(:,:), pointer   :: mToProc       => null()
  integer(kind=INK), dimension(:,:), pointer   :: mConnectivity => null()
  integer(kind=INK), dimension(:), allocatable :: mTotItems ! Total number of ghost cells/nodes
  integer(kind=INK)                            :: mCentring ! Node/Cell/Corner centring
  integer(kind=INK)                            :: mStride   ! Stride across memory

  public :: TYPH_Create_Key_Set
  public :: ty_GetKeySet
  public :: ty_GetKey
  public :: Key_Set_tp
  public :: KeyLL_tp

  public :: TYPH_KTYPE_CELL
  public :: TYPH_KTYPE_NODE
  public :: TYPH_KTYPE_CELL_CORNER

contains

  integer(kind=INK) function TYPH_Create_Key_Set(ID, KType, Lmin, Lmax, PartitionID)

    use TYPH_Decomposition_mod, only: ty_GetPartition

    implicit none

    integer(kind=INK), intent(out) :: ID           !    KeySet index
    integer(kind=INK), intent(in)  :: KType        !    Key Type (Node/Cell/Cell Corner)
    integer(kind=INK), intent(in)  :: Lmin         !    smallest ghost layer index
    integer(kind=INK), intent(in)  :: Lmax         !    largest ghost layer index
    integer(kind=INK), intent(in)  :: PartitionID  !    ID of Partition to use with Key

    type(Key_Set_tp), pointer  :: iPKeySet         => null()

    character(len=SUBNAME_LEN) :: subname = "TYPH_Create_Key_Set"
    integer(kind=INK)          :: irc
    integer(kind=INK)          :: iAllocStat
    integer(kind=INK)          :: iAuxID

    ! Set up the pointers to decomposition info
    irc = ty_GetPartition(PartitionID, mPartInfo)

!   Obtain MPI data
    mMP   => ty_GetMP()

!   Get Typh data type (holding No. cells/elements, eltoproc, globel, connectivity)?
    allocate(mTotItems(0: mPartInfo%Nlayers), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "mTotItems")

    select case(KType)
    case (TYPH_KTYPE_CELL)
      mCentring = TYPH_CENTRE_CELL
      iAuxID    = TYPH_AUXID_NONE
      mStride   = 1_INK
      mToProc  => mPartInfo%El_To_Proc
      mTotItems = mPartInfo%NEl_tot
      mToGlob  => mPartInfo%El_Loc_To_Glob
    case (TYPH_KTYPE_NODE)
      mCentring = TYPH_CENTRE_NODE
      iAuxID    = TYPH_AUXID_NONE
      mStride   = 1_INK
      mToProc  => mPartInfo%Nod_To_Proc
      mTotItems = mPartInfo%NNod_tot
      mToGlob  => mPartInfo%Nod_Loc_To_Glob
    case (TYPH_KTYPE_CELL_CORNER)
      mCentring = TYPH_CENTRE_CELL
      iAuxID    = TYPH_AUXID_NONE
      mStride   = mPartInfo%NodesPerElem
      mToProc  => mPartInfo%El_To_Proc
      mTotItems = mPartInfo%NEl_tot
      mToGlob  => mPartInfo%El_Loc_To_Glob
      mConnectivity => mPartInfo%Connectivity
      if (.not. associated(mConnectivity)) then ! OR irc =/ TYPH_SUCCESS????
        irc = ty_ErrorCheck(.false. , subname, ERR_INT, TYPH_ERR_INT, &
              "Connectivity array must be supplied for key type=TYPH_CELL_CORNER")
      endif
    case default
        irc = ty_ErrorCheck(.false. , subname, ERR_INT, TYPH_ERR_INT, "Invalid key type")
    end select

!   check limits are correct
    if (mCentring == TYPH_CENTRE_NODE) then
      irc = ty_ErrorCheck(((Lmin >= 0_INK) .and. (Lmin <= mPartInfo%NLayers)) ,    &
            subname, ERR_INT, TYPH_ERR_INT, "Lmin is not between 0 and NGhost_Lay")
      irc = ty_ErrorCheck(((Lmax >= 0_INK) .and. (Lmax <= mPartInfo%NLayers)) ,    &
            subname, ERR_INT, TYPH_ERR_INT, "Lmax is not between 0 and NGhost_Lay")
    else
      irc = ty_ErrorCheck(((Lmin >= 1_INK) .and. (Lmin <= mPartInfo%NLayers)) ,    &
            subname, ERR_INT, TYPH_ERR_INT, "Lmin is not between 1 and NGhost_Lay")
      irc = ty_ErrorCheck(((Lmax >= 1_INK) .and. (Lmax <= mPartInfo%NLayers)) ,    &
            subname, ERR_INT, TYPH_ERR_INT, "Lmax is not between 1 and NGhost_Lay")
    endif

    irc = ty_ErrorCheck((Lmin <= Lmax) , subname, ERR_INT, TYPH_ERR_INT, "Lmin > Lmax")

!   Create a new KeySet to work with
    irc = mNewKeySet(ID, mCentring, iAuxID, mStride, Lmin, Lmax, PartitionID, iPKeySet)
    irc = mDefineKeySet(iPKeySet, KType, Lmin, Lmax)

    deallocate(mTotItems, stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "mTotItems")

    TYPH_Create_Key_Set = irc
    return

  end function TYPH_Create_Key_Set


  integer(kind=INK) function mAddKey(aLL, aNkey, aProc, aLayer, aNode)

    implicit none

    type(KeyLL_tp), pointer, intent(inout) :: aLL    ! Head of list
    integer(kind=INK),       intent(inout) :: aNkey  ! No. keys in list
    integer(kind=INK),       intent(in)    :: aProc  ! Processor ID
    integer(kind=INK),       intent(in)    :: aLayer ! Ghost layer
    type(KeyLL_tp), pointer, intent(out)   :: aNode  ! new node in list

    type(KeyLL_tp), pointer                :: iNewKey  => null()
    type(KeyLL_tp), pointer                :: iPrevKey => null()
    type(KeyLL_tp), pointer                :: iPKey    => null()

    integer(kind=INK) :: iAllocStat
    integer(kind=INK) :: ii
    integer(kind=INK) :: irc

    nullify(aNode)
    nullify(iPrevKey)

    iPKey => aLL

    do ii = 1, aNkey
      ! Drop out of loop if new key needs inserting in middle of list
      if ((iPKey%proc > aProc) .or. ((iPKey%proc == aProc) .and. (iPKey%layer > aLayer))) then
        exit
      endif
      iPrevKey => iPKey
      iPKey    => iPKey%next
    enddo

    allocate(iNewKey, stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iNewKey")

    iNewKey%next => iPKey     ! unassociated if last item in list
    if (associated(iPrevKey)) then
      iPrevKey%next => iNewKey
      iNewKey%prev  => iPrevKey
    else
      aLL => iNewKey
      iNewKey%prev => NULL()
    endif

    aNode => iNewKey
    aNode%proc  = aProc
    aNode%layer = aLayer
    aNode%nlist = 0
    nullify(iNewKey%parent)
    nullify(iNewKey%list)

    aNkey = aNkey + 1

    mAddKey = irc
    return

  end function mAddKey

  integer(kind=INK) function mNewKeySet(aID, aCentring, aAuxID, aStride, aLmin,   &
                                        aLmax, aPartitionID, aKeySet)
    implicit none
    integer(kind=INK),        intent(out) :: aID            ! New KeySet index
    integer(kind=INK),        intent(in)  :: aCentring      ! variable centring (NODE/CELL)
    integer(kind=INK),        intent(in)  :: aAuxID         ! handle for type of auxilary data
    integer(kind=INK),        intent(in)  :: aStride        ! no. parts object is divided
                                                             ! into (for cell corner props)
    integer(kind=INK),        intent(in)  :: aLmin          ! smallest ghost layer index
    integer(kind=INK),        intent(in)  :: aLmax          ! largest ghost layer index
    integer(kind=INK),        intent(in)  :: aPartitionID   ! ID of partition used by this KeySet
    type(Key_Set_tp), pointer,intent(out) :: aKeySet        ! pointer to new KeySet

    integer(kind=INK)          :: irc
    integer(kind=INK)          :: iAllocStat

    nullify(aKeySet)

    allocate(aKeySet, stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "aKeySet")

    ! add new keyset to keyset array
    irc = mAddKeySet(aKeySet, aID)

    aKeySet%centring    = aCentring
    aKeySet%AuxID       = aAuxID
    aKeySet%stride      = aStride
    aKeySet%lmin        = aLmin
    aKeySet%lmax        = aLmax
    aKeySet%partitionID = aPartitionID
    aKeySet%nSend       = 0_INK
    aKeySet%nRecv       = 0_INK

    nullify(aKeySet%send_keys)
    nullify(aKeySet%recv_keys)

    mNewKeySet = irc
    return

  end function mNewKeySet

  integer(kind=INK) function mAddKeySet(aKeySet, aID)

    implicit none
    type(Key_Set_tp), pointer, intent(in)  :: aKeySet  ! pointer to new KeySet
    integer(kind=INK),         intent(out) :: aID      ! KeySet index

    type(Key_Set_Array_tp), dimension(:), pointer :: temp_array
    integer(kind=INK) :: ii
    integer(kind=INK) :: irc
    integer(kind=INK) :: iAllocStat
    integer(kind=INK) :: iMemTag

    mNKeySets = mNKeySets + 1
    iMemTag  = 40200 + mNKeySets

    allocate(temp_array(mNKeySets), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "temp_array")

    if (mNKeySets > 1) then
      ! copy existing data to temporary array then deallocate main array
      do ii = 1, mNKeySets-1
        temp_array(ii)%KeySet => KeySetArray(ii)%KeySet
      enddo
      deallocate(KeySetArray, stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "temp_array")
      iMemTag = iMemTag - 1   ! Tag was derived from previous mNKeySets size
    endif

    KeySetArray             => temp_array
    aID                     =  mNKeySets
    KeySetArray(aID)%KeySet => aKeySet

    mAddKeySet = irc
    return

  end function mAddKeySet

! Defines a Key Set
  integer(kind=INK) function mDefineKeySet(aPKeySet, aKType, aLmin, aLmax)

    implicit none

    type(Key_Set_tp),  pointer     :: aPKeySet
    integer(kind=INK), intent(in)  :: aKType        !    Key Type (Node/Cell/Cell Corner)
    integer(kind=INK), intent(in)  :: aLmin         !    smallest ghost layer index
    integer(kind=INK), intent(in)  :: aLmax         !    largest ghost layer index

    integer(kind=INK), dimension(MPI_STATUS_SIZE) :: iMpiStatus
    logical(kind=LOK)                             :: iGlobPresent

    type(KeyLL_tp), pointer :: iPKey => null()

    integer(kind=INK), dimension(:),   allocatable :: iCounts   ! No. ghost items from each neighb
    integer(kind=INK), dimension(:),   allocatable :: iTCounts
    integer(kind=INK), dimension(:),   allocatable :: iStarts
    integer(kind=INK), dimension(:),   allocatable :: iSendReq
    integer(kind=INK)                              :: iRecvReq
    integer(kind=INK), dimension(:,:), allocatable :: iSendItemList
    integer(kind=INK), dimension(:,:), allocatable :: iRecvItemList

!   counters
    integer(kind=INK) :: ii
    integer(kind=INK) :: jj
    integer(kind=INK) :: kk
    integer(kind=INK) :: mm
    integer(kind=INK) :: iProc        ! current processor
    integer(kind=INK) :: iStartindex  ! start index
    integer(kind=INK) :: iLayer       ! current ghost layer

!   sizes
    integer(kind=INK) :: iNsize       ! No. properties per communicated item (nd)
    integer(kind=INK) :: iNitems      ! No. send items
    integer(kind=INK) :: iNrecv       ! No. recv items
    integer(kind=INK) :: iNcount      ! message count
    integer(kind=INK) :: iNremain     ! remaining message count
    integer(kind=INK) :: iSreq        ! send request counter

    character(len=SUBNAME_LEN) :: subname = "mDefineKeySet"
    integer(kind=INK)          :: irc
    integer(kind=INK)          :: iAllocStat

    integer(kind=INK)          :: iV3_COMM_TAG

!   if global map array is present ensure that this will be included in iSendItemList

    if (associated(mToGlob)) then
      iNsize = 2_INK
      iGlobPresent = .true.
    else
      iNsize = 1_INK
      iGlobPresent = .false.
    endif

    allocate(iCounts(0: mMP%size-1), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iCounts")
    iCounts = 0_INK

    allocate(iStarts(0: mMP%size-1), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iStarts")
    iStarts = 0_INK

    allocate(iTCounts(0: mMP%size-1), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iTCounts")
    iTCounts = 0_INK

    allocate(iSendReq(mMP%size), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSendReq")
    iSendReq = 0_INK

!   Build key lists for each ghost cell in each ghost layer
    do iLayer = aLmin, aLmax
      iCounts = 0_INK
      iV3_COMM_TAG = 1000 + iLayer

      select case(aKType)
      case (TYPH_KTYPE_CELL, TYPH_KTYPE_NODE)
        iNitems = 0_INK
        if (iLayer > 0) then
          iStartindex = mTotItems(iLayer - 1_INK) + 1_INK
        else
          iStartindex = 1_INK
        endif

!       first work out how many ghost cells/nodes belong to each neighbour processor
        do ii = iStartindex, mTotItems(iLayer)
          iProc = mToProc(1,ii)
          if (iProc == mMP%rank) then
            cycle
          endif
          iNitems = iNitems + 1_INK
          iCounts(iProc) = iCounts(iProc) + 1_INK
        enddo

!       Now build up a list of start positions for each procs contributions in the
!       send array that will be built shortly

        iStarts(0) = 1_INK
        do ii = 1, mMP%size - 1_INK
          iStarts(ii) = iStarts(ii-1) + iCounts(ii-1)
        enddo



!       Build an array containing the displacements (localID-1) and global index (if avail)
!       of each ghost cell receiving data in the exchange on this processor. iStarts tells
!       us where each neighbour proc's contribution begins.
!       The relevent bits passed to the neighbour procs later to let them work out their
!       sending cell displacements.

!       NOTE: this array can be used to work out a reverse send signal

        allocate(iSendItemList(iNsize, iNitems), stat=iAllocStat)
        irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSendItemList")
!       irc = ty_MemAdd(40306, TYPH_INTEGER, iNsize*iNitems)


!       Now loop through and fill the array
!       Could this be done without the need for the full second loop?

        do ii = iStartindex, mTotItems(iLayer)
          iProc = mToProc(1,ii)
          if (iProc == mMP%rank) then
            cycle
          endif
          iSendItemList(1,iStarts(iProc)) = ii - 1_INK
          iStarts(iProc) = iStarts(iProc) + 1_INK   ! Increases index in iSendItemList
        enddo

      case (TYPH_KTYPE_CELL_CORNER)
!     Cell corners are generated the same way with the added complication that
!     we need each corner's component

        iNitems = 0_INK

        iStartindex = mTotItems(iLayer - 1) + 1_INK


!       first work out how many ghost cells/nodes belong to each neighbour processor

        jj = mPartInfo%NNod_tot(iLayer-1_INK)

        do ii = iStartindex, mTotItems(iLayer)
          iProc = mToProc(1,ii)
          if (iProc == mMP%rank) then
            cycle
          endif
          do kk = 1, mPartInfo%NodesPerElem
            if (mConnectivity(kk, ii) <= jj) then
              iNitems = iNitems + 1_INK
              iCounts(iProc) = iCounts(iProc) + 1_INK
            endif
          enddo
        enddo

!       Now build up a list of start positions for each procs contributions in the
!       send array that will be built shortly

        iStarts(0) = 1_INK
        do ii = 1, mMP%size - 1_INK
          iStarts(ii) = iStarts(ii-1) + iCounts(ii-1)
        enddo



!       Build an array containing the displacements (localID-1) and global index (if avail)
!       of each ghost cell receiving data in the exchange on this processor. iStarts tells
!       us where each neighbour proc's contribution begins.
!       The relevent bits passed to the neighbour procs later to let them work out their
!       sending cell displacements.

!       NOTE: this array can be used to work out a reverse send signal

        allocate(iSendItemList(iNsize, iNitems), stat=iAllocStat)
        irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSendItemList")
!       irc = ty_MemAdd(40306, TYPH_INTEGER, iNsize*iNitems)


!       Now loop through and fill the array
!       Could this be done without the need for the full second loop?

        do ii = iStartindex, mTotItems(iLayer)
          iProc = mToProc(1,ii)
          if (iProc == mMP%rank) then
            cycle
          endif
          do kk = 1, mPartInfo%NodesPerElem
            if (mConnectivity(kk, ii) <= jj) then
              iSendItemList(1,iStarts(iProc)) = (ii-1) * mPartInfo%NodesPerElem + kk - 1_INK
              iStarts(iProc) = iStarts(iProc) + 1_INK    ! Increases index in iSendItemList
            endif
          enddo
        enddo

      case default
        irc = ty_ErrorCheck(.false. , subname, ERR_INT, TYPH_ERR_INT, "Invalid key type")
      end select


!     Reset iStarts array (was mangled in above loop constructing iSendItemList)

      iStarts(0) = 1_INK
      do ii = 1, mMP%size - 1_INK
        iStarts(ii) = iStarts(ii-1) + iCounts(ii-1)
      enddo


!     Ensure all processors have the total number of items to send

!     Must ensure this call is maintained if the collectives change
      irc = TYPH_Reduce(iCounts, RVal=iTCounts, Op=TYPH_OP_SUM, Comm=mMP%comm)
      iNrecv =  iTCounts(mMP%rank)


!     Fill iSendItemList with global IDs if available
      if (iGlobPresent) then
        do ii = 1, iNitems
          jj = (iSendItemList(1, ii)) / mStride + 1_INK
          iSendItemList(2,ii) = mToGlob(jj)
        enddo
      endif

!     Now have the information required to build the receive MPI types for
!     each processor. Time to build the receive keys and work out the send
!     information on the neighbour processors. Each processor may be sending
!     to more than one neighbour so need to build up list of all communications

      iSreq = 0

!     Build receive key lists for the current processor from each neighbour.
!     Then convert iSendItemList array to store local displacement on neighbour proc
!     Then send out sections of the iSendItemList to the relevent neighbour proc

!     First post a recv to prevent unexpected messages later

      allocate(iRecvItemList(iNsize, iNrecv), stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iRecvItemList")
!     irc = ty_MemAdd(40308, TYPH_INTEGER, iNsize*iNrecv)

      if (iNrecv > 0) then
        call MPI_iRecv(iRecvItemList(1,1), iNrecv * iNsize, MPI_INTEGER, MPI_ANY_SOURCE,   &
                      iV3_COMM_TAG, mMP%comm, iRecvReq, irc)
      endif

      do iProc = 0, mMP%size - 1_INK

        if (iCounts(iProc) > 0_INK) then

          irc = mAddKey(aPKeySet%recv_keys, aPKeySet%nRecv, iProc, iLayer, iPKey)

          iPKey%nlist = iCounts(iProc)
          mMaxKeySize = max(mMaxKeySize, iPKey%nlist)

          allocate(iPKey%list(iPKey%nlist), stat=iAllocStat)
          irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPKey%list")
!         irc = ty_MemAdd(40307, TYPH_INTEGER, iPKey%nlist)

          ii = 1_INK
          do jj = iStarts(iProc), iStarts(iProc)+iCounts(iProc) - 1_INK

!           Build receive key lists for the current processor from each neighbour.
            iPKey%list(ii) = iSendItemList(1,jj)
            mm = (iSendItemList(1, jj)/mStride) + 1_INK
            kk = iSendItemList(1, jj) - mStride * (mm - 1_INK) + 1_INK

!           Convert iSendItemList array to store local displacement on neighbour proc
            iSendItemList(1, jj) = mStride * (mToProc(2, mm) - 1_INK) + kk - 1_INK
            ii = ii + 1_INK

          enddo

!         Send out sections of the iSendItemList to the relevent neighbour proc
          iSreq = iSreq + 1_INK
          call MPI_ISend(iSendItemList(1, iStarts(iProc)), iNsize * iCounts(iProc),    &
                         MPI_INTEGER, iProc, iV3_COMM_TAG, mMP%comm, iSendReq(iSreq), irc)

        endif

      enddo


!     Receive sections of the iSendItemList from neighbour procs
      iNremain = iNrecv
      do

        if (iNremain == 0) then
          exit
        else
          call MPI_Wait(iRecvReq, iMpiStatus, irc)
          call MPI_GET_COUNT(iMpiStatus, MPI_INTEGER, iNcount, irc)
          iNcount = iNcount / iNsize
          iNremain = iNremain - iNcount
          iProc = iMpiStatus(MPI_SOURCE)

          irc = mAddKey(aPKeySet%send_keys, aPKeySet%nSend, iProc, iLayer, iPKey)

          iPKey%nlist = iNcount
          mMaxKeySize = max(mMaxKeySize, iPKey%nlist)

          allocate(iPKey%list(iPKey%nlist), stat=iAllocStat)
          irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPKey%list")
!         irc = ty_MemAdd(40309, TYPH_INTEGER, iPKey%nlist)

!         Build send key lists from the current processor to each neighbour.
          do ii = 1, iNcount
            iPKey%list(ii) = iRecvItemList(1,ii)
          enddo

!         Sanity check. Do the iRecvItemList global IDs match the main global list ones?
          if (iGlobPresent) then
            do ii = 1, iNcount
              jj = (iRecvItemList(1, ii) / mStride) + 1_INK
              if (mToGlob(jj) /= iRecvItemList(2, ii)) then
                print*,"Warning: Global IDs do not match between processors"
                print*,"Rank          = ", mMP%rank
                print*,"ii, jj        = ", ii, jj
                print*,"mToGlob       = ", mToGlob(jj)
                print*,"iRecvItemList = ", iRecvItemList(2,ii)
                irc = ty_ErrorCheck(.false. , subname, ERR_INT, TYPH_ERR_INT, &
              "Global IDs do not match iRecvItemList")
              endif
            enddo
          endif
          if (iNremain > 0) then
            call MPI_iRecv(iRecvItemList(1,1), iNrecv * iNsize, MPI_INTEGER,  &
                           MPI_ANY_SOURCE, iV3_COMM_TAG, mMP%comm, iRecvReq, irc)
          endif

        endif

      enddo

      do ii=1, iSreq
         call MPI_Wait(iSendReq(ii), iMpiStatus, irc)
      enddo

      deallocate(iSendItemList, stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSendItemList")
!     irc = ty_MemRem( 40306, TYPH_INTEGER, iNsize*iNitems)

      deallocate(iRecvItemList, stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iRecvItemList")
!     irc = ty_MemRem( 40308, TYPH_INTEGER, iNsize*iNrecv)

    enddo      !   do iLayer = aLmin, aLmax

    deallocate(iCounts, stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iCounts")
!   irc = ty_MemRem( 40302, TYPH_INTEGER, mMP%size )
    deallocate(iTCounts, stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iTCounts")
!   irc = ty_MemRem( 40303, TYPH_INTEGER, mMP%size )
    deallocate(iStarts, stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iStarts")
!   irc = ty_MemRem( 40304, TYPH_INTEGER, mMP%size )
    deallocate(iSendReq, stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSendReq")
!   irc = ty_MemRem( 40305, TYPH_INTEGER, mMP%size )

    mDefineKeySet = irc
    return

  end function mDefineKeySet

  integer(kind=INK) function ty_GetKey(aKeySetLL, aKeyType, aKeyID, aPKey, aProc, aLayer)

    implicit none

    type(Key_Set_tp),  pointer,intent(inout) :: aKeySetLL ! Head of KeySet Linked list
    integer(kind=INK), intent(in)            :: aKeyType  ! Type - Send or recv key
    integer(kind=INK), intent(out)           :: aKeyID    ! returned key poition in list
    type(KeyLL_tp),    pointer,intent(out)   :: aPKey     ! pointer to returned key
    integer(kind=INK), intent(in), optional  :: aProc     ! Processor ID
    integer(kind=INK), intent(in), optional  :: aLayer    ! Ghost layer (default last layer)

    type(KeyLL_tp), pointer    :: iPKey     => null()

    integer(kind=INK)          :: iCount
    integer(kind=INK)          :: irc
    character(len=SUBNAME_LEN) :: subname = "ty_GetKey"

    irc = TYPH_SUCCESS

    if (.not. associated(aKeySetLL)) then        ! No keys to get

      aKeyID = 0
      nullify(aPKey)

    else

      select case(aKeyType)      ! get head of Key list for this Key type
      case (TYPH_V3_SEND)
        aPKey => aKeySetLL%send_keys
        aKeyID = aKeySetLL%Nsend
      case (TYPH_V3_RECV)
        aPKey => aKeySetLL%recv_keys
        aKeyID = aKeySetLL%Nrecv
      case default
        irc = ty_ErrorCheck(.false. , subname, ERR_INT, TYPH_ERR_INT,  &
               &          "aKeyType is not TYPH_V3_SEND or TYPH_V3_RECV")
      end select


      if (associated(aPKey) .and. aKeyID > 0) then  ! Return if no key found

        if (present(aProc)) then      ! hunt through list for key matching proc

          do iCount = 1, aKeyID
            if (aPKey%proc == aProc) then           ! Found first key matching proc
              exit
            endif
            aPKey => aPKey%next
          enddo



          if (iCount > aKeyID) then   ! Key not found. Have run off end of list

            aKeyID = 0
            nullify(aPkey)            ! Return Null key

          else

            if (present(aLayer)) then    ! Now hunt through rest of key list for matching layer

              do
                if (aPKey%layer == aLayer) then
                  exit
                endif
                aPKey => aPKey%next
                if (.not. associated(aPKey)) then  ! Key not found. Have run off end of list
                  exit
                endif
              enddo
              if (.not. associated(aPKey)) then    ! Return null if no key found
                aKeyID = 0
                nullify(aPkey)
              else                                 ! check there aren't two matching keys
                aKeyID = 0                         ! list is sorted so next one would be the copy
                iPKey => aPKey                     ! use second pointer for this search
                do
                  if ((iPKey%proc /= aProc) .or. (iPKey%layer /= aLayer)) then
                    exit
                  endif
                  aKeyID = aKeyID + 1
                  iPKey => iPKey%next
                  if (.not. associated(iPKey)) then    ! Key not found. Have run off end of list
                    exit
                  endif
                enddo
              endif

            else       ! if (present(aLayer)) then - no layer,just check for 2nd proc match

              aKeyID = 0
              iPKey => aPKey
              do
                if (iPKey%proc /= aProc) then
                  exit
                endif
                aKeyID = aKeyID + 1
                iPKey => iPKey%next
                if (.not. associated(iPKey)) then    ! Key not found. Have run off end of list
                  exit
                endif
              enddo
            endif       ! if (present(aLayer)) then

          endif       ! if (iCount > aKeyID) then

        endif       ! if (present(aProc)) then

      endif       ! if (associated(aPKey) .and. aKeyID > 0) then

    endif

    ty_GetKey = irc
    return

  end function ty_GetKey

!
!----------------------
!

  integer(kind=INK) function ty_GetKeySet(aID, aKeySet)

    implicit none
    integer(kind=INK),         intent(in)  :: aID       ! KeySet index
    type(Key_Set_tp), pointer, intent(out) :: aKeySet   ! pointer to new KeySet

    integer(kind=INK)            :: irc = TYPH_SUCCESS
    character(len=TYPH_STRLEN)   :: errstr
    character(len=SUBNAME_LEN)   :: subname = "ty_GetKeySet"

    nullify(aKeySet)

    if (aID < 1 .or. aID > mNKeySets) then
      errstr = "Invalid ID: "//trim(adjustl(errstr))
      write(errstr, '(i8)') aID
      irc = ty_ErrorCheck(.false. , subname, ERR_INT, TYPH_ERR_INT, errstr)
    else
      aKeySet => KeySetArray(aID)%KeySet
    endif

    ty_GetKeySet = irc
    return

  end function ty_GetKeySet

end module TYPH_Key_mod

! EOF
