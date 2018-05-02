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
module TYPH_Schedule_mod

  use TYPH_Types_mod
  use TYPH_Register_mod
  use TYPH_Key_mod, only: ty_GetKeySet, ty_GetKey
  use TYPH_Decomposition_mod, only: Part_Info_tp, ty_GetPartition
  use TYPH_util_mod

  implicit none

  integer(kind=INK), save :: mMaxCommProc = 0     ! Max procs to send to
  integer(kind=INK), save :: mMaxParts    = 0     ! Largest SchedulePart size

contains

  integer(kind=INK) function ty_BuildSchedule( aPhaseID, NumGhosts )

! This routine works out all the information required to build the derived MPI types in ty_commit
! Routine breakdown is essentially:

    integer(kind=INK), intent(in)            :: aPhaseID
    integer(kind=INK), intent(in), optional  :: NumGhosts ! Number of ghost layers
                                                             ! requested by Ext
    type(V3_Schedule_tp),   pointer  :: iSchedule
    type(V3_Schedule_tp),   pointer  :: iNewSchedule
    type (Phase_tp),        pointer  :: iPhase
    type (MP_tp),           pointer  :: iMP
    type(V3_comm_Quant_tp), pointer  :: iPQinfo
    type (Quant_tp),        pointer  :: iQuant
    type(Key_Set_tp),       pointer  :: iKeySet
    type(KeyLL_tp),         pointer  :: iKey
    type (Part_Info_tp),    pointer  :: iPartInfo

    integer(kind=INK)                           :: ii
    integer(kind=INK)                           :: jj
    integer(kind=INK)                           :: kk
    integer(kind=INK)                           :: irc
    integer(kind=INK)                           :: iAllocStat
    character(len=SUBNAME_LEN)                  :: subname = "mBuildSchedule"
    integer(kind=INK), dimension(:),allocatable :: iDims       ! Array of non-mesh dims
    integer(kind=INK), dimension(:),allocatable :: iScounts    ! No. valid Send keys to each proc
    integer(kind=INK), dimension(:),allocatable :: iRcounts    ! No. valid Recv keys from each proc
    integer(kind=INK)                           :: iTcounts    ! Total send + recv counts
    integer(kind=INK)                           :: iMinLay     ! Minimum Ghost layer
    integer(kind=INK)                           :: iMaxLay     ! Maximum Ghost layer
    integer(kind=INK)                           :: iKeyID      ! Key index
    integer(kind=INK)                           :: iQuantSize  ! Quant Size
    integer(kind=INK)                           :: iDimProd    ! Product of Quant dimensions
    integer(kind=INK)                           :: iLayer      ! Current layer
    integer(kind=INK)                           :: iNSend      ! Number of nodes/cells sent
    integer(kind=INK)                           :: iNRecv      ! Number of nodes/cells received
    integer(kind=INK)                           :: iSend       ! Send counter
    integer(kind=INK)                           :: iRecv       ! Recv counter
    integer(kind=INK)                           :: iProc       ! Processor counter
    integer(kind=INK)                           :: iPos        ! Current index in array
    integer(kind=INK)                           :: iStart      ! Start index
    logical                                     :: iQSendalloc ! Send Quant allocated?
    logical                                     :: iQRecvalloc ! Receive Quant allocated?

    nullify(iSchedule,iNewSchedule,iPhase,iMP)
    nullify(iPQinfo,iQuant,iKeySet,iKey,iPartInfo)

!   Get Phase, and the Phase's keyset to get a pointer to the MPI info
    irc =  ty_GetPhase(aPhaseID, iPhase)
    irc =  ty_GetKeySet(iPhase%keySetID, iKeySet)
    irc =  ty_GetPartition(iKeySet%partitionID, iPartInfo)
    iMP => ty_GetMP()

!   These will store the number of keys that will be sent or received to/from each proc
!   (keysets may have keys for layers we don't care about so we'll ignore the keys outside
!   our range)
    allocate(iScounts(0:iMP%size-1), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iScountslist")
    iScounts = 0
    allocate(iRcounts(0:iMP%size-1), stat=iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iRcountslist")
    iRcounts = 0

!   Loop over all quants in this phase and get T3 Quant info for each (PQinfo)

    iPQinfo => iPhase%PQinfo

    do kk=1,iPhase%nquants
      if (.not. associated(iPQinfo)) then
        write(0,*) "Warning! Less Quants than expected"
        exit
      endif

!     Ghost layers are set to the value stored in Quant (which default to phase value)
      iMinLay = iPQinfo%ghostsMin
      iMaxLay = iPQinfo%ghostsMax

!     Get KeySet for this Quant in this Phase. Each Quant could have unique KeySet.
      irc = ty_GetKeySet(iPQinfo%keySetID, iKeySet)
      if (.not. associated(iKeySet)) then
        irc = ty_ErrorCheck(.false. , subname, ERR_INT, TYPH_ERR_INT, "iKeySet not found")
      endif

      iPQinfo%keySet => iKeySet

!     Send and receive Quant can be different. One or other could be unallocated.
!     We need to work out if a) anything is being sent or received and b) is the array allocated

!     Part a) Anything being sent/received?
!     We don't need the exact number of sends/receives, just if it is >0
      iNSend = 0
      if (associated(iKeySet%send_keys)) then
        iKey => iKeySet%send_keys
        do
          iLayer = iKey%layer
          if ((iLayer >= iMinLay) .and. (iLayer <= iMaxLay) .and. iKey%nlist > 0) then
            iNSend = iNSend + 1
          endif
          if (.not. associated(iKey%next) .OR. iNSend > 0) then
            exit   ! -something- is being sent; don't care about number yet
          else
            iKey => iKey%next
          endif
        enddo
      endif

      iNRecv = 0
      if (associated(iKeySet%recv_keys)) then
        iKey => iKeySet%recv_keys
        do
          iLayer = iKey%layer
          if ((iLayer >= iMinLay) .and. (iLayer <= iMaxLay) .and. iKey%nlist > 0) then
            iNRecv = iNRecv + 1
          endif
          if (.not. associated(iKey%next) .OR. iNRecv > 0) then
            exit   ! -something- is being received; don't care about number yet
          else
            iKey => iKey%next
          endif
        enddo
      endif

!     Now part b) Is the array associated?
!     Note: the schedule builder will ignore unassociated Quants that should be
!     sending/receiving data. Assumption is that this Quant is not involved.
!     This can occur for Phases where the Quant can change each time
!     This can lead to errors though where the user forgot to set the Quant
!     address for a valid Quant
!     Unassociated Quants that are not sending/receiving will be ignored later on
      iQSendalloc = .false.
      iQRecvalloc = .false.
      irc = ty_GetQuant(iPQinfo%receiveQuantID, iQuant)
      if (iQuant%quant_address == TYPH_NULL_ADDR) then
        if (iNRecv > 0) then
          iPQinfo => iPQinfo%next
          cycle
        endif
      else
        iQRecvalloc = .true.
      endif
      irc = ty_GetQuant(iPQinfo%quantID, iQuant)
      if (iQuant%quant_address == TYPH_NULL_ADDR) then
        if (iNSend > 0) then
          iPQinfo => iPQinfo%next
          cycle
        endif
      else
        iQSendalloc = .true.
      endif

!     Check Ghost range is valid for sending Quant
      irc = ty_ErrorCheck((iKeySet%lmin <= iMinLay) .or. (iKeySet%lmax >= iMaxLay),   &
                           subname, ERR_INT, TYPH_ERR_INT,                            &
                           "Key set has insufficient ghost layers for Quant")

!     Set dimension for this quant in T3 Quant type
      iQuantSize = 1
      if (iQuant%rank > 1) then
        allocate(iDims(iQuant%rank-1))
        jj=0
        do ii = 1, SIZE(iQuant%dims)
          if (iQuant%dims(ii) /= TYPH_MESH_DIM) then
            jj = jj+1
            iDims(jj) = iQuant%dims(ii)
          endif
        enddo
        iDimProd = product(iDims)
        deallocate(iDims)
        iQuantSize = iDimProd/iKeySet%stride
      endif

!     For Quants with >1D where index is not ordered (for example) (3,nel)
!     we set the quantsize to 1 and define the quant's stride as the mesh dimension value.
!     e.g.
!        arr(3,nel) - "default" arrangement, stride =1, quantsize=3,nrepeat=1
!        arr(nel,3) - "reversed"           stride =nel,quantsize=1,nrepeat=1
!
!        As we use the mesh based array bounds the stride will also be worked out correctly
!        where the mesh dimension is (for example) arr(0:nel+2, 3)
!
!       Will also work for 3D arrays with "wrong" order, e.g. arr(3,nel,2)
!       Quant%meshdim gives the dimension that is mesh based (2 in this case)
!
      if (iQuant%meshdim /= iQuant%rank) then
        iPQinfo%nrepeat   = iQuantSize
        iPQinfo%quantSize = 1
!       Use upper/lower bound of array to work out stride
        iPQinfo%stride = iQuant%uppbnd(iQuant%meshdim)-iQuant%lowbnd(iQuant%meshdim)+1
!             e.g. arr(1:5)=>stride=5
!                  arr(0:5)=>stride=6
      else
!       nothing special needs doing for the "default" ordering
        iPQinfo%nrepeat   = 1
        iPQinfo%quantSize = iQuantSize
        iPQinfo%stride    = 1
      endif

      iPQinfo%oldMpiType => iQuant%mpi_datatype !this may be NULL type. Will work out later

!     Work out exact number of send and receive keys to/from each neighbouring processor

!     Sends - loop through list of keys until all are processed
!     Ignore unassociated Quants
      if (associated(iKeySet%send_keys)) then
        iKey => iKeySet%send_keys
        do
          iLayer = iKey%layer
          if ((iLayer >= iMinLay) .and. (iLayer <= iMaxLay) .and. iQSendalloc) then
            iScounts(iKey%proc) = iScounts(iKey%proc) + 1
          endif
          if (.not. associated(iKey%next)) then
            exit
          else
            iKey => iKey%next
          endif
        enddo
      endif

!     Receives - loop through list of keys until all are processed
!     Ignore unassociated Quants
      if (associated(iKeySet%recv_keys)) then
        iKey => iKeySet%recv_keys
        do
          iLayer = iKey%layer
          if ((iLayer >= iMinLay) .and. (iLayer <= iMaxLay) .and. iQRecvalloc) then
            iRcounts(iKey%proc) = iRcounts(iKey%proc) + 1
          endif
          if (.not. associated(iKey%next)) then
            exit
          else
            iKey => iKey%next
          endif
        enddo
      endif

!     Next Quant in this phase

      iPQinfo => iPQinfo%next

    enddo                 ! End of loop over all quants in this phase.

    if (associated(iPQinfo)) then
      write(0,*) "Warning! More Quants than expected"
    endif


!   Allocate new schedule
    iSchedule => iPhase%schedule

    if (.not. associated(iSchedule)) then     ! First Quant added
      allocate(iNewSchedule, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iNewSchedule")
!       irc = ty_MemAdd(40450+PhaseID, TYPH_REAL, size(iSchedule))
      iPhase%schedule => iNewSchedule
      iSchedule       => iPhase%schedule
      iSchedule%nsend=0
      iSchedule%nrecv=0
      nullify(iSchedule%send_requests)
      nullify(iSchedule%recv_requests)
      nullify(iSchedule%parts)
    else
      irc = ty_ErrorCheck(.false. , subname, ERR_INT, TYPH_ERR_INT,    &
                          "Schedule already exists for phase")
    endif


!   Store the number of procs we are sending/receiving to/from

    iNSend = 0
    iNRecv = 0

    do iProc = 0, iMP%size-1
      if (iScounts(iProc) > 0) iNSend = iNSend + 1
      if (iRcounts(iProc) > 0) iNRecv = iNRecv + 1
    enddo

    iSchedule%nsend = iNSend
    iSchedule%nrecv = iNRecv

    iTCounts = sum(iScounts) + sum(iRCounts)
    if (iTCounts > 0) then
      allocate(iSchedule%parts(iTCounts), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%parts")
    endif


!   *** Send Schedule ***
!   Set up send data start positions and displacements
!   Schedule will have one entry for each proc we send to
!   Each proc's messages will be broken into nparts where nparts = no. send keys to that proc

    iPos = 1

    if (iNSend > 0) then
      allocate(iSchedule%send_proc(iNSend), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%send_proc")
!       irc = ty_MemAdd(41000+PhaseID, TYPH_INT, iNSend)
      allocate(iSchedule%mpi_send_tp(iNSend), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%mpi_send_tp")
!       irc = ty_MemAdd(41100+PhaseID, TYPH_INT, iNSend)
      allocate(iSchedule%send_requests(iNSend), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%send_requests")
!       irc = ty_MemAdd(41200+PhaseID, TYPH_INT, iNSend)
      allocate(iSchedule%send_nparts(iNSend), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%send_nparts")
!       irc = ty_MemAdd(41300+PhaseID, TYPH_INT, iNSend)
      allocate(iSchedule%send_start(iNSend), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%send_start")
!       irc = ty_MemAdd(41400+PhaseID, TYPH_INT, iNSend)

      iNSend = 0
      do iProc = 0, iMP%size-1
        if (iScounts(iProc) > 0) then
          iNSend = iNSend + 1
          iSchedule%send_proc(iNSend)   = iProc
          iSchedule%mpi_send_tp(iNSend) = MPI_DATATYPE_NULL
          iSchedule%send_nparts(iNSend) = iScounts(iProc)
          iSchedule%send_start(iNSend)  = iPos
          iPos = iPos + iScounts(iProc)
        endif
      enddo

!     Build up schedule parts. Loop over Quants and then ghost layers within each Quant
!     Add each valid send key as we find it.

      do iSend = 1, iSchedule%nsend
        iProc   =  iSchedule%send_proc(iSend)
        iStart  =  iSchedule%send_start(iSend)
        iPQinfo => iPhase%PQinfo
        do
          if (.not. associated(iPQinfo)) then
            exit
          endif
          irc = ty_GetQuant(iPQinfo%quantID, iQuant)
          iKeySet => iPQinfo%keySet

!         Don't set schedule for this Quant if it has a NULL address
          if (iQuant%quant_address == TYPH_NULL_ADDR) then
            iPQinfo => iPQinfo%next
            cycle
          endif

!         Work out min/max layer
          iMinLay = iPQinfo%ghostsMin
          iMaxLay = iPQinfo%ghostsMax

          do iLayer = iMinLay, iMaxLay
            irc = ty_GetKey(iKeySet, TYPH_V3_SEND, iKeyID, iKey, iProc, iLayer)
            if (iKeyID > 0) then
              if (iKeyID > 1) then
                irc = ty_ErrorCheck(.false. , subname, ERR_INT, TYPH_ERR_INT,  &
                      &             "More than one Send Key found.")
              else
                if (.not. associated(iKey)) then
                  print*,"Error send key!", isend, iPQinfo%quantID, iLayer, iKeyID
                endif
                nullify(iSchedule%parts(iStart)%key)
                nullify(iSchedule%parts(iStart)%old_mpi_tp)
                nullify(iSchedule%parts(iStart)%address)
                iSchedule%parts(iStart)%key        => iKey
                iSchedule%parts(iStart)%quantSize  =  iPQinfo%quantSize
                iSchedule%parts(iStart)%old_mpi_tp => iPQinfo%oldMpiType
                iSchedule%parts(iStart)%address    => iQuant%quant_address
                iSchedule%parts(iStart)%nrepeat    =  iPQinfo%nrepeat
                iSchedule%parts(iStart)%stride     =  iPQinfo%stride
                iSchedule%parts(iStart)%new_mpi_tp =  MPI_DATATYPE_NULL
                iStart = iStart + 1
              endif
            endif
          enddo
          iPQinfo => iPQinfo%next
        enddo

      enddo

    endif

!   *** Receive Schedule ***
!   Set up receive data start positions and displacements
!   Schedule will have one entry for each processor this processor receives from

    if (iNRecv > 0) then
      allocate(iSchedule%recv_proc(iNRecv), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%recv_proc")
!       irc = ty_MemAdd(41000+PhaseID, TYPH_INT, iNRecv)
      allocate(iSchedule%mpi_recv_tp(iNRecv), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%mpi_recv_tp")
!       irc = ty_MemAdd(41100+PhaseID, TYPH_INT, iNRecv)
      allocate(iSchedule%recv_requests(iNRecv), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%recv_requests")
!       irc = ty_MemAdd(41200+PhaseID, TYPH_INT, iNRecv)
      allocate(iSchedule%recv_nparts(iNRecv), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%recv_nparts")
!       irc = ty_MemAdd(41300+PhaseID, TYPH_INT, iNRecv)
      allocate(iSchedule%recv_start(iNRecv), stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iSchedule%recv_start")
!       irc = ty_MemAdd(41400+PhaseID, TYPH_INT, iNRecv)

      iNRecv = 0
      do iProc = 0, iMP%size-1
        if (iRcounts(iProc) > 0) then
          iNRecv = iNRecv + 1
          iSchedule%recv_proc(iNRecv)   = iProc
          iSchedule%mpi_recv_tp(iNRecv) = MPI_DATATYPE_NULL
          iSchedule%recv_nparts(iNRecv) = iRcounts(iProc)
          iSchedule%recv_start(iNRecv)  = iPos
          iPos = iPos + iRcounts(iProc)
        endif
      enddo

!     Build up schedule parts. Loop over Quants and then ghost layers within each Quant
!     Add each valid recv key as we find it.


      do iRecv = 1, iSchedule%nrecv
        iProc   =  iSchedule%recv_proc(iRecv)
        iStart  =  iSchedule%recv_start(iRecv)
        iPQinfo => iPhase%PQinfo
        do
          if (.not. associated(iPQinfo)) then
            exit
          endif
          irc = ty_GetQuant(iPQinfo%receiveQuantID, iQuant)
          iKeySet => iPQinfo%keySet

!         Don't set schedule for this Quant if it has a NULL address
          if (iQuant%quant_address == TYPH_NULL_ADDR) then
            iPQinfo => iPQinfo%next
            cycle
          endif

!         Work out min/max layer
          iMinLay = iPQinfo%ghostsMin
          iMaxLay = iPQinfo%ghostsMax

          do iLayer = iMinLay, iMaxLay
            irc = ty_GetKey(iKeySet, TYPH_V3_RECV, iKeyID, iKey, iProc, iLayer)
            if (iKeyID > 0) then
              if (iKeyID > 1) then
                irc = ty_ErrorCheck(.false. , subname, ERR_INT, TYPH_ERR_INT,  &
                      &             "More than one Receive Key found.")
              else
                if (.not. associated(iKey)) then
                  print*,"Error recv key!", isend, iPQinfo%quantID, iLayer, iKeyID
                endif
                nullify(iSchedule%parts(iStart)%key)
                nullify(iSchedule%parts(iStart)%old_mpi_tp)
                nullify(iSchedule%parts(iStart)%address)
                iSchedule%parts(iStart)%key        => iKey
                iSchedule%parts(iStart)%quantSize  =  iPQinfo%quantSize
                iSchedule%parts(iStart)%old_mpi_tp => iPQinfo%oldMpiType
                iSchedule%parts(iStart)%address    => iQuant%quant_address
                iSchedule%parts(iStart)%nrepeat    =  iPQinfo%nrepeat
                iSchedule%parts(iStart)%stride     =  iPQinfo%stride
                iSchedule%parts(iStart)%new_mpi_tp =  MPI_DATATYPE_NULL
                iStart = iStart + 1
              endif
            endif
          enddo
          iPQinfo => iPQinfo%next
        enddo
      enddo

    endif


    mMaxCommProc = max(mMaxCommProc, iSchedule%nsend, iSchedule%nrecv)
    mMaxParts    = max(mMaxParts, maxval(iScounts), maxval(iRcounts))

    deallocate(iScounts, stat = iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iScounts")
    deallocate(iRcounts, stat = iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iRcounts")


    ty_BuildSchedule = irc

  end function ty_BuildSchedule


  integer(kind=INK) function ty_DeleteSchedule( aPhaseID )

    integer(kind=INK),     intent(in) :: aPhaseID

    type (Phase_tp),         pointer  :: iPhase    => null()
    type(V3_Schedule_tp),    pointer  :: iSchedule => null()
    type(V3_SchedulePart_tp),pointer  :: iSchedulePart => null()
    integer(kind=INK)                 :: iAllocStat
    integer(kind=INK)                 :: ii,irc

    irc =  ty_GetPhase(aPhaseID, iPhase)
    iSchedule => iPhase%schedule

!   First deallocate everything inside the Schedule
    if (allocated(iSchedule%recv_proc)) then
      deallocate(iSchedule%recv_proc, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%recv_proc")
    endif
    if (allocated(iSchedule%mpi_send_tp)) then
      do ii=1,iSchedule%nsend
        if (iSchedule%mpi_send_tp(ii) /= MPI_DATATYPE_NULL) then
          call MPI_TYPE_FREE(iSchedule%mpi_send_tp(ii), irc)
        endif
      enddo
      deallocate(iSchedule%mpi_send_tp, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%mpi_send_tp")
    endif
    if (associated(iSchedule%send_requests)) then
      deallocate(iSchedule%send_requests, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%send_requests")
    endif
    if (allocated(iSchedule%send_nparts)) then
      deallocate(iSchedule%send_nparts, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%send_nparts")
    endif
    if (allocated(iSchedule%send_start)) then
      deallocate(iSchedule%send_start, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%send_start")
    endif
    if (allocated(iSchedule%recv_proc)) then
      deallocate(iSchedule%recv_proc, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%recv_proc")
    endif
    if (allocated(iSchedule%mpi_recv_tp)) then
      do ii=1,iSchedule%nrecv
        if (iSchedule%mpi_recv_tp(ii) /= MPI_DATATYPE_NULL) then
          call MPI_TYPE_FREE(iSchedule%mpi_recv_tp(ii), irc)
        endif
      enddo
      deallocate(iSchedule%mpi_recv_tp, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%mpi_recv_tp")
    endif
    if (associated(iSchedule%recv_requests)) then
      deallocate(iSchedule%recv_requests, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%recv_requests")
    endif
    if (allocated(iSchedule%recv_nparts)) then
      deallocate(iSchedule%recv_nparts, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%recv_nparts")
    endif
    if (allocated(iSchedule%recv_start)) then
      deallocate(iSchedule%recv_start, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%recv_start")
    endif
    if (associated(iSchedule%parts)) then
      do ii = 1, iSchedule%nsend+iSchedule%nrecv
        iSchedulePart => iSchedule%parts(ii)
        if (iSchedulePart%new_mpi_tp /= MPI_DATATYPE_NULL) then
          call MPI_TYPE_FREE(iSchedulePart%new_mpi_tp, irc)
        endif
        nullify(iSchedulePart%key,iSchedulePart%address,iSchedulePart%old_mpi_tp)
      enddo
      deallocate(iSchedule%parts, stat = iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iSchedule%parts")
    endif

!   Then deallocate the Schedule itself
    deallocate(iPhase%schedule, stat = iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iPhase%schedule")

    ty_DeleteSchedule = irc

  end function ty_DeleteSchedule


  integer(kind=INK) function ty_CommitPhase( aPhase )

!   Takes all the schedule data for a Phase and creates and Commits an MPI derived type

    type (Phase_tp), pointer, intent(inout) :: aPhase

    type(V3_Schedule_tp),     pointer  :: iSchedule     => null()
    type(V3_SchedulePart_tp), pointer  :: iSchedulePart => null()
    type (MP_tp),             pointer  :: iMP           => null()
!    type(V3_comm_Quant_tp),   pointer  :: iPQinfo       => null()


    integer(kind=INK) :: irc

    integer(kind=INK) :: iCount      ! Counter
    integer(kind=INK) :: iSend       ! Send Counter
    integer(kind=INK) :: iRecv       ! Recv Counter
    integer(kind=INK) :: iProc       ! Processor counter
    integer(kind=INK) :: iAllocStat
    integer(kind=INK) :: iNlist
    integer(kind=INK) :: iList
    integer(kind=INK) :: ii
    integer(kind=INK) :: jj

    integer(kind=INK),  dimension(:), allocatable :: iMpiTypeArr      ! MPI Types
    integer(kind=INK),  dimension(:), allocatable :: iMpiBlklenArr    ! Blocklengths
    integer(kind=TMEMK),dimension(:), allocatable :: iMpiAddressArr   ! Addresses
    integer(kind=INK),  dimension(:), pointer     :: iPlist => null() ! list of indexes

    allocate(iMpiTypeArr(mMaxParts), stat = iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iMpiTypeArr")
    iMpiTypeArr = 0_INK
    allocate(iMpiBlklenArr(mMaxParts), stat = iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iMpiBlklenArr")
    iMpiBlklenArr = 0_INK
    allocate(iMpiAddressArr(mMaxParts), stat = iAllocStat)
    irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iMpiAddressArr")
    iMpiAddressArr = 0_TMEMK

    iMP => ty_GetMP()

!   Create derived MPI types to send and receive all required data for this phase
!   to and from each destination processor


    iSchedule => aPhase%schedule

!   *** Sends ***

    do iSend = 1, iSchedule%nsend

      iCount = 0

!     Just ensure the type has been freed up

      if (iSchedule%mpi_send_tp(iSend) /= MPI_DATATYPE_NULL) then
        call MPI_TYPE_FREE(iSchedule%mpi_send_tp(iSend), irc)
      endif

!     Build loop over destination procs
      do iProc = iSchedule%send_start(iSend),       & 
                 iSchedule%send_start(iSend) + iSchedule%send_nparts(iSend) - 1

        iSchedulePart => iSchedule%parts(iProc)
        if (iSchedulePart%new_mpi_tp /= MPI_DATATYPE_NULL) then
          call MPI_TYPE_FREE(iSchedulePart%new_mpi_tp, irc)
        endif

        if (iSchedulePart%key%nlist > 0) then

          iNlist = iSchedulePart%key%nlist * iSchedulePart%nrepeat

!         For 2D/3D arrays with the element/node index at the start, we need to 
!         create multiples of the displacement index. e.g. (nel,3) arrays will effectively
!         be three copies of a nel arrays displacements whereas a (3,nel) array will have
!         nel sets of blocks of 3

          if (iSchedulePart%nrepeat > 1) then
            allocate(iPlist(iSchedulePart%key%nlist*iSchedulePart%nrepeat))
            iList = 0
            do ii = 0, iSchedulePart%nrepeat-1
              do jj = 1, iSchedulePart%key%nlist
                iList=iList+1
                iPlist(iList) = ii*iSchedulePart%stride+iSchedulePart%key%list(jj)
              enddo
            enddo
          else
            iPlist => iSchedulePart%key%list
          endif

          if (associated(iSchedulePart%key%blockLens)) then
!           call MPI_TYPE_INDEXED( iSchedulePart%key%nlist,        &  ! count
            call MPI_TYPE_INDEXED( iNlist,                         &  ! count
                                   iSchedulePart%key%blockLens,    &  ! blocklen
!                                  iSchedulePart%key%list,         &  ! displacements
                                   iPlist,                         &  ! displacements
                                   iSchedulePart%old_mpi_tp,       &  ! old type
                                   iSchedulePart%new_mpi_tp,       &  ! new type
                                   irc                             )
          else

!           call MPI_TYPE_CREATE_INDEXED_BLOCK( iSchedulePart%key%nlist, &  ! count
            call MPI_TYPE_CREATE_INDEXED_BLOCK( iNlist,                  &  ! count
                    iSchedulePart%quantSize,                             &  ! blocklen
!                   iSchedulePart%key%list * iSchedulePart%quantSize,    &  ! displacements
                    iPlist * iSchedulePart%quantSize,                    &  ! displacements
                    iSchedulePart%old_mpi_tp,                            &  ! old type
                    iSchedulePart%new_mpi_tp,                            &  ! new type
                    irc)

          endif
          if (iSchedulePart%nrepeat > 1) then
            deallocate(iPlist)
          endif

          iCount = iCount + 1
!         Store information needed to create the final derived type
          iMpiAddressArr(iCount) = iSchedulePart%address
          iMpiTypeArr(iCount)    = iSchedulePart%new_mpi_tp
          iMpiBlklenArr(iCount)  = 1

        endif

      enddo

!     Create derived type and commit
      if (iCount > 0) then
        call MPI_TYPE_CREATE_STRUCT( iCount,                       &  ! count
                                     iMpiBlklenArr,                &  ! blocklens (all=1)
                                     iMpiAddressArr,               &  ! addresses
                                     iMpiTypeArr,                  &  ! MPI types
                                     iSchedule%mpi_send_tp(iSend), &  ! Combined Type
                                     irc)

        call MPI_TYPE_COMMIT(iSchedule%mpi_send_tp(iSend), irc)
      endif
    enddo


!   *** Receives ***

    do iRecv = 1, iSchedule%nrecv

      iCount = 0

      if (iSchedule%mpi_recv_tp(iRecv) /= MPI_DATATYPE_NULL) then
        call MPI_TYPE_FREE(iSchedule%mpi_recv_tp(iRecv), irc)
      endif

!     Build loop over destination procs
      do iProc = iSchedule%recv_start(iRecv),       & 
                 iSchedule%recv_start(iRecv) + iSchedule%recv_nparts(iRecv) - 1

        iSchedulePart => iSchedule%parts(iProc)
        if (iSchedulePart%new_mpi_tp /= MPI_DATATYPE_NULL) then
          call MPI_TYPE_FREE(iSchedulePart%new_mpi_tp, irc)
        endif

        if (iSchedulePart%key%nlist > 0) then

          iNlist = iSchedulePart%key%nlist * iSchedulePart%nrepeat

!         For 2D/3D arrays with the element/node index at the start, we need to 
!         create multiples of the displacement index. e.g. (nel,3) arrays will effectively
!         be three copies of a nel arrays displacements whereas a (3,nel) array will have
!         nel sets of blocks of 3

          if (iSchedulePart%nrepeat > 1) then
            allocate(iPlist(iSchedulePart%key%nlist*iSchedulePart%nrepeat))
            iList = 0
            do ii = 0, iSchedulePart%nrepeat-1
              do jj = 1, iSchedulePart%key%nlist
                iList=iList+1
                iPlist(iList) = ii*iSchedulePart%stride+iSchedulePart%key%list(jj)
              enddo
            enddo
          else
            iPlist => iSchedulePart%key%list
          endif

          if (associated(iSchedulePart%key%blockLens)) then
            call MPI_TYPE_INDEXED( iNlist,                         &  ! count
                                   iSchedulePart%key%blockLens,    &  ! blocklen
!                                  iSchedulePart%key%list,         &  ! displacements
                                   iPlist,                         &  ! displacements
                                   iSchedulePart%old_mpi_tp,       &  ! old type
                                   iSchedulePart%new_mpi_tp,       &  ! new type
                                   irc                             )
          else

            call MPI_TYPE_CREATE_INDEXED_BLOCK( iNlist,                  &  ! count
                      iSchedulePart%quantSize,                           &  ! blocklen
!                     iSchedulePart%key%list * iSchedulePart%quantSize,  &  ! displacements
                      iPlist * iSchedulePart%quantSize,                  &  ! displacements
                      iSchedulePart%old_mpi_tp,                          &  ! old type
                      iSchedulePart%new_mpi_tp,                          &  ! new type
                      irc)

          endif
          if (iSchedulePart%nrepeat > 1) then
            deallocate(iPlist)
          endif


          iCount = iCount + 1
!         Store information needed to create the final derived type
          iMpiAddressArr(iCount) = iSchedulePart%address
          iMpiTypeArr(iCount)    = iSchedulePart%new_mpi_tp
          iMpiBlklenArr(iCount)  = 1

        endif

      enddo

!     Create derived type and commit
      if (iCount > 0) then
        call MPI_TYPE_CREATE_STRUCT( iCount,                       &  ! count
                                     iMpiBlklenArr,                &  ! blocklens (all=1)
                                     iMpiAddressArr,               &  ! addresses
                                     iMpiTypeArr,                  &  ! MPI types
                                     iSchedule%mpi_recv_tp(iRecv), &  ! Combined Type
                                     irc)

        call MPI_TYPE_COMMIT(iSchedule%mpi_recv_tp(iRecv), irc)
      endif
    enddo

    deallocate(iMpiTypeArr)
    deallocate(iMpiBlklenArr)
    deallocate(iMpiAddressArr)

    ty_CommitPhase = irc

  end function ty_CommitPhase


end module TYPH_Schedule_mod

! EOF
