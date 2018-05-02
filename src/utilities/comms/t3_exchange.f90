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
module TYPH_Exchange_mod

  use TYPH_Types_mod
  use TYPH_util_mod
  use TYPH_Register_mod
  use TYPH_Schedule_mod

  implicit none

  private :: TYPH_Start_Exch,TYPH_Finish_Exch
  public  :: TYPH_exchange

contains

  integer(kind=INK) function TYPH_Start_Exch(PhaseID, NumGhosts)

    use TYPH_Decomposition_mod, only: Part_Info_tp, ty_GetPartition
    use TYPH_Key_mod,           only: ty_GetKeySet
    integer(kind=INK), intent(in)            :: PhaseID
    integer(kind=INK), intent(in), optional  :: NumGhosts

    type (Phase_tp),       pointer :: iPhase        => null()
    type (V3_Schedule_tp), pointer :: iSchedule     => null()
    type (MP_tp),          pointer :: iMP           => null()

    type(Key_Set_tp),    pointer  :: iKeySet      => null()
    type (Part_Info_tp), pointer  :: iPartInfo    => null()

    integer(kind=INK), dimension(:), pointer :: iRecvRequests => null()

    integer(kind=INK) :: iSend      ! Counter
    integer(kind=INK) :: iRecv      ! Counter
    integer(kind=INK) :: iNSend
    integer(kind=INK) :: iNRecv
    integer(kind=INK) :: iMpiTag
    integer(kind=INK) :: irc
    integer(kind=INK) :: iDestin    ! Message Destination ProcID
    integer(kind=INK) :: iSource    ! Message Source ProcID

!   Get MPI via Phase main keyset
    irc =  ty_GetPhase(PhaseID, iPhase)
    irc =  ty_GetKeySet(iPhase%keySetID, iKeySet)
    irc =  ty_GetPartition(iKeySet%partitionID, iPartInfo)
    iMP => ty_GetMP()

!   Build the Phase if required, should be done once for pure.
    if (.not. iPhase%isBuilt) then
      if (associated(iPhase%schedule)) then
        irc = ty_DeleteSchedule(PhaseID)
      endif
      irc = ty_BuildSchedule(PhaseID)
      iPhase%isBuilt = .true.
    endif
!   Commit needs doing once for pure Phases (on first pass)
    if (.not. iPhase%isCommit) then
      irc = ty_CommitPhase(iPhase)
      iPhase%isCommit = .true.
    endif

    iSchedule => iPhase%schedule

    iNSend = iSchedule%nsend
    iNRecv = iSchedule%nrecv

    iRecvRequests => iSchedule%recv_requests

    iMpiTag = PhaseID

!   Post Receives
    if (iNrecv > 0) then
      iRecvRequests = MPI_REQUEST_NULL
      do iRecv = 1, iNRecv
        iSource = iSchedule%recv_proc(iRecv)
        if (iSchedule%mpi_recv_tp(iRecv) /= MPI_DATATYPE_NULL) then
          call MPI_IRECV(MPI_BOTTOM,                      &
                         1,                               &
                         iSchedule%mpi_recv_tp(iRecv),    &
                         iSource,                         &
                         iMpiTag,                         &
                         iMP%comm,                        &
                         iRecvRequests(iRecv),            &
                         irc)
        endif
      enddo
    endif

!   Perform Sends
    if (iNSend > 0) then
      do iSend = 1, iNSend
        iDestin = iSchedule%send_proc(iSend)
        if (iSchedule%mpi_send_tp(iSend) /= MPI_DATATYPE_NULL) then
          call MPI_SEND(MPI_BOTTOM,                       &
                        1,                                &
                        iSchedule%mpi_send_tp(iSend),     &
                        iDestin,                          &
                        iMpiTag,                          &
                        iMP%comm,                         &
                        irc)
        endif
      enddo
    endif

    TYPH_Start_Exch = irc

  end function TYPH_Start_Exch

  integer(kind=INK) function TYPH_Finish_Exch(PhaseID)

    integer(kind=INK),     intent(in) :: PhaseID

    type (Phase_tp),       pointer    :: iPhase        => null()
    type (V3_Schedule_tp), pointer    :: iSchedule     => null()
    integer(kind=INK)                 :: irc

    irc = ty_GetPhase(PhaseID, iPhase)
    iSchedule => iPhase%schedule
    irc = mCompleteExch(iSchedule)

    TYPH_Finish_Exch = irc

  end function TYPH_Finish_Exch

  integer(kind=INK) function mCompleteExch(aSchedule)

    use TYPH_Schedule_mod, only: mMaxCommProc

    type (V3_Schedule_tp), pointer, intent(in) :: aSchedule

    integer(kind=INK), dimension(MPI_STATUS_SIZE, mMaxCommProc) :: iStatuses
    integer(kind=INK), dimension(:), pointer                    :: iRecvRequests => null()
    integer(kind=INK) :: iNSend
    integer(kind=INK) :: iNRecv
    integer(kind=INK) :: irc

    character(len=SUBNAME_LEN), parameter :: subname = "mCompleteExch"

    iNSend = aSchedule%nsend
    iNRecv = aSchedule%nrecv

    iRecvRequests => aSchedule%recv_requests

    if (iNRecv > 0) then
      call MPI_WAITALL(iNRecv, iRecvRequests, iStatuses, irc)
    endif

    mCompleteExch = irc

  end function mCompleteExch

  subroutine TYPH_exchange(PhaseID,timer)

    ! Argument list
    integer(kind=INK),intent(in)    :: PhaseID
    real(kind=rlk),   intent(inout) :: timer
    ! Local
    real(kind=rlk)    :: t0,t1
    integer(kind=INK) :: ierr

    ! Timer
    t0=TYPH_gettime()

    ! Exchange
    ierr=TYPH_Start_Exch(PhaseID)
    ierr=TYPH_Finish_Exch(PhaseID)

    ! Timing data
    t1=TYPH_gettime()
    timer=timer+t1-t0

  end subroutine TYPH_exchange

end module TYPH_Exchange_mod

! EOF
