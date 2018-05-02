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
  
  use dataAPI_kinds_mod,ONLY: INK,RLK,LOK
  use dataAPI_types_mod,ONLY: dt_t
  use MPI
  
  implicit none
  
  public
  
  integer, parameter :: TMEMK            = MPI_ADDRESS_KIND
  
  integer, parameter :: TYPH_STRLEN      = 64      ! Length of strings in external API
  integer, parameter :: SUBNAME_LEN      = 30      ! Length of routine name strings
  
  integer(kind=INK), parameter :: TYPH_NULL  = -1      ! Widely-used NULL value - must be -ve
  integer(kind=INK), parameter :: TYPH_ALL   = -999    ! Widely-used special value - must be -ve
  
  
  integer(kind=INK), parameter :: TYPH_SIZEK       = 0       ! Not made app-public
  integer(kind=INK), parameter :: TYPH_REAL        = 1
  integer(kind=INK), parameter :: TYPH_INTEGER     = 2
  integer(kind=INK), parameter :: TYPH_LOGICAL     = 3
  integer(kind=INK), parameter :: TYPH_CHARACTER   = 4       ! Not made app-public
  integer(kind=INK), parameter :: TYPH_STRING      = 5       ! Not made app-public
  integer(kind=INK), parameter :: TYPH_TYPHK       = 6       ! Not made app-public
  integer(kind=INK), parameter :: TYPH_MEMK        = 7       ! Not made app-public
  integer(kind=INK), parameter :: TYPH_MPIK        = 8       ! Not made app-public

  integer(kind=INK), parameter :: TYPH_AUXID_NONE   = -9099


  integer(kind=INK), parameter :: TYPH_V3_SEND = 1001
  integer(kind=INK), parameter :: TYPH_V3_RECV = 1002

  integer(kind=INK), parameter :: TYPH_GHOSTS_ZERO  = 0
  integer(kind=INK), parameter :: TYPH_GHOSTS_ONE   = 1
  integer(kind=INK), parameter :: TYPH_GHOSTS_TWO   = 2
  integer(kind=INK), parameter :: TYPH_GHOSTS_THREE = 3

  integer(kind=INK), parameter :: TYPH_SHAPE_QUAD4  = 4

  integer(kind=INK), parameter :: TYPH_MESH_DIM     = -88

  integer(kind=TMEMK), parameter :: TYPH_NULL_ADDR = -999

  integer(kind=INK), parameter :: TY_MEM_ALLOC   = 1111
  integer(kind=INK), parameter :: TY_MEM_DEALLOC = 1112
  !
  ! Error classes:
  !
  integer(kind=INK), public, parameter :: ERR_USER = 101    ! User error
  integer(kind=INK), public, parameter :: ERR_MEM  = 102    ! Memory error
  integer(kind=INK), public, parameter :: ERR_MPI  = 103    ! MPI error
  integer(kind=INK), public, parameter :: ERR_INT  = 104    ! Internal error
  integer(kind=INK), public, parameter :: ERR_APP  = 105    ! For when application calls abort

  !
  ! Error codes:
  !
  integer(kind=INK), public, parameter :: TYPH_SUCCESS            =    0
  integer(kind=INK), public, parameter :: TYPH_FAIL               =   -1
  integer(kind=INK), public, parameter :: TYPH_ERR_USER           = -101
  integer(kind=INK), public, parameter :: TYPH_ERR_MEM            = -102
  integer(kind=INK), public, parameter :: TYPH_ERR_MPI            = -103
  integer(kind=INK), public, parameter :: TYPH_ERR_INT            = -104
  integer(kind=INK), public, parameter :: TYPH_ERR_APP            = -105
  integer(kind=INK), public, parameter :: TYPH_ERR_UNINITIALISED  = -110
  integer(kind=INK), public, parameter :: TYPH_ERR_INVALID_ARG    = -111
  integer(kind=INK), public, parameter :: TYPH_ERR_MISSING_ARG    = -112
  integer(kind=INK), public, parameter :: TYPH_ERR_INVALID_OP     = -113
  integer(kind=INK), public, parameter :: TYPH_ERR_UNKNOWN_MODE   = -114

! Ideally these should be defined in V3 directory
! Intel compiler cannot cope with class(*), pointer.
  type :: KeyLL_tp
    integer(kind=INK)                        :: layer   ! ghost layer that key belongs to
    integer(kind=INK)                        :: proc    ! proc to send or receive from
    integer(kind=INK), dimension(:), pointer :: list   => null()  ! list of elements or nodes
    integer(kind=INK)                        :: nlist             ! size of element/node list
!   Optional block lengths to variable amounts of data to be communicated per cell/node
    integer(kind=INK), dimension(:), pointer :: blockLens =>null()
    type (KeyLL_tp), pointer                 :: next   => null()  ! next key in linked list
    type (KeyLL_tp), pointer                 :: prev   => null()  ! previous key in linked list
    type (KeyLL_tp), pointer                 :: parent => null()  ! key this key is derived from
  end type KeyLL_tp


  type Key_Set_tp
    integer(kind=INK) :: centring    ! Type of variable centring (NODE/CELL)
    integer(kind=INK) :: AuxID       ! Type of Auxiliary data
    integer(kind=INK) :: stride      ! no. parts object is divided into (for 2D etc arrays)
    integer(kind=INK) :: lmin        ! smallest ghost layer index
    integer(kind=INK) :: lmax        ! largest ghost layer index
    integer(kind=INK) :: partitionID ! ID of Partition to use with Key
    integer(kind=INK) :: nSend       ! No. Send keys in set
    integer(kind=INK) :: nRecv       ! No. Recv keys in set
!   character(len=TYPH_STRLEN) :: name        ! key set name
    type (KeyLL_tp), pointer   :: send_keys => null()   ! linked list of send keys
    type (KeyLL_tp), pointer   :: recv_keys => null()   ! linked list of recv keys
  end type Key_Set_tp

  type :: V3_comm_Quant_tp
    integer(kind=INK)         :: quantID
    integer(kind=INK)         :: receiveQuantID
    integer(kind=INK)         :: KeySetID
    integer(kind=INK)         :: ghostsMin  ! Default Ghost layer range
    integer(kind=INK)         :: ghostsMax  ! Default Ghost layer range
    integer(kind=INK)         :: quantSize
    integer(kind=INK)         :: nrepeat
    integer(kind=INK)         :: stride
    integer(kind=INK),      pointer :: oldMpiType => null()
    integer(kind=INK),      pointer :: newMpiType => null()
    type(Key_Set_tp),       pointer :: keySet     => null() ! KeySet for this Quant
    type(V3_comm_Quant_tp), pointer :: next       => null()
  end type V3_comm_Quant_tp

  type V3_SchedulePart_tp
    integer(kind=INK)           :: new_mpi_tp
    integer(kind=INK)           :: quantSize
    integer(kind=INK)           :: nrepeat
    integer(kind=INK)           :: stride
    type(KeyLL_tp),     pointer :: key        => null()  ! offset array and length
    integer(kind=INK),  pointer :: old_mpi_tp => null()
    integer(kind=TMEMK),pointer :: address    => null()  ! Start address of item
  end type V3_SchedulePart_tp

  type V3_Schedule_tp
     integer(kind=INK)                              :: nsend      ! no. procs to send to
     integer(kind=INK),   dimension(:), allocatable :: send_proc   ! procs to send to
     integer(kind=INK),   dimension(:), allocatable :: mpi_send_tp ! MPI types of send data
!    MPI send requests
     integer(kind=INK), dimension(:), pointer       :: send_requests => null()
     integer(kind=INK), dimension(:), allocatable   :: send_nparts
!    Number of parts sent to each proc
     integer(kind=INK), dimension(:), allocatable   :: send_start
!    Start index in parts array for send to proc i

     integer(kind=INK)                              :: nrecv      ! no. procs to recv from
     integer(kind=INK),   dimension(:), allocatable :: recv_proc   ! procs to recv from
     integer(kind=INK),   dimension(:), allocatable :: mpi_recv_tp ! MPI types of recv data
!    MPI recv requests
     integer(kind=INK), dimension(:), pointer       :: recv_requests => null()
     integer(kind=INK), dimension(:), allocatable   :: recv_nparts
!    Number of parts received from each proc
     integer(kind=INK), dimension(:), allocatable   :: recv_start
!    Start index in parts array for recv from proc i
     type(V3_SchedulePart_tp),dimension(:), pointer :: parts  => null()
!    Array of all parts to/from all procs for this proc

  end type V3_Schedule_tp

end module TYPH_Types_mod
  
! EOF

