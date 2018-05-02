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
  use TYPH_util_mod, only: ty_ErrorCheck,ty_MemCheck

  implicit none

  private

  public :: TYPH_Start_Register
  public :: TYPH_Finish_Register
  public :: TYPH_Add_Phase
  public :: TYPH_Get_Phase
  public :: TYPH_Add_Quant
  public :: TYPH_Get_Quant
  public :: TYPH_Tag_Quant

  public :: ty_RegisterKill
  
  public :: ty_GetPhase
  public :: ty_GetLLPhase
  public :: ty_GetQuant
  public :: ty_GetLLQuant
  public :: ty_IsRegistration
  
  public :: Phase_tp
  public :: Quant_tp
  
  
  integer(kind=INK), public, parameter :: TYPH_PURE            = 1

  integer(kind=INK), public, parameter :: TYPH_GHOSTS_NULL     = TYPH_NULL
  integer(kind=INK), public, parameter :: TYPH_GHOSTS_MAX      = 4


  integer(kind=INK), public, parameter :: TYPH_CENTRE_NODE     = 2001
  integer(kind=INK), public, parameter :: TYPH_CENTRE_CELL     = 2002

  type :: Phase_tp
    integer(kind=INK)          :: nghosts
    integer(kind=INK)          :: nquants
    logical                    :: pure
    character(len=TYPH_STRLEN) :: name
    integer(kind=INK), dimension(:), allocatable :: quantlist
!   Typhon V3 components
    integer(kind=INK)          :: keySetID   ! KeySet associated with this Phase
    integer(kind=INK)          :: ghostsMin  ! Default Ghost layer range
    integer(kind=INK)          :: ghostsMax  ! Default Ghost layer range
    type(V3_comm_Quant_tp),pointer:: PQinfo  =>null() ! Quant info for this phase
    type(V3_Schedule_tp),  pointer:: schedule=>null() ! schedule
    logical                    :: isBuilt       ! Schedule built?
    logical                    :: isCommit      ! Phase MPI committed?
  end type Phase_tp
  
  integer, parameter :: pPhase_tp_size = 224     ! in bytes
  
  type :: Quant_tp
    integer(kind=INK)          :: qdataID
    integer(kind=INK)          :: nghosts
    integer(kind=INK)          :: centring
    integer(kind=INK)          :: datatype
    logical                    :: pure, aux
    character(len=TYPH_STRLEN) :: name

!   Typhon V3 components
    integer(kind=INK)                        :: mpi_datatype
    integer(kind=INK)                        :: AuxID    ! Handle for type of Aux data
    integer(kind=INK)                        :: rank     ! rank of variable
    integer(kind=INK), dimension(:), pointer :: dims=>null() ! if rank > 1 then dims holds the sizes
                                                            ! for the non-mesh dimensions
    integer(kind=INK)                        :: meshdim  ! dimension of mesh that is mesh based
                                                            ! have cell or node index first
    integer(kind=INK)                        :: stride   ! stride across memory for multiD arrays
    integer(kind=TMEMK)                      :: quant_address = TYPH_NULL_ADDR
                                                            ! Start address of quant.
                                                            ! Only set after allocation
    integer(kind=INK),dimension(:),allocatable:: lowbnd  ! Array Lower boundary
    integer(kind=INK),dimension(:),allocatable:: uppbnd  ! Array Upper boundary
  end type Quant_tp
  
  integer, parameter :: pQuant_tp_size = 160     ! estimate in bytes
  

! Can this be more cleverly with F2003?  

  type :: PQLL_tp
    type (Phase_tp)                          :: phase
    type (Quant_tp)                          :: quant
    integer(kind=INK), dimension(:), pointer :: list => null()
    integer(kind=INK)                        :: nlist
    type (PQLL_tp), pointer                  :: next => null()
  end type PQLL_tp
  
  integer(kind=INK), parameter :: mLL_PHASE = 40000
  integer(kind=INK), parameter :: mLL_QUANT = 40001
  
  type (PQLL_tp), pointer, save :: mPhaseLL => null()    ! Base of temporaray linked-list (LL)
  type (PQLL_tp), pointer, save :: mQuantLL => null()
  
  type (Phase_tp), dimension(:), allocatable, target, save :: mPhaseArray
  type (Quant_tp), dimension(:), allocatable, target, save :: mQuantArray
  
  integer(kind=INK), save :: mNumPhases = 0
  integer(kind=INK), save :: mNumQuants = 0
  
  logical, save :: mInit         = .false.
  logical, save :: mRegistration = .false.

contains
  
  integer(kind=INK) function TYPH_Start_Register()

    implicit none
    integer(kind=INK) :: irc

    irc = ty_ErrorCheck(.not. mRegistration, "TYPH_Start_Register", ERR_USER, &
         &  TYPH_ERR_INVALID_OP,"Already in registration mode")

    ! Initialise the register
    irc = mRegisterInit()

    ! Flag that finalised Phase / Quant structures don't yet exist
    mRegistration = .true.

    TYPH_Start_Register = irc

  end function TYPH_Start_Register


  integer(kind=INK) function TYPH_Finish_Register()

    implicit none

    character(len=SUBNAME_LEN), parameter :: subname = "TYPH_Finish_Register"

    type (PQLL_tp),  pointer :: iPQLL => null()
    type (Phase_tp), pointer :: iPhaseLL => null(), iPhaseAr => null()
    type (Quant_tp), pointer :: iQuantLL => null(), iQuantAr => null()

    integer(kind=INK) :: is, iq
    integer(kind=INK), dimension(TYPH_REAL:TYPH_LOGICAL) :: iNQ

    integer(kind=INK) :: irc
    integer           :: iAllocStat
    !
    ! Converts temporary linked-lists of Phases & Quants into allocatable arrays
    !

    irc = ty_ErrorCheck(mInit, "TYPH_Finish_Register", ERR_USER,  &
         &  TYPH_ERR_INVALID_OP, "Register has not been initialised")

    irc = ty_ErrorCheck(mRegistration, "TYPH_Finish_Register", ERR_USER,  &
         &  TYPH_ERR_INVALID_OP, "Not in registration mode")

    ! Copy over the Quant LL into the Quant Table
    ! Keep a tally of the number of Quants of each datatype as this will be used to allocate
    ! the QuantData stores

    iNQ = 0

    if (mNumQuants > 0) then

      allocate (mQuantArray(mNumQuants), stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "mQuantArray")

      do iq = 1, mNumQuants

        irc = mGetLL(mQuantLL, mNumQuants, iq, iPQLL)
        iQuantLL => iPQLL%quant
        iQuantAr => mQuantArray(iq)

        iQuantAr = iQuantLL

        iNQ(iQuantAr%datatype) = iNQ(iQuantAr%datatype) + 1
        iQuantAr%qdataID = iNQ(iQuantAr%datatype)

      end do

      irc = mKillLL(mQuantLL, mNumQuants)

    end if

    ! Copy over the Phase LL into the Phase Table
    ! Track whether each Phase has any face- or edge-centred Quants.  If not tag this as it can
    ! make savings when calculating overlaps.
    ! Also track if there are no Aux Quants in a Phase - if this is the case then can turn off all
    ! Aux for the Phase

    if (mNumPhases > 0) then
      allocate (mPhaseArray(mNumPhases), stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "PhaseArray")

      do is = 1, mNumPhases
        irc = mGetLL(mPhaseLL, mNumPhases, is, iPQLL)
        iPhaseLL => iPQLL%phase
        iPhaseAr => mPhaseArray(is)
        iPhaseAr = iPhaseLL

        ! Fill Quant lookup tables within phases
        iPQLL%nlist = 0

        if(iPhaseAr%nquants > 0) then

          allocate(iPhaseAr%quantlist(iPhaseAr%nquants), stat=iAllocStat)
          irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iPhaseAr%quantlist")

          iPhaseAr%quantlist(1:iPhaseAr%nquants) = iPQLL%list(1:iPhaseAr%nquants)

          deallocate(iPQLL%list, stat=iAllocStat)
          irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iPQLL%list")

        endif

!       For Typhon 3 - ensure Phases need building before use.
        iPhaseAr%isBuilt   = .false.
      end do

      irc = mKillLL(mPhaseLL, mNumPhases)
    end if

    mRegistration = .false.

    TYPH_Finish_Register = irc

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

    character(len=SUBNAME_LEN), parameter :: subname = "TYPH_Add_Phase"

    type (PQLL_tp),  pointer :: iPhaseLL => null()
    type (Phase_tp), pointer :: iPhase => null()

    integer(kind=INK) :: irc

    ! Add Phase to temporary linked-list

    irc = ty_ErrorCheck(mRegistration, subname, ERR_USER, TYPH_ERR_INVALID_OP,  &
         &  "Not in registration mode")

    irc = mAddLL(mPhaseLL, mNumPhases, iPhaseLL)
    PhaseID = mNumPhases
    iPhase => iPhaseLL%phase
    iPhase%nghosts = NumGhosts
    iPhase%nquants = 0
!    iPhase%pure = (iand(PureOrAux, TYPH_PURE) == TYPH_PURE)
    iPhase%pure = .true.
    iPhase%name = trim(name)

    if (present(KeySetID)) then
      iPhase%keySetID = KeySetID
    else
      iPhase%keySetID = 0
    endif

    if (present(GhostsMin)) then
      iPhase%ghostsMin = GhostsMin
    else
      iPhase%ghostsMin = 1
    endif

    iPhase%ghostsMax = NumGhosts
    iPhase%isBuilt   = .false.
    iPhase%isCommit  = .false.

    TYPH_Add_Phase = irc

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

    type (PQLL_tp),  pointer :: iPhaseLL
    type (Phase_tp), pointer :: iPhase
    integer(kind=INK) :: irc

    if (mRegistration) then
      ! Get Phase from LL
      irc = mGetLL(mPhaseLL, mNumPhases, PhaseID, iPhaseLL)
      iPhase => iPhaseLL%phase
    else
      ! Get phase in the usual way from the table
      irc = ty_GetPhase(PhaseID, iPhase)
    end if

    if (present(Name)) then
      Name = iPhase%name
    end if
    if (present(NumGhosts)) then
      NumGhosts = iPhase%nghosts
    end if

    if (present(PureOrAux)) then
      if (iPhase%pure) then
        PureOrAux = TYPH_PURE
      else
        PureOrAux = 0
      end if
    end if

    if (present(KeySetID)) then
      KeySetID = iPhase%keySetID
    endif

    if (present(GhostsMin)) then
      GhostsMin = iPhase%ghostsMin
    endif

    TYPH_Get_Phase = irc

  end function TYPH_Get_Phase


  integer(kind=INK) function TYPH_Add_Quant( QuantID, Name, NumGhosts,       &
       &                                     Datatype, Centring, PureOrAux,  &
       &                                     AuxID, Dims )
    
    ! Add quant to temporary linked-list
    
    implicit none
    integer(kind=INK), intent(out) :: QuantID
    character(len=*),  intent(in)  :: Name
    integer(kind=INK), intent(in)  :: NumGhosts
    integer(kind=INK), intent(in)  :: Datatype
    integer(kind=INK), intent(in)  :: Centring
    integer(kind=INK), intent(in)  :: PureOrAux
    integer(KIND=INK), intent(in), optional :: AuxID
    integer(kind=INK), dimension(:), intent(in), optional :: Dims

    type (PQLL_tp),  pointer :: iQuantLL => null()
    type (Quant_tp), pointer :: iQuant => null()
    integer(kind=INK) :: irc
    integer(kind=INK) :: iAllocStat
    integer(kind=INK) :: ii

    irc = ty_ErrorCheck(mRegistration, "TYPH_Add_Quant", ERR_USER,  &
         &  TYPH_ERR_INVALID_OP, "Not in registration mode")

    irc = mAddLL(mQuantLL, mNumQuants, iQuantLL)
    QuantID = mNumQuants
    iQuant => iQuantLL%quant
    iQuant%nghosts  = NumGhosts
    iQuant%datatype = Datatype
    iQuant%centring = Centring
!    iQuant%pure     = (iand(PureOrAux, TYPH_PURE) == TYPH_PURE)
    iQuant%pure     = .true.
    iQuant%name     = trim(name)

    select case(Datatype)
    case (TYPH_REAL)
      iQuant%mpi_datatype = MPI_REAL8
    case (TYPH_INTEGER)
      iQuant%mpi_datatype = MPI_INTEGER
    case (TYPH_LOGICAL)
      iQuant%mpi_datatype = MPI_LOGICAL
    case default
      irc = ty_ErrorCheck(.false., "TYPH_Add_Quant", ERR_USER,  &
           &  TYPH_ERR_INVALID_OP, "Unknown mpi datatype")
    end select

    iQuant%AuxID = TYPH_AUXID_NONE
    if (present(AuxID)) then
      iQuant%AuxID = AuxID
    end if

    iQuant%meshdim  = 0
    iQuant%rank = 1
    if (present(Dims)) then
      iQuant%rank = size(Dims)
      if (iQuant%rank > 1) then
        allocate(iQuant%dims(iQuant%rank), stat=iAllocStat)
        irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iQuant%dims")
      end if
      do ii=1,iQuant%rank
        iQuant%dims(ii) = Dims(ii)
        if (Dims(ii)==TYPH_MESH_DIM) then
          iQuant%meshdim = ii
        endif
      enddo
    else
      if (iQuant%rank > 1) then
        irc = ty_ErrorCheck(.false., "TYPH_Add_Quant", ERR_USER,  &
           &  TYPH_ERR_MISSING_ARG, "Missing Dims argument for array > 1D")
      endif
      allocate(iQuant%dims(iQuant%rank), stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "iQuant%dims")
      iQuant%dims(1) = TYPH_MESH_DIM
    end if

    if (iQuant%meshdim == 0) then
      iQuant%meshdim = iQuant%rank
    endif

    TYPH_Add_Quant = irc

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

    type (PQLL_tp),  pointer :: iQuantLL => null()
    type (Quant_tp), pointer :: iQuant => null()
    integer(kind=INK) :: irc

    if (mRegistration) then
      ! Get Quant from LL
      irc = mGetLL(mQuantLL, mNumQuants, QuantID, iQuantLL)
      iQuant => iQuantLL%quant
    else
      ! Get Quant in usual way from table
      irc = ty_GetQuant(QuantID, iQuant)
    end if

    if (present(Name)) then
      Name = iQuant%name
    end if
    if (present(NumGhosts)) then
      NumGhosts = iQuant%nghosts
    end if
    if (present(Datatype)) then
      Datatype  = iQuant%datatype
    end if
    if (present(Centring)) then
      Centring  = iQuant%centring
    end if
    if (present(PureOrAux)) then
      if (iQuant%pure) then
        PureOrAux = TYPH_PURE
      else
        PureOrAux = 0
      end if
    end if
    if (present(AuxID)) then
      AuxID = iQuant%AuxID
    end if

    TYPH_Get_Quant = irc

  end function TYPH_Get_Quant
  
  integer(kind=INK) function TYPH_Tag_Quant( QuantID, PhaseID )

    implicit none
    integer(kind=INK),  intent(in) :: QuantID
    integer(kind=INK),  intent(in) :: PhaseID

    type (PQLL_tp), pointer :: iPhaseLL => null(), iQuantLL => null()
    integer(kind=INK), dimension(:), pointer :: iTempList => null()
    integer(kind=INK) :: iNSize
    integer           :: iAllocStat
    integer(kind=INK) :: irc

    irc = ty_ErrorCheck(mRegistration, "TYPH_Tag_Quant", ERR_USER,  &
         &  TYPH_ERR_INVALID_OP, "Not in registration mode")

    irc = mGetLL( mQuantLL, mNumQuants, QuantID, iQuantLL )
    irc = ty_ErrorCheck((irc == 0), "TYPH_Tag_Quant", ERR_USER,  &
         &  TYPH_ERR_INVALID_ARG, "Invalid QuantID")

    irc = mGetLL( mPhaseLL, mNumPhases, PhaseID, iPhaseLL )
    irc = ty_ErrorCheck((irc == 0), "TYPH_Tag_Quant", ERR_USER,   &
         &  TYPH_ERR_INVALID_ARG, "Invalid PhaseID")

    ! Allocate temporary quant list in phase LL
    !
    ! - we keep the temporary list of quants to be the maximum number of quants that could be
    !   tagged to the phase, ie. all the quants
    !   Although a bit wasteful of memory, it's still only a relatively small amount and
    !   it does get consolidated later.

    if (iPhaseLL%phase%nquants == 0) then
      allocate(iPhaseLL%list(mNumQuants), stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "list")
      iPhaseLL%list  = 0
      iPhaseLL%nlist = mNumQuants
    end if

    ! Check if Quant has already been tagged for this phase
    if (any(iPhaseLL%list == QuantID)) then
      TYPH_Tag_Quant = 0
      return
    end if

    ! Expand list if necessary
    if (mNumQuants > iPhaseLL%nlist) then
      ! Allocate to be new mNumQuants size, and add a few extra for further expansion, so we don't
      ! end up doing this every time
      iNSize = mNumQuants + 10

      nullify(iTempList)
      allocate(iTempList(iNSize), stat=iAllocStat)

      iTempList = 0
      iTempList(1:iPhaseLL%phase%nquants) = iPhaseLL%list(1:iPhaseLL%phase%nquants)

      deallocate(iPhaseLL%list, stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "list")

      iPhaseLL%list  => iTempList
      iPhaseLL%nlist =  iNSize
    end if


    iNSize = iPhaseLL%phase%nquants + 1

    iPhaseLL%phase%nquants = iNSize
    iPhaseLL%list(iNSize)  = QuantID

    TYPH_Tag_Quant = irc

  end function TYPH_Tag_Quant


  integer(kind=INK) function mRegisterInit()
    
    use :: TYPH_util_mod
    implicit none
    integer(kind=INK) :: irc    ! Internal return code
    
    mRegisterInit = 0
    
    if (mInit) then
      return
    end if
    
    ! register kill routine to Typhon core
    irc = ty_KillInsert(ty_RegisterKill)
    
    mNumPhases = 0
    mNumQuants = 0
    
    nullify(mPhaseLL)
    nullify(mQuantLL)
    
    mInit = .true.
    
  end function mRegisterInit
  
  
  integer(kind=INK) function ty_RegisterKill()
    
    implicit none
    
    integer(kind=INK) :: ii
    integer           :: iAllocStat
    integer(kind=INK) :: irc


    if (.not. mInit) then
      return
    end if

    if (mRegistration) then
      irc = TYPH_Finish_Register()
    end if

    ! Deallocate all the finalized Phase & Quant array structures
    if (mNumPhases > 0) then

      do ii = 1, mNumPhases
        if (mPhaseArray(ii)%nquants > 0) then
          deallocate(mPhaseArray(ii)%quantlist, stat=iAllocStat)
          irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC,  &
               &  "PhaseArray(ii)%quantlist")
        end if
      end do

      deallocate(mPhaseArray)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "PhaseArray")

    end if

    if (mNumQuants > 0) then
      deallocate(mQuantArray)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "QuantArray")
    end if

    mNumPhases     = 0
    mNumQuants     = 0

    mRegistration = .false.
    mInit         = .false.

    ty_RegisterKill = 0
    
  end function ty_RegisterKill

  integer(kind=INK) function ty_GetPhase(aPhaseID, aPhase)

    implicit none
    integer(kind=INK),        intent(in)  :: aPhaseID
    type (Phase_tp), pointer, intent(out) :: aPhase

    aPhase => mPhaseArray(aPhaseID)
    ty_GetPhase = 0

  end function ty_GetPhase


  integer(kind=INK) function ty_GetLLPhase(aPhaseID, aPhase)

    implicit none
    integer(kind=INK),        intent(in)  :: aPhaseID
    type (Phase_tp), pointer, intent(out) :: aPhase
    type (PQLL_tp),  pointer :: iPhaseLL => null()
    integer(kind=INK)      :: irc

    ! Get Phase from LL
    irc = mGetLL(mPhaseLL, mNumPhases, aPhaseID, iPhaseLL)
    aPhase => iPhaseLL%phase
    ty_GetLLPhase = 0

  end function ty_GetLLPhase


  integer(kind=INK) function ty_GetQuant(aQuantID, aQuant)

    implicit none
    integer(kind=INK),        intent(in)  :: aQuantID
    type (Quant_tp), pointer, intent(out) :: aQuant

    aQuant => mQuantArray(aQuantID)
    ty_GetQuant = 0

  end function ty_GetQuant

  integer(kind=INK) function ty_GetLLQuant(aQuantID, aQuant)

    implicit none

    integer(kind=INK),        intent(in)  :: aQuantID
    type (Quant_tp), pointer, intent(out) :: aQuant

    type (PQLL_tp),  pointer :: iQuantLL => null()
    integer(kind=INK)        :: irc

    irc = mGetLL(mQuantLL, mNumQuants, aQuantID, iQuantLL)

    aQuant => iQuantLL%quant

    ty_GetLLQuant = 0
    
  end function ty_GetLLQuant
  
!   integer(kind=INK) function ty_GetNoQuants( aNoQuants )
!     
!     implicit none
!     
!     integer(kind=INK),     intent(out)  :: aNoQuants
!     
!     aNoQuants = mNumQuants
! 
!     ty_GetNoQuants = 0
!     
!   end function ty_GetNoQuants
  
  
  logical function ty_IsRegistration()
    implicit none
    ty_IsRegistration = mRegistration
  end function ty_IsRegistration
  
  
  integer(kind=INK) function mAddLL(aLL, aN, aNode)

    implicit none
    type (PQLL_tp),    pointer       :: aLL       ! intent(in)
    integer(kind=INK), intent(inout) :: aN
    type (PQLL_tp),    pointer       :: aNode     ! intent(in)

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

      do is = 1, aN - 1      ! do loop not executed when mNumPhases = 1
        irc = ty_ErrorCheck( associated(aNode%next), "mAddLL", ERR_INT, TYPH_ERR_INT,  &
             &          "Next item in LL is not associated")
        aNode => aNode%next
      end do

      allocate(aNode%next, stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_ALLOC, "aNode%next")

      aNode => aNode%next
    end if

    nullify(aNode%list)
    nullify(aNode%next)
    aN = aN + 1
    mAddLL = 0

  end function mAddLL


  integer(kind=INK) function mGetLL(aLL, aN, aID, aNode)

    implicit none
    type (PQLL_tp),    pointer    :: aLL       ! intent(in)
    integer(kind=INK), intent(in) :: aN
    integer(kind=INK), intent(in) :: aID
    type (PQLL_tp),    pointer    :: aNode     ! intent(out)

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


  integer(kind=INK) function mGetNameLL(aLLtype, aName, aID)

    implicit none
    integer(kind=INK), intent(in)  :: aLLtype
    character(len=*),  intent(in)  :: aName
    integer(kind=INK), intent(out) :: aID

    type (PQLL_tp), pointer :: iNode     ! intent(out)
    integer(kind=INK)       :: ii
    integer(kind=INK)       :: irc

    aID = TYPH_NULL

    select case (aLLtype)
    case (mLL_PHASE)
      irc = -1
      iNode => mPhaseLL
      do ii = 1, mNumPhases
        if (iNode%phase%name == aName) then
          aID = ii
          irc = 0
          exit
        end if
        iNode => iNode%next
      end do
    case (mLL_QUANT)
      irc = -1
      iNode => mQuantLL
      do ii = 1, mNumQuants
        if (iNode%quant%name == aName) then
          aID = ii
          irc = 0
          exit
        end if
        iNode => iNode%next
      end do
    case default
      irc = ty_ErrorCheck( .false., "mGetNameLL", ERR_INT, TYPH_ERR_INT, "Invalid aLLtype")
      mGetNameLL = -1
      return
    end select

    mGetNameLL = irc

  end function mGetNameLL

  integer(kind=INK) function mKillLL(aLL, aN)

    implicit none

    type (PQLL_tp),    pointer    :: aLL       ! intent(in)
    integer(kind=INK), intent(in) :: aN

    type (PQLL_tp), pointer :: iCurrLL => null(), iNextLL => null()

    integer(kind=INK) :: ii
    integer           :: iAllocStat
    integer(kind=INK) :: irc


    ! Deallocate aLL, but leave aN as-is

    if (aN < 1) then
      mKillLL = 0
      return
    end if

    iNextLL => aLL

    do ii = 1, aN

      iCurrLL => iNextLL
      iNextLL => iCurrLL%next

      nullify(iCurrLL%next)

      deallocate(iCurrLL, stat=iAllocStat)
      irc = ty_MemCheck(iAllocStat, TY_MEM_DEALLOC, "iCurrLL")

      nullify(iCurrLL)

    end do

    nullify(aLL)

    mKillLL = 0

  end function mKillLL

end module TYPH_Register_mod

! EOF

