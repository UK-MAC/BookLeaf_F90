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
module TYPH_util_mod

  use TYPH_Types_mod
  use iso_fortran_env,ONLY: ERROR_UNIT

  implicit none

  public  :: TYPH_Init,TYPH_kill,TYPH_abort,TYPH_get_size,TYPH_get_rank,&
&            TYPH_gettime,TYPH_barrier,TYPH_set_comm,TYPH_set_comm_self

contains

  integer(kind=INK) function TYPH_Init( Comm )

    integer(kind=INK),intent(in),optional :: Comm

    TYPH_Init=0

  end function TYPH_Init

  integer(kind=INK) function TYPH_Kill( FinalizeMPI )

    logical,intent(in),optional :: FinalizeMPI

    TYPH_Kill=0

  end function Typh_Kill

  integer(kind=INK) function TYPH_Abort(Code)

    integer(kind=INK),intent(in) :: Code

    TYPH_Abort=0

  end function TYPH_Abort

  integer(kind=INK) function TYPH_get_size(Size)

    integer(kind=INK),         intent(out) :: Size

    Size=1_INK
    TYPH_get_size=0

  end function TYPH_get_size

  integer(kind=INK) function TYPH_get_rank(Rank)

    integer(kind=INK),         intent(out) :: Rank

    Rank=0_INK
    TYPH_get_rank=0

  end function TYPH_get_rank

  real(kind=RLK) function TYPH_gettime()

    call CPU_TIME(TYPH_gettime)

  end function TYPH_gettime

  integer(kind=INK) function TYPH_Barrier()

    implicit none

    TYPH_barrier=0

  end function TYPH_Barrier

  integer(kind=INK) function ty_ErrorCheck( aCondition, aRoutine, aClass, aCode, aString )

    implicit none

    logical,           intent(in) :: aCondition   ! if .false. then there's an error
    character(len=*),  intent(in) :: aRoutine     ! Calling routine name
    integer(kind=INK), intent(in) :: aClass       ! Class of error, using ERR_*
    integer(kind=INK), intent(in) :: aCode        ! Abort code to use
    character(len=*),  intent(in) :: aString      ! Error description string

    integer(kind=INK) :: irc

    ty_ErrorCheck = 0

    if (aCondition) then
      return
    end if
    ! If here, then the check failed and there's an error
    ty_ErrorCheck = aCode
    WRITE(ERROR_UNIT,*) aString

  end function ty_ErrorCheck

  integer(kind=INK) function ty_MemCheck(aStat, aAorD, aVar)
    
    implicit none
    
    integer,          intent(in) :: aStat      ! Default integer = allocate/deallocate istat
    integer(kind=INK),intent(in) :: aAorD      ! Flag for allocation or deallocation
    character(len=*), intent(in) :: aVar       ! Variable name
    
    character(len=64) :: ierrstr
    integer(kind=INK) :: irc        ! Internal return code
    
    ty_MemCheck = 0

    if (aStat /= 0) then
      select case (aAorD)
      case (TY_MEM_ALLOC)
        write(ierrstr,*) "Memory allocation error:", aStat, aVar
        write(ERROR_UNIT,*) ierrstr
        stop 
      case (TY_MEM_DEALLOC)
        write(ierrstr,*) "Memory deallocation error:", aStat, aVar
        write(ERROR_UNIT,*) ierrstr
        stop 
      case default
        ! Um, shouldn't get here
      end select
    end if

  end function ty_MemCheck

  integer(kind=INK) function set_info(pinfo)

    implicit none
    integer(kind=ink), intent(inout) :: pinfo

    pinfo    = -1
    set_info = 0

  end function set_info

  integer(kind=INK) function TYPH_set_comm(comm)

    implicit none
    integer(kind=ink), intent(inout) :: comm

    comm     = -1
    TYPH_set_comm = 0

  end function TYPH_set_comm

  integer(kind=INK) function TYPH_set_comm_self(comm)

    implicit none
    integer(kind=ink), intent(inout) :: comm

    comm          = -1
    TYPH_set_comm_self = 0

  end function TYPH_set_comm_self

end module TYPH_util_mod
