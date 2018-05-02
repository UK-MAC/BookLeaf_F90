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
  use TYPH_util_mod, ONLY: TYPH_gettime

  implicit none

  private :: TYPH_Start_Exch,TYPH_Finish_Exch
  public  :: TYPH_exchange

contains

  integer(kind=INK) function TYPH_Start_Exch(PhaseID, NumGhosts)

    integer(kind=INK), intent(in)            :: PhaseID
    integer(kind=INK), intent(in), optional  :: NumGhosts

    TYPH_Start_Exch = 0

  end function TYPH_Start_Exch

  integer(kind=INK) function TYPH_Finish_Exch(PhaseID)

    integer(kind=INK), intent(in) :: PhaseID

    TYPH_Finish_Exch = 0

  end function TYPH_Finish_Exch

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
