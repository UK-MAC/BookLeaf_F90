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
module TYPH_Collect_mod
  
  USE TYPH_Types_mod

  implicit none
  
  private
  
  public :: TYPH_Gather, TYPH_Reduce
  
  integer(kind=INK), public, parameter :: TYPH_OP_SUM  = 1001    ! No significance to these values
  integer(kind=INK), public, parameter :: TYPH_OP_PROD = 1002    ! - just chose them to be a bit
  integer(kind=INK), public, parameter :: TYPH_OP_MAX  = 1003    !   different to avoid possible
  integer(kind=INK), public, parameter :: TYPH_OP_MIN  = 1004    !   conflicts elsewhere
  integer(kind=INK), public, parameter :: TYPH_OP_OR   = 1011
  integer(kind=INK), public, parameter :: TYPH_OP_XOR  = 1012
  integer(kind=INK), public, parameter :: TYPH_OP_AND  = 1013

  interface TYPH_Reduce
    module procedure mReduce2D_Int
    module procedure mReduce1D_Real
    module procedure mReduce1D_Int
    module procedure mReduce0D_Real
    module procedure mReduce0D_Int
  end interface
  
  interface TYPH_Gather
    module procedure mAllGather0D_Real
    module procedure mAllGather0D_Int
    module procedure mAllGather1D_Int
  end interface

contains
  
  integer(kind=INK) function mAllGather0D_Real(Val, GVal, Comm) result(fres)

    implicit none

    real(kind=RLK),    intent(in)                  :: Val
    real(kind=RLK),    dimension(0:),intent(inout) :: GVal     ! intent(out)
    integer(kind=INK), intent(in)                  :: Comm

    integer(kind=INK) :: irc

    GVal(0) = Val

    fres = irc

  end function mAllGather0D_Real

  integer(kind=INK) function mAllGather0D_Int(Val, GVal, Comm) result(fres)

    implicit none

    integer(kind=INK), intent(in)                  :: Val
    integer(kind=INK), dimension(0:),intent(inout) :: GVal
    integer(kind=INK), intent(in)                  :: Comm

    integer(kind=INK) :: irc

    GVal(0) = Val

    fres = irc

  end function mAllGather0D_Int

  integer(kind=INK) function mAllGather1D_Int(Val, GVal, Comm) result(fres)
    
    implicit none

    integer(kind=INK),  dimension(:),   intent(in)    :: Val
    integer(kind=INK),  dimension(:,0:),intent(inout) :: GVal          ! intent(out)
    integer(kind=INK),                  intent(in)    :: Comm
    integer(kind=INK) :: irc        ! Internal return code
    
    GVal(:,0) = Val(:)

    fres = irc
   
  end function mAllGather1D_Int
  
  integer(kind=INK) function mReduce2D_Int(Val, RVal, Op, Comm)  result(fres)
    
    implicit none

    integer(kind=INK),  dimension(:,:), intent(in)  :: Val
    integer(kind=INK),  dimension(:,:), intent(out) :: RVal
    integer(kind=INK),                  intent(in)  :: Op
    integer(kind=INK),                  intent(in)  :: Comm

    integer(kind=INK) :: iMPIop                          ! MPI reduction operation
    integer(kind=INK) :: irc                             ! Internal return code

    RVal = Val
    fres = irc
    
  end function mReduce2D_Int

  integer(kind=INK) function mReduce1D_Real(Val, RVal, Op, Comm) result(fres)
    
    implicit none

    real(kind=RLK),    dimension(:), intent(in)  :: Val
    real(kind=RLK),    dimension(:), intent(out) :: RVal
    integer(kind=INK),               intent(in)  :: Op
    integer(kind=INK),               intent(in)  :: Comm
    
    integer(kind=INK) :: iMPIop       ! MPI reduction operation
    integer(kind=INK) :: irc          ! Internal return code

    RVal = Val
    fres = irc
    
  end function mReduce1D_Real


  integer(kind=INK) function mReduce1D_Int(Val, RVal, Op, Comm)  result(fres)
    
    implicit none

    integer(kind=INK),  dimension(:), intent(in)  :: Val
    integer(kind=INK),  dimension(:), intent(out) :: RVal
    integer(kind=INK),                intent(in)  :: Op
    integer(kind=INK),                intent(in)  :: Comm

    integer(kind=INK) :: iMPIop                          ! MPI reduction operation
    integer(kind=INK) :: irc                             ! Internal return code

    RVal = Val
    fres = irc
    
  end function mReduce1D_Int

  integer(kind=INK) function mReduce0D_Real(Val, RVal, Op, Comm) result(fres)
    
    implicit none

    real(kind=RLK),     intent(in)  :: Val
    real(kind=RLK),     intent(out) :: RVal
    integer(kind=INK),  intent(in)  :: Op
    integer(kind=INK),  intent(in)  :: Comm
    
    integer(kind=INK) :: iMPIop       ! MPI reduction operation
    integer(kind=INK) :: irc          ! Internal return code

    RVal = Val
    fres = irc
    
  end function mReduce0D_Real


  integer(kind=INK) function mReduce0D_Int(Val, RVal, Op, Comm)  result(fres)
    
    implicit none

    integer(kind=INK), intent(in)  :: Val
    integer(kind=INK), intent(out) :: RVal
    integer(kind=INK), intent(in)  :: Op
    integer(kind=INK), intent(in)  :: Comm

    integer(kind=INK) :: iMPIop                          ! MPI reduction operation
    integer(kind=INK) :: irc                             ! Internal return code

    RVal = Val
    fres = irc
    
  end function mReduce0D_Int

end module TYPH_Collect_mod


! EOF


