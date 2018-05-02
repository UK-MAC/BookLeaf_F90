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
MODULE inf_error_API_mod

  INTERFACE
    SUBROUTINE inf_error_halt(config,runtime,timer,dh,error)
      USE dataAPI_types_mod, ONLY: config_t,runtime_t,data_t,error_t
      USE timerAPI_types_mod,ONLY: timer_t
      IMPLICIT NONE
      TYPE(config_t),              INTENT(IN)    :: config
      TYPE(runtime_t),             INTENT(IN)    :: runtime
      TYPE(timer_t),  DIMENSION(:),INTENT(INOUT) :: timer
      TYPE(data_t),   DIMENSION(:),INTENT(IN)    :: dh
      TYPE(error_t),               INTENT(INOUT) :: error
    END SUBROUTINE inf_error_halt
  END INTERFACE

END MODULE inf_error_API_mod
