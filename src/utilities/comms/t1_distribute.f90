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
MODULE TYPH_distribute_mod

  ! Internal
  USE dataAPI_kinds_mod, ONLY: ink,lok
  USE dataAPI_types_mod, ONLY: config_t,sizes_t,data_t,error_t
  USE dataAPI_params_mod,ONLY: FAILURE,HALT_SINGLE

  IMPLICIT NONE

  PUBLIC  :: TYPH_distribute_mesh

 CONTAINS 

  SUBROUTINE TYPH_distribute_mesh(conndata,part,config,sizes,dh,error)

    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,INTENT(INOUT) :: conndata 
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,INTENT(INOUT) :: part
    TYPE(config_t),                              INTENT(IN)    :: config
    TYPE(sizes_t),                               INTENT(INOUT) :: sizes
    TYPE(data_t),     DIMENSION(:),  ALLOCATABLE,INTENT(INOUT) :: dh
    TYPE(error_t),                               INTENT(OUT)   :: error

    error%ierr=FAILURE
    error%iout=HALT_SINGLE
    error%serr="ERROR: attempting to distribute mesh with no MPI"

  END SUBROUTINE TYPH_distribute_mesh

END MODULE TYPH_distribute_mod
