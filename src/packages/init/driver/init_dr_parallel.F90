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
MODULE init_dr_parallel_mod

  ! Internal
  USE dataAPI_kinds_mod,    ONLY: ink,lok
  USE dataAPI_types_mod,    ONLY: comms_t,comm_t,error_t
  USE dataAPI_params_mod,   ONLY: HALT_ALL
  USE typhon_API_mod,       ONLY: TYPH_finish_register,TYPH_init,TYPH_get_size,&
&                                 TYPH_get_rank,TYPH_set_comm,TYPH_Kill,       &
&                                 TYPH_Abort,TYPH_set_comm_self,               &
&                                 TYPH_add_reduce_dt
  ! External
#ifndef NOOMP
  USE omp_lib
#endif

  IMPLICIT NONE

  PUBLIC  :: init_dr_mpi,init_dr_omp,init_dr_phasestypes,init_dr_kill,         &
&            init_dr_mpi_comm
  PRIVATE :: init_dr_setcomm,init_dr_nullcomm,init_dr_copycomm
 
CONTAINS

  SUBROUTINE init_dr_mpi(comm)

    ! Argument list
    TYPE(comms_t),INTENT(INOUT) :: comm
    ! Local
    INTEGER(KIND=ink) :: ierr

    ! initialise MPI
    ierr=TYPH_init()
    ! associate all communicators
    CALL init_dr_setcomm(comm%world)
    CALL init_dr_nullcomm(comm%spatial)
    CALL init_dr_nullcomm(comm%replicant)
    ! global info
    comm%zmpi=.FALSE._lok
    IF (comm%world%nproc.GT.1_ink) comm%zmpi=.TRUE._lok

  END SUBROUTINE init_dr_mpi

  SUBROUTINE init_dr_kill(error)

    ! Argument list
    TYPE(error_t),INTENT(IN) :: error
    ! Local
    INTEGER(KIND=ink) :: ierr

    ! Stop program
    IF (error%iout.EQ.HALT_ALL) THEN
      ierr=TYPH_Kill(FinalizeMPI=.true.)
    ELSE
      ierr=TYPH_Abort(-1)
    ENDIF
    STOP

  END SUBROUTINE init_dr_kill

  SUBROUTINE init_dr_omp(comm)

    ! Argument list
    TYPE(comms_t),INTENT(INOUT) :: comm

#ifdef NOOMP 
    comm%nthread=1_ink
#else
!#OMP parallel
    comm%nthread=OMP_Get_Max_Threads()
!#OMP end parallel
#endif
   
  END SUBROUTINE init_dr_omp

  SUBROUTINE init_dr_phasestypes()

    ! Local
    INTEGER(KIND=ink) :: ierr

    ! Finish registry
    ierr=TYPH_finish_register()

    ! Initialise dt reduction type
    ierr=TYPH_add_reduce_dt()

  END SUBROUTINE init_dr_phasestypes

  SUBROUTINE init_dr_mpi_comm(comm)

    ! Argument list
    TYPE(comms_t),INTENT(INOUT) :: comm

    ! spatial communicator
    CALL init_dr_copycomm(comm%world,comm%spatial)

    ! replicate communicator
    CALL init_dr_nullcomm(comm%replicant)

  END SUBROUTINE init_dr_mpi_comm

  SUBROUTINE init_dr_setcomm(comm)

    ! Argument list
    TYPE(comm_t),POINTER,INTENT(INOUT) :: comm
    ! Local
    INTEGER(KIND=ink) :: ierr

    IF (.NOT.ASSOCIATED(comm)) ALLOCATE(comm)
    ierr=TYPH_get_size(comm%nproc)
    ierr=TYPH_get_rank(comm%rank)
    ierr=TYPH_set_comm(comm%comm)
    comm%zmproc=.FALSE._lok
    IF (comm%rank.EQ.0_ink) comm%zmproc=.TRUE._lok

  END SUBROUTINE init_dr_setcomm

  SUBROUTINE init_dr_nullcomm(comm)

    ! Argument list
    TYPE(comm_t),POINTER,INTENT(INOUT) :: comm
    ! Local
    INTEGER(KIND=ink) :: ierr

    IF (.NOT.ASSOCIATED(comm)) ALLOCATE(comm)
    comm%nproc=1_ink
    comm%rank=-1_ink
    ierr=TYPH_set_comm_self(comm%comm)
    comm%zmproc=.FALSE._lok

  END SUBROUTINE init_dr_nullcomm

  SUBROUTINE init_dr_copycomm(commin,commout)

    ! Argument list
    TYPE(comm_t),        INTENT(IN)    :: commin
    TYPE(comm_t),POINTER,INTENT(INOUT) :: commout

    IF (.NOT.ASSOCIATED(commout)) ALLOCATE(commout)
    commout=commin

  END SUBROUTINE init_dr_copycomm

END MODULE init_dr_parallel_mod
