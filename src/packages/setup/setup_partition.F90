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
MODULE setup_partition_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_types_mod, ONLY: config_t,sizes_t,data_t,error_t
  USE dataAPI_params_mod,ONLY: SUCCESS
  USE dataAPI_id_mod,    ONLY: indlocglobid,indtypeid,ndxid,ndyid
  USE timerAPI_types_mod,ONLY: timer_t
  USE timer_advance_mod, ONLY: timer_start,timer_end
  USE setup_mesh_mod,    ONLY: setup_mesh_pack
  USE typhon_API_mod,    ONLY: TYPH_distribute_mesh
#ifdef METIS
  USE setup_metis_mod,   ONLY: setup_metis
#else
  USE setup_mesh_mod,    ONLY: setup_mesh_rcb
#endif
  USE setup_mesh_mod,    ONLY: setup_mesh_nddata

  IMPLICIT NONE

  PUBLIC  :: setup_partition

CONTAINS

  SUBROUTINE setup_partition(config,sizes,timer,dh,error)

    ! Argument list
    TYPE(config_t),                         INTENT(IN)    :: config
    TYPE(sizes_t),                          INTENT(INOUT) :: sizes
    TYPE(timer_t),                          INTENT(INOUT) :: timer
    TYPE(data_t),  DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: dh
    TYPE(error_t),                          INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: icolour,coldata
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: conndata

    ! Timer
    CALL timer_start(timer)

#ifdef METIS
    ! pack mesh data
    CALL setup_mesh_pack(nproc=config%comm%spatial%nproc,                      &
&                        irank=config%comm%spatial%rank,conndata=conndata,     &
&                        error=error)
    IF (error%ierr.NE.SUCCESS) RETURN
    ! set colour using METIS
    CALL setup_metis(SIZE(conndata,2),config%comm%spatial%nproc,               &
&                    config%comm%spatial%rank,config%comm%spatial%comm,        &
&                    conndata,coldata,error)
    IF (error%ierr.NE.SUCCESS) RETURN
#else
    ! set colour using basic RCB
    CALL setup_mesh_rcb(config%comm%spatial%nproc,icolour,error)
    IF (error%ierr.NE.SUCCESS) RETURN
    ! pack mesh data
    CALL setup_mesh_pack(config%comm%spatial%nproc,config%comm%spatial%rank,   &
&                        conndata,icolour,coldata,error)
    IF (error%ierr.NE.SUCCESS) RETURN
#endif
    ! distribute mesh data to partition
    CALL TYPH_distribute_mesh(conndata,coldata,config,sizes,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN
    ! set nodal mesh data
    CALL setup_mesh_nddata(sizes%nnd,dh(indlocglobid)%iaddr,                   &
&                          dh(indtypeid)%iaddr,dh(ndxid)%raddr,dh(ndyid)%raddr,&
&                          error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE setup_partition

END MODULE setup_partition_mod
