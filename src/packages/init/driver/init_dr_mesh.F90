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
MODULE init_dr_mesh_mod

  USE dataAPI_kinds_mod,  ONLY: ink
  USE dataAPI_types_mod,  ONLY: config_t,runtime_t,data_t,error_t
  USE dataAPI_params_mod, ONLY: SUCCESS
  USE dataAPI_id_mod,     ONLY: indtypeid,ndxid,ndyid,nduid,ndvid,ielregid,    &
&                               ielmatid,ielndid,ielelid,ielfcid
  USE timerAPI_types_mod, ONLY: timer_t
  USE timerAPI_id_mod,    ONLY: TMESHPARTITIONID,TMESHGENID
  USE data_mod,           ONLY: data_set,data_setquant
  USE init_kn_mod,        ONLY: init_kn_serialghosts,init_kn_connectivity,     &
&                               init_kn_nodetype    
#ifdef LSETUP
  USE setup_partition_mod,ONLY: setup_partition
  USE setup_mesh_mod,     ONLY: setup_mesh_transfer
#endif

  IMPLICIT NONE

  PUBLIC  :: init_dr_mesh
 
CONTAINS

  SUBROUTINE init_dr_mesh(config,runtime,timer,dh,error)

    ! Argument list
    TYPE(config_t),                          INTENT(IN)    :: config
    TYPE(runtime_t),                         INTENT(INOUT) :: runtime
    TYPE(timer_t),  DIMENSION(:),            INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: dh
    TYPE(error_t),                           INTENT(OUT)   :: error

    ! transfer data from mesh generation
    IF (config%comm%spatial%nproc.GT.1_ink) THEN
#ifdef LSETUP
      ! partition and transfer mesh
      CALL setup_partition(config,runtime%sizes,timer(TMESHPARTITIONID),dh,    &
&                          error)
      IF (error%ierr.NE.SUCCESS) RETURN
#else
      ! read header
      ! read data
#endif
      ! register data
      CALL data_setquant(config,runtime%sizes,dh,error)
      IF (error%ierr.NE.SUCCESS) RETURN
    ELSE
#ifdef LSETUP
      ! set null ghost extents
      CALL init_kn_serialghosts(runtime%sizes%nel,runtime%sizes%nel1,          &
&                               runtime%sizes%nel2,runtime%sizes%nnd,          &
&                               runtime%sizes%nnd1,runtime%sizes%nnd2)
#else
      ! read header
#endif
      ! register data
      CALL data_set(config,runtime%sizes,dh,error)
      IF (error%ierr.NE.SUCCESS) RETURN
#ifdef LSETUP
      ! transfer mesh
      CALL setup_mesh_transfer(timer(TMESHGENID),runtime%sizes%nel,            &
&                              runtime%sizes%nnd,dh(indtypeid)%iaddr,          &
&                              dh(ndxid)%raddr,dh(ndyid)%raddr,                &
&                              dh(ielregid)%iaddr,dh(ielmatid)%iaddr,          &
&                              dh(ielndid)%iaddr,error)
      IF (error%ierr.NE.SUCCESS) RETURN
#else
      ! read data
#endif
    ENDIF

    ! initialise connectivity
    CALL init_kn_connectivity(runtime%sizes%nel2,dh(ielndid)%iaddr,            &
&                             dh(ielelid)%iaddr,dh(ielfcid)%iaddr)

    ! initialise node type
    CALL init_kn_nodetype(runtime%sizes%nel1,runtime%sizes%nnd1,               &
&                         dh(ielndid)%iaddr,dh(indtypeid)%iaddr)

  END SUBROUTINE init_dr_mesh

END MODULE init_dr_mesh_mod
