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
MODULE hydro_dr_getacceleration_mod

  USE dataAPI_kinds_mod,           ONLY: ink
  USE dataAPI_types_mod,           ONLY: hydro_t,runtime_t,sizes_t,data_t,     &
&                                        comm_t
  USE dataAPI_id_mod,              ONLY: ielsort1id,ielndid,eldensityid,cnwtid,&
&                                        cnmassid,nduid,ndvid,frvolumeid
  USE dataAPI_comm_mod,            ONLY: HALFSTEP
  USE dataAPI_lagstepid_mod,       ONLY: ndubarid,ndvbarid,cnfxid,cnfyid,cnuid,&
&                                        ndareaid,ndmassid,cpfxid,cpfyid,cnvid
  USE timerAPI_types_mod,          ONLY: timer_t
  USE utils_dr_average_mod,        ONLY: utils_dr_average
  USE utils_dr_bc_mod,             ONLY: utils_dr_bc
  USE utils_dr_gather_mod,         ONLY: utils_dr_cngather
  USE hydro_kn_getacceleration_mod,ONLY: hydro_kn_getacceleration,             &
&                                        hydro_kn_scatteracceleration,         &
&                                        hydro_kn_applyacceleration
  USE typhon_API_mod,              ONLY: TYPH_exchange

  IMPLICIT NONE

  PUBLIC :: hydro_dr_getacceleration,hydro_dr_initacceleration,                &
&           hydro_dr_scatteracceleration,hydro_dr_applyacceleration

CONTAINS

  SUBROUTINE hydro_dr_getacceleration(hydro,sizes,dh)

    ! Argument list
    TYPE(hydro_t),             INTENT(IN)    :: hydro
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    CALL hydro_kn_getacceleration(sizes%nnd,hydro%global%dencut,               &
&                                 hydro%global%zerocut,dh(ndareaid)%raddr,     &
&                                 dh(ndmassid)%raddr,dh(ndubarid)%raddr,       &
&                                 dh(ndvbarid)%raddr)

    ! apply boundary conditions
    CALL utils_dr_bc(hydro%global,sizes,ndubarid,ndvbarid,dh)

  END SUBROUTINE hydro_dr_getacceleration

  SUBROUTINE hydro_dr_initacceleration(comm,sizes,timer,dh)

    ! Argument list
    TYPE(comm_t),              INTENT(IN)    :: comm
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(timer_t),             INTENT(INOUT) :: timer
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    IF (sizes%ncp.GT.0_ink) THEN
      ! average forces
      CALL utils_dr_average(sizes,frvolumeid,cpfxid,cpfyid,cnfxid,cnfyid,dh)
    ENDIF

    IF (comm%nproc.GT.1_ink) THEN
      CALL TYPH_exchange(HALFSTEP,timer%time)
      timer%start=timer%time
    ENDIF

  END SUBROUTINE hydro_dr_initacceleration

  SUBROUTINE hydro_dr_scatteracceleration(hydro,sizes,dh)

    ! Argument list
    TYPE(hydro_t),             INTENT(IN)    :: hydro
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    CALL hydro_kn_scatteracceleration(sizes%nel1,sizes%nnd1,                   &
&                                     hydro%global%zerocut,                    &
&                                     dh(ielsort1id)%iaddr,dh(ielndid)%iaddr,  &
&                                     dh(eldensityid)%raddr,dh(cnwtid)%raddr,  &
&                                     dh(cnmassid)%raddr,dh(ndareaid)%raddr,   &
&                                     dh(ndmassid)%raddr,dh(cnfxid)%raddr,     &
&                                     dh(cnfyid)%raddr,dh(ndubarid)%raddr,     &
&                                     dh(ndvbarid)%raddr)

  END SUBROUTINE hydro_dr_scatteracceleration

  SUBROUTINE hydro_dr_applyacceleration(runtime,dh)

    ! Argument list
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    CALL hydro_kn_applyacceleration(runtime%sizes%nnd,runtime%timestep%dt,     &
&                                   dh(ndubarid)%raddr,dh(ndvbarid)%raddr,     &
&                                   dh(nduid)%raddr,dh(ndvid)%raddr)
    ! gather
    CALL utils_dr_cngather(runtime%sizes,ndubarid,cnuid,dh)
    CALL utils_dr_cngather(runtime%sizes,ndvbarid,cnvid,dh)

  END SUBROUTINE hydro_dr_applyacceleration

END MODULE hydro_dr_getacceleration_mod
