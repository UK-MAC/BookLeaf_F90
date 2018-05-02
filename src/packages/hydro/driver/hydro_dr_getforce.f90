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
MODULE hydro_dr_getforce_mod

  USE hydro_kn_getforce_mod,ONLY: hydro_kn_getforcep,hydro_kn_getforceq,       &
&                                 hydro_kn_getforcesp,hydro_kn_getforcehg
  USE dataAPI_kinds_mod,    ONLY: ink
  USE dataAPI_types_mod,    ONLY: hydro_t,runtime_t,data_t
  USE dataAPI_id_mod,       ONLY: elpressureid,eldensityid,elcs2id,elvolumeid, &
&                                 a1id,a3id,b1id,b3id,cnviscxid,cnviscyid,     &
&                                 cnxid,cnyid,ielregid,spmassid,cppressureid,  &
&                                 cpviscxid,cpviscyid,cpa1id,cpa3id,cpb1id,    &
&                                 cpb3id
  USE dataAPI_lagstepid_mod,ONLY: cnfxid,cnfyid,cnuid,cnvid,cpfxid,cpfyid
  USE timerAPI_types_mod,   ONLY: timer_t
  USE timerAPI_id_mod,      ONLY: TGETHGID,TGETSPID
  USE timer_advance_mod,    ONLY: timer_start,timer_end

  IMPLICIT NONE

  PUBLIC :: hydro_dr_getforce

CONTAINS

  SUBROUTINE hydro_dr_getforce(hydro,runtime,timer,dh)

    ! Argument list
    TYPE(hydro_t),               INTENT(IN)    :: hydro
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(timer_t),  DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    ! Pressure force
    CALL hydro_kn_getforcep(runtime%sizes%nel,dh(elpressureid)%raddr,          &
&                           dh(a1id)%raddr,dh(a3id)%raddr,dh(b1id)%raddr,      &
&                           dh(b3id)%raddr,dh(cnfxid)%raddr,dh(cnfyid)%raddr)
    IF (runtime%sizes%ncp.GT.0_ink) THEN
      CALL hydro_kn_getforcep(runtime%sizes%ncp,dh(cppressureid)%raddr,        &
&                             dh(cpa1id)%raddr,dh(cpa3id)%raddr,               &
&                             dh(cpb1id)%raddr,dh(cpb3id)%raddr,               &
&                             dh(cpfxid)%raddr,dh(cpfyid)%raddr)
    ENDIF

    ! Artificial viscosity force
    CALL hydro_kn_getforceq(runtime%sizes%nel,dh(cnviscxid)%raddr,             &
&                           dh(cnviscyid)%raddr,dh(cnfxid)%raddr,              &
&                           dh(cnfyid)%raddr)
    IF (runtime%sizes%ncp.GT.0_ink) THEN
      CALL hydro_kn_getforceq(runtime%sizes%ncp,dh(cpviscxid)%raddr,           &
&                             dh(cpviscyid)%raddr,dh(cpfxid)%raddr,            &
&                             dh(cpfyid)%raddr)
    ENDIF

    ! Subzonal pressure force
    IF (hydro%zsp) THEN
      CALL timer_start(timer(TGETSPID))
      CALL hydro_kn_getforcesp(runtime%sizes%nel,hydro%pmeritreg,              &
&                              dh(ielregid)%iaddr,dh(eldensityid)%raddr,       &
&                              dh(elcs2id)%raddr,dh(cnxid)%raddr,              &
&                              dh(cnyid)%raddr,dh(spmassid)%raddr,             &
&                              dh(cnfxid)%raddr,dh(cnfyid)%raddr)
      CALL timer_end(timer(TGETSPID))
    ENDIF

    ! Anti-hourglass force
    IF (runtime%timestep%zcorrector.AND.hydro%zhg) THEN
      CALL timer_start(timer(TGETHGID))
      CALL hydro_kn_getforcehg(runtime%sizes%nel,runtime%timestep%dt,          &
&                              hydro%kappareg,dh(ielregid)%iaddr,              &
&                              dh(eldensityid)%raddr,dh(elvolumeid)%raddr,     &
&                              dh(cnuid)%raddr,dh(cnvid)%raddr,                &
&                              dh(cnfxid)%raddr,dh(cnfyid)%raddr)
      CALL timer_end(timer(TGETHGID))
    ENDIF

  END SUBROUTINE hydro_dr_getforce

END MODULE hydro_dr_getforce_mod  
