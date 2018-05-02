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
MODULE hydro_dr_getenergy_mod

  USE dataAPI_kinds_mod,     ONLY: ink
  USE dataAPI_types_mod,     ONLY: hydro_t,runtime_t,data_t
  USE dataAPI_id_mod,        ONLY: elenergyid,elmassid,cpenergyid,cpmassid,    &
&                                  imxelid,imxfcpid,imxncpid
  USE dataAPI_lagstepid_mod, ONLY: cnfxid,cnfyid,cnuid,cnvid,cpfxid,cpfyid,    &
&                                  cpuid,cpvid
  USE hydro_kn_getenergy_mod,ONLY: hydro_kn_getenergy
  USE utils_kn_gather_mod,   ONLY: utils_kn_mxgather

  IMPLICIT NONE

  PUBLIC :: hydro_dr_getenergy

CONTAINS

  SUBROUTINE hydro_dr_getenergy(hydro,runtime,dh)

    ! Argument list
    TYPE(hydro_t),               INTENT(IN)    :: hydro
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh

    ! hydro internal energy update
    CALL hydro_kn_getenergy(runtime%sizes%nel,runtime%timestep%dts,            &
&                           hydro%global%zerocut,dh(elenergyid)%raddr,         &
&                           dh(elmassid)%raddr,dh(cnfxid)%raddr,               &
&                           dh(cnfyid)%raddr,dh(cnuid)%raddr,dh(cnvid)%raddr)
    IF (runtime%sizes%ncp.GT.0_ink) THEN
      CALL utils_kn_mxgather(runtime%sizes%nel,runtime%sizes%nmx,              &
&                            runtime%sizes%ncp,dh(imxelid)%iaddr,              &
&                            dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,            &
&                            dh(cnuid)%raddr,dh(cpuid)%raddr)
      CALL utils_kn_mxgather(runtime%sizes%nel,runtime%sizes%nmx,              &
&                            runtime%sizes%ncp,dh(imxelid)%iaddr,              &
&                            dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,            &
&                            dh(cnvid)%raddr,dh(cpvid)%raddr)
      CALL hydro_kn_getenergy(runtime%sizes%ncp,runtime%timestep%dts,          &
&                             hydro%global%zerocut,dh(cpenergyid)%raddr,       &
&                             dh(cpmassid)%raddr,dh(cpfxid)%raddr,             &
&                             dh(cpfyid)%raddr,dh(cpuid)%raddr,dh(cpvid)%raddr)
    ENDIF

  END SUBROUTINE hydro_dr_getenergy

END MODULE hydro_dr_getenergy_mod
