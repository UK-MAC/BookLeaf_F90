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
MODULE hydro_dr_getdt_mod

  USE dataAPI_kinds_mod,           ONLY: ink
  USE dataAPI_types_mod,        ONLY: hydro_t,sizes_t,error_t,dt_t,data_t
  USE dataAPI_params_mod,       ONLY: SUCCESS,HALT_SINGLE
  USE dataAPI_id_mod,           ONLY: ielregid,eldensityid,elviscid,elcs2id,   &
&                                     cnxid,cnyid,ielndid,nduid,ndvid,a1id,    &
&                                     a3id,b1id,b3id,elvolumeid
  USE dataAPI_dtstepid_mod,     ONLY: cnuid,cnvid,scratchid,ellengthid
  USE timerAPI_types_mod,       ONLY: timer_t
  USE hydro_dr_getviscosity_mod,ONLY: hydro_dr_getviscosity
  USE hydro_dr_getcs2_mod,      ONLY: hydro_dr_getcs2
  USE utils_kn_gather_mod,      ONLY: utils_kn_cngather
  USE hydro_kn_getdt_mod,       ONLY: hydro_kn_getdtcfl,hydro_kn_getdtdiv

  IMPLICIT NONE

  PUBLIC :: hydro_dr_getdt

CONTAINS

  SUBROUTINE hydro_dr_getdt(hydro,sizes,timer,dh,dt,error)

    ! Argument list
    TYPE(hydro_t),             INTENT(IN)    :: hydro
    TYPE(sizes_t),             INTENT(IN)    :: sizes 
    TYPE(timer_t),DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(dt_t),   POINTER,     INTENT(INOUT) :: dt
    TYPE(error_t),             INTENT(OUT)   :: error

    ! Artificial viscosity
    CALL utils_kn_cngather(sizes%nel,sizes%nnd,dh(ielndid)%iaddr,              &
&                          dh(nduid)%raddr,dh(cnuid)%raddr)
    CALL utils_kn_cngather(sizes%nel,sizes%nnd,dh(ielndid)%iaddr,              &
&                          dh(ndvid)%raddr,dh(cnvid)%raddr)
    CALL hydro_dr_getviscosity(hydro,sizes,timer,dh)

    ! CFL
    ALLOCATE(dt%next)
    dt=>dt%next
    CALL hydro_dr_getcs2(sizes,dh)
    CALL hydro_kn_getdtcfl(sizes%nreg,sizes%nel,hydro%global%zcut,hydro%cfl_sf,&
&                          hydro%zdtnotreg,hydro%zmidlength,dh(ielregid)%iaddr,&
&                          dh(elcs2id)%raddr,dh(cnxid)%raddr,dh(cnyid)%raddr,  &
&                          dh(scratchid)%raddr,dh(ellengthid)%raddr,dt%rdt,    &
&                          dt%idt,dt%sdt,error%ierr)
    IF (error%ierr.NE.SUCCESS) THEN
      error%serr="ERROR: negative CFL condition in hydro_dr_getdt"
      error%iout=HALT_SINGLE
      RETURN
    ENDIF

    ! Divergence
    ALLOCATE(dt%next)
    dt=>dt%next
    CALL hydro_kn_getdtdiv(sizes%nel,hydro%div_sf,dh(a1id)%raddr,              &
&                          dh(a3id)%raddr,dh(b1id)%raddr,dh(b3id)%raddr,       &
&                          dh(elvolumeid)%raddr,dh(cnuid)%raddr,               &
&                          dh(cnvid)%raddr,dt%rdt,dt%idt,dt%sdt)

  END SUBROUTINE hydro_dr_getdt

END MODULE hydro_dr_getdt_mod
