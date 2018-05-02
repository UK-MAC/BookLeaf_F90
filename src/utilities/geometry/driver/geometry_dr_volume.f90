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
MODULE geometry_dr_volume_mod

  USE geometry_kn_volume_mod,ONLY: geometry_kn_getiso,geometry_kn_getvolume,   &
&                                  geometry_kn_checkvolume
  USE utils_kn_gather_mod,   ONLY: utils_kn_cngather,utils_kn_mxgather
  USE utils_kn_math_mod,     ONLY: utils_kn_multiply
  USE dataAPI_types_mod,     ONLY: sizes_t,data_t,error_t
  USE dataAPI_kinds_mod,     ONLY: ink,rlk
  USE dataAPI_id_mod,        ONLY: ielndid,ndxid,cnxid,ndyid,cnyid,a1id,a2id,  &
&                                  a3id,b1id,b2id,b3id,cnwtid,elvolumeid,      &
&                                  imxelid,imxfcpid,imxncpid,cpvolumeid,       &
&                                  frvolumeid,cpa1id,cpa3id,cpb1id,cpb3id
  USE dataAPI_params_mod,    ONLY: ESTREAM,SUCCESS,HALT_SINGLE
  USE timer_advance_mod,     ONLY: timer_start,timer_end
  USE timerAPI_types_mod,    ONLY: timer_t

  IMPLICIT NONE

  PUBLIC  :: geometry_dr_getgeometry

CONTAINS

  SUBROUTINE geometry_dr_getgeometry(sizes,timer,dh,error)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(timer_t),             INTENT(INOUT) :: timer
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),             INTENT(OUT)   :: error

    ! Timer
    CALL timer_start(timer)

    ! Gather position to element
    CALL utils_kn_cngather(sizes%nel,sizes%nnd,dh(ielndid)%iaddr,              &
&                          dh(ndxid)%raddr,dh(cnxid)%raddr)
    CALL utils_kn_cngather(sizes%nel,sizes%nnd,dh(ielndid)%iaddr,              &
&                          dh(ndyid)%raddr,dh(cnyid)%raddr)

    ! Calculate iso-parametric terms
    CALL geometry_kn_getiso(sizes%nel,dh(cnxid)%raddr,dh(cnyid)%raddr,         &
&                           dh(a1id)%raddr,dh(a2id)%raddr,dh(a3id)%raddr,      &
&                           dh(b1id)%raddr,dh(b2id)%raddr,dh(b3id)%raddr,      &
&                           dh(cnwtid)%raddr)
    ! Calculate volume
    CALL geometry_kn_getvolume(sizes%nel,dh(a1id)%raddr,dh(a3id)%raddr,        &
&                              dh(b1id)%raddr,dh(b3id)%raddr,                  &
&                              dh(elvolumeid)%raddr)
    IF (sizes%ncp.GT.0_ink) THEN
      ! gather iso-parametric terms to component
      CALL utils_kn_mxgather(sizes%nel,sizes%nmx,sizes%ncp,dh(imxelid)%iaddr,  &
&                            dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,            &
&                            dh(a1id)%raddr,dh(a3id)%raddr,dh(b1id)%raddr,     &
&                            dh(b3id)%raddr,dh(cpa1id)%raddr,dh(cpa3id)%raddr, &
&                            dh(cpb1id)%raddr,dh(cpb3id)%raddr)
      ! calculate component volume
      CALL geometry_kn_getvolume(sizes%ncp,dh(cpa1id)%raddr,dh(cpa3id)%raddr,  &
&                                dh(cpb1id)%raddr,dh(cpb3id)%raddr,            &
&                                dh(cpvolumeid)%raddr)
      CALL utils_kn_multiply(sizes%ncp,dh(frvolumeid)%raddr,                   &
&                            dh(cpvolumeid)%raddr)
    ENDIF

    ! Check volume is positive
    CALL geometry_kn_checkvolume(sizes%nel,0.0_rlk,dh(elvolumeid)%raddr,       &
&                                error%ierr)

    ! Timing data
    CALL timer_end(timer)

    ! Error handle
    IF (error%ierr.NE.SUCCESS) THEN
      error%iout=HALT_SINGLE
      error%serr="ERROR: negative volume in geometry_dr_getgeometry"
      WRITE(ESTREAM,*) "Negative volume in cell ",-error%ierr
      RETURN
    ENDIF

  END SUBROUTINE geometry_dr_getgeometry

END MODULE geometry_dr_volume_mod
