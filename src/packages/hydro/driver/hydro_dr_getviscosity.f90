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
MODULE hydro_dr_getviscosity_mod

  USE hydro_kn_edgeviscosity_mod,ONLY: hydro_kn_initeviscosity,                &
&                                      hydro_kn_geteviscosity,                 &
&                                      hydro_kn_limiteviscosity
  USE dataAPI_kinds_mod,         ONLY: ink
  USE dataAPI_types_mod,         ONLY: hydro_t,sizes_t,data_t
  USE dataAPI_id_mod,            ONLY: cnviscxid,cnviscyid,elviscid,elcs2id,   &
&                                      eldensityid,cnxid,cnyid,ielelid,ielndid,&
&                                      ielfcid,indtypeid
  USE dataAPI_comm_mod,          ONLY: VISCOSITY
  USE dataAPI_dtstepid_mod,      ONLY: cnuid,cnvid,dxid,dyid,duid,dvid,storeid
  USE timerAPI_types_mod,        ONLY: timer_t
  USE timerAPI_id_mod,           ONLY: TCOMMTID,TGETVISCOSITYID
  USE timer_advance_mod,         ONLY: timer_start,timer_end
  USE typhon_API_mod,            ONLY: TYPH_exchange

  IMPLICIT NONE

  PUBLIC :: hydro_dr_getviscosity

CONTAINS

  SUBROUTINE hydro_dr_getviscosity(hydro,sizes,timer,dh)

    ! Argument list
    TYPE(hydro_t),             INTENT(IN)    :: hydro
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(timer_t),DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    ! Timer
    CALL timer_start(timer(TGETVISCOSITYID))

    !# Missing code here that can't be merged

    IF (hydro%ztq) THEN
    ELSE
      ! initialisation
      CALL hydro_kn_initeviscosity(sizes%nel,dh(cnxid)%raddr,dh(cnyid)%raddr,  &
&                                  dh(cnuid)%raddr,dh(cnvid)%raddr,            &
&                                  dh(elviscid)%raddr,dh(dxid)%raddr,          &
&                                  dh(dyid)%raddr,dh(duid)%raddr,              &
&                                  dh(dvid)%raddr,dh(cnviscxid)%raddr,         &
&                                  dh(cnviscyid)%raddr)

      ! MPI comm
      IF (hydro%comm%nproc.GT.1_ink) THEN
        CALL TYPH_exchange(VISCOSITY,timer(TCOMMTID)%time)
      ENDIF

      ! Christiensen monotonic limit
      CALL hydro_kn_limiteviscosity(sizes%nel,sizes%nel1,sizes%nnd1,           &
&                                   hydro%global%zerocut,hydro%cvisc1,         &
&                                   hydro%cvisc2,dh(indtypeid)%iaddr,          &
&                                   dh(ielelid)%iaddr,dh(ielndid)%iaddr,       &
&                                   dh(ielfcid)%iaddr,dh(eldensityid)%raddr,   &
&                                   dh(elcs2id)%raddr,dh(duid)%raddr,          &
&                                   dh(dvid)%raddr,dh(dxid)%raddr,             &
&                                   dh(dyid)%raddr,dh(storeid)%raddr,          &
&                                   dh(cnviscxid)%raddr,dh(cnviscyid)%raddr,   &
&                                   dh(elviscid)%raddr)

      ! Final Q calculation
      CALL hydro_kn_geteviscosity(sizes%nel,hydro%global%zerocut,              &
&                                 dh(cnxid)%raddr,dh(cnyid)%raddr,             &
&                                 dh(cnuid)%raddr,dh(cnvid)%raddr,             &
&                                 dh(cnviscxid)%raddr,dh(cnviscyid)%raddr)
    ENDIF

    ! Timing data
    CALL timer_end(timer(TGETVISCOSITYID))

  END SUBROUTINE hydro_dr_getviscosity

END MODULE hydro_dr_getviscosity_mod
