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
MODULE ale_dr_utils_mod

  USE dataAPI_kinds_mod,    ONLY: ink,lok
  USE dataAPI_types_mod,    ONLY: ale_t,timestep_t,data_t
  USE dataAPI_comm_mod,     ONLY: advel,advnd,keycommcells
  USE dataAPI_id_mod,       ONLY: cnmassid,cnwtid,eldensityid,elenergyid,      &
&                                 elvolumeid
  USE dataAPI_alestepid_mod,ONLY: cnuid,cnvid,fcdvid,fcdmid,store1id
  USE typhon_API_mod,       ONLY: TYPH_add_phase,TYPH_add_quant_to_phase,      &
&                                 TYPH_PURE,TYPH_GHOSTS_TWO

  IMPLICIT NONE

  PUBLIC :: ale_active,ale_dr_initphases

CONTAINS

  PURE LOGICAL(KIND=lok) FUNCTION ale_active(ale,timestep)

    ! Argument list
    TYPE(ale_t),     INTENT(IN) :: ale
    TYPE(timestep_t),INTENT(IN) :: timestep

    ! Check ALE requested
    IF (.NOT.ale%zexist) THEN
      ale_active=.FALSE._lok
      RETURN
    ENDIF

    ! Check ALE currently on
    ale_active=(timestep%time.GE.ale%mintime).AND.                             &
&              (timestep%time.LE.ale%maxtime)

  END FUNCTION ale_active

  SUBROUTINE ale_dr_initphases(ale,dh)

    ! Argument list
    TYPE(ale_t),              INTENT(IN) :: ale
    TYPE(data_t),DIMENSION(:),INTENT(IN) :: dh
    ! Local
    INTEGER(KIND=ink) :: ierr

    ! Check ALE is required
    IF (.NOT.ale%zexist) RETURN

    ! Create phases
    ierr=TYPH_add_phase(advel,"Element advection",TYPH_GHOSTS_TWO,TYPH_PURE,   &
&                       KeySetID=keycommcells)
    ierr=TYPH_add_phase(advnd,"Nodal advection",TYPH_GHOSTS_TWO,TYPH_PURE,     &
&                       KeySetID=keycommcells)

    ! Attach quantities to phases
    ierr=TYPH_add_quant_to_phase(advel,dh(fcdvid)%taddr)
    ierr=TYPH_add_quant_to_phase(advel,dh(eldensityid)%taddr)
    ierr=TYPH_add_quant_to_phase(advel,dh(elenergyid)%taddr)
    ierr=TYPH_add_quant_to_phase(advel,dh(cnmassid)%taddr)
    ierr=TYPH_add_quant_to_phase(advel,dh(cnwtid)%taddr)
    ierr=TYPH_add_quant_to_phase(advnd,dh(fcdvid)%taddr)
    ierr=TYPH_add_quant_to_phase(advnd,dh(fcdmid)%taddr)
    ierr=TYPH_add_quant_to_phase(advnd,dh(cnuid)%taddr)
    ierr=TYPH_add_quant_to_phase(advnd,dh(cnvid)%taddr)
    ierr=TYPH_add_quant_to_phase(advnd,dh(store1id)%taddr)
    ierr=TYPH_add_quant_to_phase(advnd,dh(elvolumeid)%taddr)
    ierr=TYPH_add_quant_to_phase(advnd,dh(cnmassid)%taddr)

  END SUBROUTINE ale_dr_initphases

END MODULE ale_dr_utils_mod
