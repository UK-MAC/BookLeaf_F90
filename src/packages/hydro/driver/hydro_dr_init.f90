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
MODULE hydro_dr_init_mod

  USE dataAPI_kinds_mod,    ONLY: ink
  USE dataAPI_types_mod,    ONLY: hydro_t,sizes_t,data_t
  USE dataAPI_comm_mod,     ONLY: halfstep,viscosity,keycommcells
  USE dataAPI_id_mod,       ONLY: elviscid,cnviscxid,cnviscyid,eldensityid,    &
&                                 cnxid,cnyid,spmassid,cpviscid,cpviscxid,     &
&                                 cpviscyid,cnmassid,cnwtid,elcs2id
  USE dataAPI_lagstepid_mod,ONLY: cnfxid,cnfyid
  USE dataAPI_dtstepid_mod, ONLY: dxid,dyid,duid,dvid
  USE typhon_API_mod,       ONLY: TYPH_add_phase,TYPH_add_quant_to_phase,      &
&                                 TYPH_GHOSTS_ONE,TYPH_PURE
  USE hydro_kn_init_mod,    ONLY: hydro_kn_viscinit,hydro_kn_spmassinit

  IMPLICIT NONE

  PUBLIC :: hydro_dr_init,hydro_dr_initphases

CONTAINS

  SUBROUTINE hydro_dr_init(hydro,sizes,dh)

    ! Argument list
    TYPE(hydro_t),             INTENT(IN)    :: hydro
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    ! initialise artificial viscosity
    CALL hydro_kn_viscinit(sizes%nel2,dh(elviscid)%raddr,dh(cnviscxid)%raddr,  &
&                          dh(cnviscyid)%raddr)
    IF (sizes%ncp.GT.0_ink) THEN
      CALL hydro_kn_viscinit(sizes%ncp,dh(cpviscid)%raddr,dh(cpviscxid)%raddr, &
&                            dh(cpviscyid)%raddr)
    ENDIF

    ! initialise subzonal pressure mass
    IF (hydro%zsp) THEN
      CALL hydro_kn_spmassinit(sizes%nel,dh(eldensityid)%raddr,dh(cnxid)%raddr,&
&                              dh(cnyid)%raddr,dh(spmassid)%raddr)
    ENDIF

  END SUBROUTINE hydro_dr_init

  SUBROUTINE hydro_dr_initphases(hydro,dh)

    ! Argument list
    TYPE(hydro_t),             INTENT(IN) :: hydro
    TYPE(data_t), DIMENSION(:),INTENT(IN) :: dh
    ! Local
    INTEGER(KIND=ink) :: ierr

    ! Create phase
    ierr=TYPH_add_phase(halfstep,"Half Step",TYPH_GHOSTS_ONE,TYPH_PURE,        &
&                       KeySetID=keycommcells)
    ierr=TYPH_add_phase(viscosity,"Viscosity",TYPH_GHOSTS_ONE,TYPH_PURE,       &
&                       KeySetID=keycommcells)

    ! Attach quantities to phase
    ierr=TYPH_add_quant_to_phase(halfstep,dh(eldensityid)%taddr)
    ierr=TYPH_add_quant_to_phase(halfstep,dh(cnmassid)%taddr)
    ierr=TYPH_add_quant_to_phase(halfstep,dh(cnwtid)%taddr)
    ierr=TYPH_add_quant_to_phase(halfstep,dh(cnfxid)%taddr)
    ierr=TYPH_add_quant_to_phase(halfstep,dh(cnfyid)%taddr)
    IF (hydro%ztq) THEN
    ELSE
      ierr=TYPH_add_quant_to_phase(viscosity,dh(duid)%taddr)
      ierr=TYPH_add_quant_to_phase(viscosity,dh(dvid)%taddr)
      ierr=TYPH_add_quant_to_phase(viscosity,dh(dxid)%taddr)
      ierr=TYPH_add_quant_to_phase(viscosity,dh(dyid)%taddr)
      ierr=TYPH_add_quant_to_phase(viscosity,dh(eldensityid)%taddr)
      ierr=TYPH_add_quant_to_phase(viscosity,dh(elcs2id)%taddr)
    ENDIF

  END SUBROUTINE hydro_dr_initphases

END MODULE hydro_dr_init_mod
