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
MODULE mix_dr_list_mod

  USE dataAPI_kinds_mod,  ONLY: ink,rlk
  USE dataAPI_types_mod,  ONLY: sizes_t,data_t,error_t
  USE dataAPI_params_mod, ONLY: SUCCESS,FAILURE,HALT_SINGLE
  USE dataAPI_id_mod,     ONLY: ielmatid,imxelid,imxncpid,imxfcpid,icpprevid,  &
&                               icpnextid,icpmatid,cpenergyid,cpmassid,        &
&                               cpdensityid,cpvolumeid,cpcs2id,cppressureid,   &
&                               cpcviscid,cpviscxid,cpviscyid,frvolumeid,      &
&                               frmassid,ielmatid,elmassid,elenergyid,         &
&                               iscratch11id,icpscratch11id,icpscratch12id,    &
&                               rcpscratch11id
  USE mix_kn_list_mod,    ONLY: mix_kn_addel,mix_kn_addcp,mix_kn_deleteel,     &
&                               mix_kn_deletecp,mix_kn_mxcopystate,            &
&                               mix_kn_flattenindex,mix_kn_flattenlist,        &
&                               mix_kn_flattenquant
  USE utils_kn_sort_mod,  ONLY: utils_kn_sortwrapper
  USE utils_kn_access_mod,ONLY: utils_kn_get
  USE data_mod,           ONLY: data_reset_mxquant,data_reset_cpquant

  IMPLICIT NONE

  REAL(KIND=rlk),PARAMETER,PRIVATE :: INCR=0.05_rlk

  PUBLIC :: mix_dr_addel,mix_dr_deleteel,mix_dr_addcp,mix_dr_deletecp,         &
&           mix_dr_flatten

CONTAINS

  FUNCTION mix_dr_addel(iel,sizes,dh,error) RESULT(ires)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: iel
    TYPE(sizes_t),                 INTENT(INOUT) :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),                 INTENT(OUT)   :: error
    ! Result
    INTEGER(KIND=ink) :: ires
    ! Local
    INTEGER(KIND=ink) :: nsize

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! increment number of mixed elements
    sizes%nmx=sizes%nmx+1_ink

    ! resize data if required
    IF (sizes%nmx.GT.sizes%mmx) THEN
      nsize=INT(INCR*sizes%nel,KIND=ink)
      CALL data_reset_mxquant(nsize,sizes,dh,error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to resize multi-material connectivity"
        RETURN
      ENDIF
    ENDIF

    ! correct mixed list
    CALL mix_kn_addel(iel,sizes%nel,sizes%nmx,dh(ielmatid)%iaddr,              &
&                     dh(imxelid)%iaddr,dh(imxncpid)%iaddr,dh(imxfcpid)%iaddr)

    ! return mix index
    ires=-sizes%nmx
    IF (ires.GT.0_ink) THEN
      error%ierr=FAILURE
      error%serr="ERROR: failed to add multi-material element"
      RETURN
    ENDIF

  END FUNCTION mix_dr_addel

  SUBROUTINE mix_dr_deleteel(iel,imix,sizes,dh) 

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: iel,imix
    TYPE(sizes_t),                 INTENT(INOUT) :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    ! Local
    INTEGER(KIND=ink) :: icp

    icp=utils_kn_get(imix,sizes%nmx,dh(imxfcpid)%iaddr)
    CALL mix_kn_mxcopystate(iel,icp,sizes%nel,sizes%ncp,dh(icpmatid)%iaddr,    &
&                           dh(cpenergyid)%raddr,dh(cpmassid)%raddr,           &
&                           dh(ielmatid)%iaddr,dh(elenergyid)%raddr,           &
&                           dh(elmassid)%raddr)
    CALL mix_kn_deleteel(imix,sizes%nmx,sizes%ncp,sizes%nel,dh(imxfcpid)%iaddr,&
&                        dh(imxncpid)%iaddr,dh(imxelid)%iaddr,                 &
&                        dh(ielmatid)%iaddr,dh(icpprevid)%iaddr,               &
&                        dh(icpnextid)%iaddr,dh(icpmatid)%iaddr,               &
&                        dh(cpenergyid)%raddr,dh(cpmassid)%raddr,              &
&                        dh(cpdensityid)%raddr,dh(cpvolumeid)%raddr,           &
&                        dh(cpcs2id)%raddr,dh(cppressureid)%raddr,             &
&                        dh(cpcviscid)%raddr,dh(cpviscxid)%raddr,              &
&                        dh(cpviscyid)%raddr,dh(frvolumeid)%raddr,             &
&                        dh(frmassid)%raddr)
    sizes%nmx=sizes%nmx-1_ink

  END SUBROUTINE mix_dr_deleteel

  FUNCTION mix_dr_addcp(imix,sizes,dh,error) RESULT(ires)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: imix
    TYPE(sizes_t),                 INTENT(INOUT) :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),                 INTENT(OUT)   :: error
    ! Result
    INTEGER(KIND=ink) :: ires
    ! Local
    INTEGER(KIND=ink) :: nsize

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! increment number of components
    sizes%ncp=sizes%ncp+1_ink

    ! resize data if required
    IF (sizes%ncp.GT.sizes%mcp) THEN
      nsize=INT(INCR*sizes%nmat*sizes%nel)
      CALL data_reset_cpquant(nsize,sizes,dh,error%ierr)
    ENDIF

    ! add component
    CALL mix_kn_addcp(imix,sizes%nmx,sizes%ncp,dh(imxfcpid)%iaddr,             &
&                     dh(imxncpid)%iaddr,dh(icpprevid)%iaddr,                  &
&                     dh(icpnextid)%iaddr)

    ! return component index
    ires=sizes%ncp
    IF (ires.LE.0_ink) THEN
      error%ierr=FAILURE
      error%serr="ERROR: failed to add component"
      RETURN
    ENDIF

  END FUNCTION mix_dr_addcp

  SUBROUTINE mix_dr_deletecp(imix,icp,sizes,dh)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: imix,icp
    TYPE(sizes_t),                 INTENT(INOUT) :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    ! Local
    INTEGER(KIND=ink) :: ii

    CALL mix_kn_deletecp(icp,imix,sizes%nmx,sizes%ncp,dh(imxfcpid)%iaddr,      &
&                        dh(imxncpid)%iaddr,dh(icpnextid)%iaddr,               &
&                        dh(icpprevid)%iaddr,dh(icpmatid)%iaddr,               &
&                        dh(cpenergyid)%raddr,dh(cpmassid)%raddr,              &
&                        dh(cpdensityid)%raddr,dh(cpvolumeid)%raddr,           &
&                        dh(cpcs2id)%raddr,dh(cppressureid)%raddr,             &
&                        dh(cpcviscid)%raddr,dh(cpviscxid)%raddr,              &
&                        dh(cpviscyid)%raddr,dh(frvolumeid)%raddr,             &
&                        dh(frmassid)%raddr,ii)
    sizes%ncp=sizes%ncp-1_ink

  END SUBROUTINE mix_dr_deletecp

  SUBROUTINE mix_dr_flatten(sizes,dh)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    ! Sort mixed elements by cell list
    CALL utils_kn_sortwrapper(sizes%nmx,dh(imxelid)%iaddr,                     &
&                             dh(iscratch11id)%iaddr)

    ! Set new connectivity
    CALL mix_kn_flattenindex(sizes%nmx,sizes%ncp,dh(iscratch11id)%iaddr,       &
&                            dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,            &
&                            dh(icpmatid)%iaddr,dh(icpnextid)%iaddr,           &
&                            dh(icpprevid)%iaddr,dh(icpscratch11id)%iaddr)
    CALL mix_kn_flattenlist(sizes%nmx,sizes%ncp,dh(iscratch11id)%iaddr,        &
&                           dh(imxfcpid)%iaddr,dh(imxelid)%iaddr,              &
&                           dh(imxncpid)%iaddr,dh(icpprevid)%iaddr,            &
&                           dh(icpnextid)%iaddr)
    CALL mix_kn_flattenquant(sizes%ncp,dh(icpscratch11id)%iaddr,               &
&                            dh(icpscratch12id)%iaddr,dh(icpmatid)%iaddr)
    CALL mix_kn_flattenquant(sizes%ncp,dh(icpscratch11id)%iaddr,               &
&                            dh(rcpscratch11id)%raddr,dh(frvolumeid)%raddr)

  END SUBROUTINE mix_dr_flatten

END MODULE mix_dr_list_mod
