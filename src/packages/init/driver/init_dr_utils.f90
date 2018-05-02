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
MODULE init_dr_utils_mod

  USE dataAPI_kinds_mod,     ONLY: ink
  USE dataAPI_types_mod,     ONLY: error_t,config_t,sizes_t,data_t
  USE dataAPI_params_mod,    ONLY: SUCCESS
  USE dataAPI_id_mod,        ONLY: iellocglobid,ielsort1id,ielsort2id,ielmatid,&
&                                  eldensityid,elenergyid,elvolumeid,cnwtid,   &
&                                  elmassid,cnmassid,cpdensityid,cpvolumeid,   &
&                                  cpmassid,imxelid,imxfcpid,imxncpid,frmassid,&
&                                  rcpscratch11id
  USE timerAPI_types_mod,    ONLY: timer_t
  USE eos_dr_geteos_mod,     ONLY: eos_dr_geteos
  USE eos_cf_mod,            ONLY: eos_cf_defaults
  USE io_cf_mod,             ONLY: io_cf_defaults
  USE global_cf_mod,         ONLY: global_cf_defaults
  USE utils_kn_sort_mod,     ONLY: utils_kn_sortwrapper
  USE utils_kn_sort_mod,     ONLY: utils_kn_arthwrapper
  USE utils_kn_gather_mod,   ONLY: utils_kn_mxgather
  USE utils_kn_math_mod,     ONLY: utils_kn_divide
  USE init_kn_mod,           ONLY: init_kn_elmass,init_kn_mxmass

  IMPLICIT NONE

  PRIVATE :: init_dr_mass
  PUBLIC  :: init_dr_utils,init_dr_utilsdefaults,init_dr_state
 
CONTAINS

  SUBROUTINE init_dr_utils(config,sizes,dh,error)

    ! Argument list
    TYPE(config_t),             INTENT(IN)    :: config
    TYPE(sizes_t),              INTENT(IN)    :: sizes
    TYPE(data_t),  DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),              INTENT(OUT)   :: error

    ! initialise mis-direction
    IF (config%comm%spatial%nproc.GT.1_ink) THEN
      CALL utils_kn_sortwrapper(sizes%nel1,dh(iellocglobid)%iaddr,dh(ielsort1id)%iaddr)
      IF (error%ierr.NE.SUCCESS) RETURN
      IF (config%ale%zexist) THEN
        CALL utils_kn_sortwrapper(sizes%nel2,dh(iellocglobid)%iaddr,dh(ielsort2id)%iaddr)
        IF (error%ierr.NE.SUCCESS) RETURN
      ENDIF
    ELSE
      CALL utils_kn_arthwrapper(sizes%nel1,1_ink,1_ink,dh(ielsort1id)%iaddr)
      IF (config%ale%zexist) THEN
        CALL utils_kn_arthwrapper(sizes%nel2,1_ink,1_ink,dh(ielsort2id)%iaddr)
      ENDIF
    ENDIF

  END SUBROUTINE init_dr_utils

  SUBROUTINE init_dr_utilsdefaults(config,error)

    ! Argument list
    TYPE(config_t),INTENT(INOUT) :: config
    TYPE(error_t), INTENT(OUT)   :: error

    ! set Global defaults
    ALLOCATE(config%global)
    CALL global_cf_defaults(config%global)

    ! set EoS defaults
    ALLOCATE(config%eos)
    CALL eos_cf_defaults(config%eos,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! set IO defaults
    ALLOCATE(config%io)
    CALL io_cf_defaults(config%io,error)
    IF (error%ierr.NE.SUCCESS) RETURN

  END SUBROUTINE init_dr_utilsdefaults

  SUBROUTINE init_dr_state(config,sizes,timer,dh)

    ! Argument list
    TYPE(config_t),             INTENT(IN)    :: config
    TYPE(sizes_t),              INTENT(IN)    :: sizes
    TYPE(timer_t),              INTENT(INOUT) :: timer
    TYPE(data_t),  DIMENSION(:),INTENT(INOUT) :: dh

    ! initialise mass
    CALL init_dr_mass(sizes,dh)

    ! initialise pressure and sound speed
    CALL eos_dr_geteos(config%eos,sizes,timer,dh)

  END SUBROUTINE init_dr_state

  SUBROUTINE init_dr_mass(sizes,dh)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh

    ! initialise clean cells
    CALL init_kn_elmass(sizes%nel,dh(eldensityid)%raddr,dh(elvolumeid)%raddr,  &
&                       dh(cnwtid)%raddr,dh(elmassid)%raddr,dh(cnmassid)%raddr)    

    ! initialise mixed cells
    IF (sizes%ncp.GT.0_ink) THEN
      CALL init_kn_mxmass(sizes%ncp,dh(cpdensityid)%raddr,dh(cpvolumeid)%raddr,&
&                         dh(cpmassid)%raddr)
      CALL utils_kn_mxgather(sizes%nel,sizes%nmx,sizes%ncp,dh(imxelid)%iaddr,  &
&                            dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,            &
&                            dh(elmassid)%raddr,dh(rcpscratch11id)%raddr)
      CALL utils_kn_divide(sizes%ncp,dh(cpmassid)%raddr,                       &
&                          dh(rcpscratch11id)%raddr,dh(frmassid)%raddr)
    ENDIF

  END SUBROUTINE init_dr_mass

END MODULE init_dr_utils_mod
