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
MODULE check_dr_tests_mod

  USE dataAPI_kinds_mod,  ONLY: ink,rlk
  USE dataAPI_types_mod,  ONLY: config_t,runtime_t,data_t
  USE dataAPI_id_mod,     ONLY: elvolumeid,eldensityid,elenergyid,ndxid,ndyid, &
&                               ielndid,cnxid,cnyid
  USE typhon_API_mod,     ONLY: TYPH_Reduce,TYPH_OP_SUM
  USE check_kn_tests_mod, ONLY: check_kn_sod
  USE utils_kn_gather_mod,ONLY: utils_kn_cngather

  IMPLICIT NONE

  PUBLIC :: check_dr_sod
 
CONTAINS

  SUBROUTINE check_dr_sod(config,runtime,dh,norm)

    ! Argument list
    TYPE(config_t),              INTENT(IN)    :: config
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh
    REAL(KIND=rlk),              INTENT(OUT)   :: norm
    ! Local
    INTEGER(KIND=ink)           :: ii
    REAL(KIND=rlk)              :: basis,btotal
    REAL(KIND=rlk),DIMENSION(2) :: l1,ltotal

    ! initialise
    norm=0.0_rlk

    ! gather co-ordinates
    CALL utils_kn_cngather(runtime%sizes%nel,runtime%sizes%nnd,                &
&                          dh(ielndid)%iaddr,dh(ndxid)%raddr,dh(cnxid)%raddr)
    CALL utils_kn_cngather(runtime%sizes%nel,runtime%sizes%nnd,                &
&                          dh(ielndid)%iaddr,dh(ndyid)%raddr,dh(cnyid)%raddr)

    ! calculate L1 norm components for density and energy
    CALL check_kn_sod(runtime%sizes%nel,runtime%timestep%time,                 &
&                     dh(elvolumeid)%raddr,dh(eldensityid)%raddr,              &
&                     dh(elenergyid)%raddr,dh(cnxid)%raddr,dh(cnyid)%raddr,    &
&                     basis,l1)
    IF (config%hydro%comm%nproc.GT.1_ink) THEN
      ii=TYPH_Reduce(basis,btotal,TYPH_OP_SUM,config%hydro%comm%comm)
      ii=TYPH_Reduce(l1,ltotal,TYPH_OP_SUM,config%hydro%comm%comm)
      basis=btotal
      l1(:)=ltotal(:)
    ENDIF

    ! use L1 norm of density (energy also calculated)
    l1(:)=l1(:)/basis
    norm=l1(1)

  END SUBROUTINE check_dr_sod

END MODULE check_dr_tests_mod
