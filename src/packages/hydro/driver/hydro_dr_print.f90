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
MODULE hydro_dr_print_mod

  USE dataAPI_types_mod,  ONLY: hydro_t,runtime_t,data_t
  USE dataAPI_kinds_mod,  ONLY: rlk,ink
  USE dataAPI_params_mod, ONLY: NCORN,OSTREAM
  USE dataAPI_id_mod,     ONLY: ielregid,eldensityid,elenergyid,elpressureid,  &
&                               elvolumeid,elmassid,cnwtid,nduid,ndvid,ndxid,  &
&                               ndyid,indtypeid,ielmatid,cnviscxid,cnviscyid,  &
&                               cnuid=>rscratch21id,cnvid=>rscratch22id,       &
&                               ielndid,imxelid,imxfcpid,imxncpid,icpmatid,    &
&                               cpdensityid,cpenergyid,cppressureid,cpvolumeid,&
&                               cpmassid,cpwtid=>rcpscratch21id,               &
&                               cpuid=>rcpscratch22id,cpvid=>rcpscratch23id
  USE hydro_kn_print_mod, ONLY: hydro_kn_init_shortprint,                      &
&                               hydro_kn_calc_shortprint,                      &
&                               hydro_kn_average_shortprint,                   &
&                               hydro_kn_total_shortprint,                     &
&                               hydro_kn_write_header_shortprint,              &
&                               hydro_kn_write_table_shortprint,               &
&                               hydro_kn_write_total_shortprint,               &
&                               hydro_kn_longprint
  USE utils_kn_gather_mod,ONLY: utils_kn_cngather,utils_kn_mxgather
  USE typhon_API_mod,     ONLY: TYPH_reduce,TYPH_OP_SUM,TYPH_OP_MIN,TYPH_OP_MAX

  IMPLICIT NONE

  PUBLIC  :: hydro_dr_shortprint,hydro_dr_longprint
  PRIVATE :: hydro_dr_reduce_shortprint

CONTAINS

  SUBROUTINE hydro_dr_shortprint(hydro,runtime,dh)

    ! Argument list
    TYPE(hydro_t),               INTENT(IN)    :: hydro
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh
    ! Local
    REAL(KIND=rlk)                               :: tot_vol,tot_mass,tot_ie,   &
&                                                   tot_ke,tot_pre,tot_density
    REAL(KIND=rlk),DIMENSION(runtime%sizes%nreg) :: reg_vol,reg_mass,reg_ke,   &
&                                                   reg_ie,reg_dmn,reg_dmx,    &
&                                                   reg_pre,reg_pmn,reg_pmx,   &
&                                                   reg_density
    REAL(KIND=rlk),DIMENSION(runtime%sizes%nmat) :: mat_vol,mat_mass,mat_ke,   &
&                                                   mat_ie,mat_dmn,mat_dmx,    &
&                                                   mat_pre,mat_pmn,mat_pmx,   &
&                                                   mat_density

    ! Gather velocity to elements
    CALL utils_kn_cngather(runtime%sizes%nel,runtime%sizes%nnd,                &
&                          dh(ielndid)%iaddr,dh(nduid)%raddr,dh(cnuid)%raddr)
    CALL utils_kn_cngather(runtime%sizes%nel,runtime%sizes%nnd,                &
&                          dh(ielndid)%iaddr,dh(ndvid)%raddr,dh(cnvid)%raddr)

    ! Calculate table values
    CALL hydro_kn_init_shortprint(runtime%sizes%nreg,reg_vol,reg_mass,reg_ke,  &
&                                 reg_ie,reg_dmn,reg_dmx,reg_pre,reg_pmn,      &
&                                 reg_pmx)
    CALL hydro_kn_calc_shortprint(runtime%sizes%nreg,runtime%sizes%nel,        &
&                                 hydro%global%dencut,dh(ielregid)%iaddr,      &
&                                 dh(eldensityid)%raddr,dh(elenergyid)%raddr,  &
&                                 dh(elpressureid)%raddr,dh(elvolumeid)%raddr, &
&                                 dh(elmassid)%raddr,dh(cnwtid)%raddr,         &
&                                 dh(cnuid)%raddr,dh(cnvid)%raddr,reg_vol,     &
&                                 reg_mass,reg_ke,reg_ie,reg_dmn,reg_dmx,      &
&                                 reg_pre,reg_pmn,reg_pmx)
    CALL hydro_kn_init_shortprint(runtime%sizes%nmat,mat_vol,mat_mass,mat_ke,  &
&                                 mat_ie,mat_dmn,mat_dmx,mat_pre,mat_pmn,      &
&                                 mat_pmx)
    CALL hydro_kn_calc_shortprint(runtime%sizes%nmat,runtime%sizes%nel,        &
&                                 hydro%global%dencut,dh(ielmatid)%iaddr,      &
&                                 dh(eldensityid)%raddr,dh(elenergyid)%raddr,  &
&                                 dh(elpressureid)%raddr,dh(elvolumeid)%raddr, &
&                                 dh(elmassid)%raddr,dh(cnwtid)%raddr,         &
&                                 dh(cnuid)%raddr,dh(cnvid)%raddr,mat_vol,     &
&                                 mat_mass,mat_ke,mat_ie,mat_dmn,mat_dmx,      &
&                                 mat_pre,mat_pmn,mat_pmx)
    IF (runtime%sizes%ncp.GT.0_ink) THEN
      CALL utils_kn_mxgather(runtime%sizes%nel,runtime%sizes%nmx,              &
&                            runtime%sizes%ncp,dh(imxelid)%iaddr,              &
&                            dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,            &
&                            dh(cnwtid)%raddr,dh(cpwtid)%raddr)
      CALL utils_kn_mxgather(runtime%sizes%nel,runtime%sizes%nmx,              &
&                            runtime%sizes%ncp,dh(imxelid)%iaddr,              &
&                            dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,            &
&                            dh(cnuid)%raddr,dh(cpuid)%raddr)
      CALL utils_kn_mxgather(runtime%sizes%nel,runtime%sizes%nmx,              &
&                            runtime%sizes%ncp,dh(imxelid)%iaddr,              &
&                            dh(imxfcpid)%iaddr,dh(imxncpid)%iaddr,            &
&                            dh(cnvid)%raddr,dh(cpvid)%raddr)
      CALL hydro_kn_calc_shortprint(runtime%sizes%nmat,runtime%sizes%ncp,      &
&                                   hydro%global%dencut,dh(icpmatid)%iaddr,    &
&                                   dh(cpdensityid)%raddr,dh(cpenergyid)%raddr,&
&                                   dh(cppressureid)%raddr,                    &
&                                   dh(cpvolumeid)%raddr,dh(cpmassid)%raddr,   &
&                                   dh(cpwtid)%raddr,dh(cpuid)%raddr,          &
&                                   dh(cpvid)%raddr,mat_vol,mat_mass,mat_ke,   &
&                                   mat_ie,mat_dmn,mat_dmx,mat_pre,mat_pmn,    &
&                                   mat_pmx)
    ENDIF

    ! Reduction operation
    IF (hydro%comm%nproc.GT.1_ink) THEN
      CALL hydro_dr_reduce_shortprint(runtime%sizes%nreg,hydro%comm%comm,      &
&                                     reg_vol,reg_mass,reg_ke,reg_ie,reg_dmn,  &
&                                     reg_dmx,reg_pre,reg_pmn,reg_pmx)
      CALL hydro_dr_reduce_shortprint(runtime%sizes%nmat,hydro%comm%comm,      &
&                                     mat_vol,mat_mass,mat_ke,mat_ie,mat_dmn,  &
&                                     mat_dmx,mat_pre,mat_pmn,mat_pmx)
    ENDIF

    ! Calculate averages 
    CALL hydro_kn_average_shortprint(runtime%sizes%nreg,hydro%global%dencut,   &
&                                    reg_vol,reg_mass,reg_density,reg_pre)
    CALL hydro_kn_average_shortprint(runtime%sizes%nmat,hydro%global%dencut,   &
&                                    mat_vol,mat_mass,mat_density,mat_pre)

    ! Calculate totals
    IF (runtime%sizes%nreg.EQ.MIN(runtime%sizes%nreg,runtime%sizes%nmat)) THEN
      CALL hydro_kn_total_shortprint(runtime%sizes%nreg,reg_vol,reg_mass,      &
&                                    reg_ie,reg_ke,reg_pre,tot_vol,tot_mass,   &
&                                    tot_ie,tot_ke,tot_pre,tot_density)
    ELSE
      CALL hydro_kn_total_shortprint(runtime%sizes%nmat,mat_vol,mat_mass,      &
&                                    mat_ie,mat_ke,mat_pre,tot_vol,tot_mass,   &
&                                    tot_ie,tot_ke,tot_pre,tot_density)
    ENDIF

    ! Print values
    IF (hydro%comm%zmproc) THEN
      CALL hydro_kn_write_header_shortprint(OSTREAM,runtime%timestep%nstep,    &
&                                           runtime%timestep%time)
      CALL hydro_kn_write_table_shortprint(runtime%sizes%nreg,OSTREAM,         &
&                                          ' Table 1: Hydro region  ',         &
&                                          '    region',hydro%io%sregion,      &
&                                          reg_vol,reg_mass,reg_ie,reg_ke,     &
&                                          reg_pre,reg_pmn,reg_pmx,reg_density,&
&                                          reg_dmn,reg_dmx)
      CALL hydro_kn_write_table_shortprint(runtime%sizes%nmat,OSTREAM,         &
&                                          ' Table 2: Hydro material',         &
&                                          '  material',hydro%io%smaterial,    &    
&                                          mat_vol,mat_mass,mat_ie,mat_ke,     &
&                                          mat_pre,mat_pmn,mat_pmx,mat_density,&
&                                          mat_dmn,mat_dmx)
      CALL hydro_kn_write_total_shortprint(OSTREAM,tot_vol,tot_mass,tot_ie,    &
&                                          tot_ke,tot_pre,tot_density)
    ENDIF

  END SUBROUTINE hydro_dr_shortprint

  SUBROUTINE hydro_dr_longprint(str,iunit,dh,runtime)

    ! Argument list
    CHARACTER(LEN=*),              INTENT(IN) :: str
    INTEGER(KIND=ink),             INTENT(IN) :: iunit
    TYPE(data_t),     DIMENSION(:),INTENT(IN) :: dh
    TYPE(runtime_t),               INTENT(IN) :: runtime

    ! Write long print
    CALL hydro_kn_longprint(str,iunit,runtime%sizes%nel,runtime%sizes%nnd,     &
&                           dh(eldensityid)%raddr,dh(elpressureid)%raddr,      &
&                           dh(elenergyid)%raddr,dh(elvolumeid)%raddr,         &
&                           dh(elmassid)%raddr,dh(cnviscxid)%raddr,            &
&                           dh(cnviscyid)%raddr,dh(indtypeid)%iaddr,           &
&                           dh(ndxid)%raddr,dh(ndyid)%raddr,dh(nduid)%raddr,   &
&                           dh(ndvid)%raddr)

  END SUBROUTINE hydro_dr_longprint

  SUBROUTINE hydro_dr_reduce_shortprint(nflag,comm,flag_vol,flag_mass,flag_ke, &
&                                       flag_ie,flag_dmn,flag_dmx,flag_pre,    &
&                                       flag_pmn,flag_pmx)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)    :: nflag,comm
    REAL(KIND=rlk),   DIMENSION(nflag),INTENT(INOUT) :: flag_vol,flag_mass,    &
&                                                       flag_ke,flag_ie,       &
&                                                       flag_dmn,flag_dmx,     &
&                                                       flag_pre,flag_pmn,     &
&                                                       flag_pmx
    ! Local
    INTEGER(KIND=ink)                 :: ierr
    REAL(KIND=rlk),   DIMENSION(nflag) :: rval

    ierr=TYPH_reduce(flag_vol,rval,TYPH_OP_SUM,comm)
    flag_vol(1:nflag)=rval(1:nflag)
    ierr=TYPH_reduce(flag_mass,rval,TYPH_OP_SUM,comm)
    flag_mass(1:nflag)=rval(1:nflag)
    ierr=TYPH_reduce(flag_ke,rval,TYPH_OP_SUM,comm)
    flag_ke(1:nflag)=rval(1:nflag)
    ierr=TYPH_reduce(flag_ie,rval,TYPH_OP_SUM,comm)
    flag_ie(1:nflag)=rval(1:nflag)
    ierr=TYPH_reduce(flag_dmn,rval,TYPH_OP_MIN,comm)
    flag_dmn(1:nflag)=rval(1:nflag)
    ierr=TYPH_reduce(flag_dmx,rval,TYPH_OP_MAX,comm)
    flag_dmx(1:nflag)=rval(1:nflag)
    ierr=TYPH_reduce(flag_pre,rval,TYPH_OP_SUM,comm)
    flag_pre(1:nflag)=rval(1:nflag)
    ierr=TYPH_reduce(flag_pmn,rval,TYPH_OP_MIN,comm)
    flag_pmn(1:nflag)=rval(1:nflag)
    ierr=TYPH_reduce(flag_pmx,rval,TYPH_OP_MAX,comm)
    flag_pmx(1:nflag)=rval(1:nflag)

  END SUBROUTINE hydro_dr_reduce_shortprint

END MODULE hydro_dr_print_mod
