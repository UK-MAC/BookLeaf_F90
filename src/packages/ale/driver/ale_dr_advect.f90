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
MODULE ale_dr_advect_mod

  USE dataAPI_types_mod,    ONLY: ale_t,runtime_t,sizes_t,error_t,data_t
  USE dataAPI_kinds_mod,    ONLY: ink
  USE dataAPI_id_mod,       ONLY: ielelid,ielfcid,cnwtid,cnmassid,eldensityid, &
&                                 elenergyid,elmassid,elvolumeid,ielndid,nduid,&
&                                 ndvid,indtypeid,ielsort2id
  USE dataAPI_params_mod,   ONLY: SUCCESS,FAILURE,HALT_ALL
  USE dataAPI_comm_mod,     ONLY: ADVEL,ADVND
  USE dataAPI_alestepid_mod,ONLY: fcdvid,fcdmid,rwork1id,rwork2id,rwork3id,    &
&                                 fluxid,store1id,store2id,store3id,store4id,  &
&                                 store5id,store6id,indstatusid,zactiveid,     &
&                                 cnuid,cnvid
  USE timerAPI_types_mod,   ONLY: timer_t
  USE timerAPI_id_mod,      ONLY: TCOMMAID,TALEADVECTID,TALEADVECTELID,        &
&                                 TALEADVECTNDID,TALEADVECTBASISELID,          &
&                                 TALEADVECTBASISNDID,TALEADVECTVARELID,       &
&                                 TALEADVECTVARNDID
  USE timer_advance_mod,    ONLY: timer_start,timer_end
  USE ale_kn_advectors_mod, ONLY: ale_kn_sumflux,ale_kn_fluxelvl,              &
&                                 ale_kn_updateel,ale_kn_fluxndvl,             &
&                                 ale_kn_updatend
  USE ale_kn_advect_mod,    ONLY: ale_kn_updatebasisel,ale_kn_initbasisnd,     &
&                                 ale_kn_calcbasisnd,ale_kn_fluxbasisnd,       &
&                                 ale_kn_massbasisnd,ale_kn_cutbasisnd,        &
&                                 ale_kn_activend
  USE utils_kn_gather_mod,  ONLY: utils_kn_cngather
  USE utils_kn_copy_mod,    ONLY: utils_kn_copy
  USE typhon_API_mod,       ONLY: TYPH_exchange

  IMPLICIT NONE

  PRIVATE :: ale_dr_advectel,ale_dr_advectnd,ale_dr_advectbasisel,             &
&            ale_dr_advectvarel,ale_dr_advectbasisnd,ale_dr_advectvarnd
  PUBLIC  :: ale_dr_advect

CONTAINS

  SUBROUTINE ale_dr_advect(ale,runtime,timer,dh,error)

    ! Argument list
    TYPE(ale_t),                 INTENT(IN)    :: ale
    TYPE(runtime_t),             INTENT(IN)    :: runtime
    TYPE(timer_t),  DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),   DIMENSION(:),INTENT(INOUT) :: dh    
    TYPE(error_t),               INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink) :: ii,i1,i2,i3

    ! Timer
    CALL timer_start(timer(TALEADVECTID))

    ! Initialise
    error%ierr=SUCCESS

    ! Advect
    SELECT CASE(ale%adv_type)
      CASE(1_ink) ! Isotropic advection
        ! Advect element variables
        CALL ale_dr_advectel(1_ink,2_ink,ale,runtime%sizes,dh,timer)
        ! Advect nodal variables
        CALL ale_dr_advectnd(1_ink,2_ink,ale,runtime%sizes,dh,timer)
      CASE(2_ink) ! Split advection
        ii=MOD(runtime%timestep%nstep+1,2)
        i1=1_ink+ii
        i2=2_ink-ii
        i3=i2-i1
        DO ii=i1,i2,i3
          ! Advect element variables
          CALL ale_dr_advectel(ii,ii,ale,runtime%sizes,dh,timer)
          ! Advect nodal variables
          CALL ale_dr_advectnd(ii,ii,ale,runtime%sizes,dh,timer)
        ENDDO
      CASE DEFAULT
      error%ierr=FAILURE
      error%iout=HALT_ALL
      error%serr="ERROR: unrecognised adv_type"
    END SELECT
  
    ! Timing data
    CALL timer_end(timer(TALEADVECTID))

  END SUBROUTINE ale_dr_advect

  SUBROUTINE ale_dr_advectel(id1,id2,ale,sizes,dh,timer)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: id1,id2
    TYPE(ale_t),                   INTENT(IN)    :: ale
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(timer_t),    DIMENSION(:),INTENT(INOUT) :: timer

    ! Timer
    CALL timer_start(timer(TALEADVECTELID))

    ! MPI comm
    IF (ale%comm%nproc.GT.1_ink) THEN
      CALL TYPH_exchange(ADVEL,timer(TCOMMAID)%time)
    ENDIF

    ! Advect element basis variables
    CALL ale_dr_advectbasisel(id1,id2,ale,sizes,dh,timer(TALEADVECTBASISELID))
    ! Advect element independent variables
    CALL ale_dr_advectvarel(id1,id2,sizes,dh,timer(TALEADVECTVARELID))

    ! Timing data
    CALL timer_end(timer(TALEADVECTELID))

  END SUBROUTINE ale_dr_advectel

  SUBROUTINE ale_dr_advectnd(id1,id2,ale,sizes,dh,timer)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: id1,id2
    TYPE(ale_t),                   INTENT(IN)    :: ale
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(timer_t),    DIMENSION(:),INTENT(INOUT) :: timer

    ! Timer
    CALL timer_start(timer(TALEADVECTNDID))

    ! Gather
    CALL utils_kn_cngather(sizes%nel,sizes%nnd,dh(ielndid)%iaddr,              &
&                          dh(nduid)%raddr,dh(cnuid)%raddr)
    CALL utils_kn_cngather(sizes%nel,sizes%nnd,dh(ielndid)%iaddr,              &
&                          dh(ndvid)%raddr,dh(cnvid)%raddr)

    ! MPI comm
    IF (ale%comm%nproc.GT.1_ink) THEN
      CALL TYPH_exchange(ADVND,timer(TCOMMAID)%time)
    ENDIF

    ! Advect nodal basis variables
    CALL ale_dr_advectbasisnd(id1,id2,ale,sizes,dh,timer(TALEADVECTBASISNDID))
    ! Advect nodal independent variables
    CALL ale_dr_advectvarnd(sizes,dh,timer(TALEADVECTVARNDID))

    ! Timing data
    CALL timer_end(timer(TALEADVECTNDID))

  END SUBROUTINE ale_dr_advectnd

  SUBROUTINE ale_dr_advectbasisel(id1,id2,ale,sizes,dh,timer)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: id1,id2
    TYPE(ale_t),                   INTENT(IN)    :: ale
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(timer_t),                 INTENT(INOUT) :: timer
  
    ! Timer
    CALL timer_start(timer)

    ! calculate total volume flux
    CALL ale_kn_sumflux(id1,id2,sizes%nel,sizes%nel1,dh(ielelid)%iaddr,        &
&                       dh(ielfcid)%iaddr,dh(fcdvid)%raddr,dh(rwork1id)%raddr)

    ! construct mass flux
    CALL ale_kn_fluxelvl(id1,id2,sizes%nel1,sizes%nel2,dh(ielelid)%iaddr,      &
&                        dh(ielfcid)%iaddr,dh(cnwtid)%raddr,dh(fcdvid)%raddr,  &
&                        dh(eldensityid)%raddr,dh(fcdmid)%raddr)

    ! calculate total mass flux
    CALL ale_kn_sumflux(id1,id2,sizes%nel,sizes%nel1,dh(ielelid)%iaddr,        &
&                       dh(ielfcid)%iaddr,dh(fcdmid)%raddr,dh(rwork2id)%raddr)

    ! update
    CALL ale_kn_updatebasisel(sizes%nel,ale%global%zerocut,ale%global%dencut,  &
&                             dh(rwork1id)%raddr,dh(rwork2id)%raddr,           &
&                             dh(store5id)%raddr,dh(store6id)%raddr,           &
&                             dh(store1id)%raddr,dh(store2id)%raddr,           &
&                             dh(store3id)%raddr,dh(elvolumeid)%raddr,         &
&                             dh(elmassid)%raddr,dh(eldensityid)%raddr)

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE ale_dr_advectbasisel

  SUBROUTINE ale_dr_advectvarel(id1,id2,sizes,dh,timer)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: id1,id2
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(timer_t),                 INTENT(INOUT) :: timer

    ! Timer
    CALL timer_start(timer)

    ! internal energy (mass weighted)
    CALL ale_kn_fluxelvl(id1,id2,sizes%nel1,sizes%nel2,dh(ielelid)%iaddr,      &
&                        dh(ielfcid)%iaddr,dh(cnmassid)%raddr,dh(fcdmid)%raddr,&
&                        dh(elenergyid)%raddr,dh(fluxid)%raddr)
    CALL ale_kn_updateel(id1,id2,sizes%nel,sizes%nel1,dh(ielelid)%iaddr,       &
&                        dh(ielfcid)%iaddr,dh(store2id)%raddr,                 &
&                        dh(elmassid)%raddr,dh(store6id)%raddr,                &
&                        dh(fluxid)%raddr,dh(rwork1id)%raddr,                  &
&                        dh(elenergyid)%raddr)

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE ale_dr_advectvarel

  SUBROUTINE ale_dr_advectbasisnd(id1,id2,ale,sizes,dh,timer)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: id1,id2
    TYPE(ale_t),                   INTENT(IN)    :: ale
    TYPE(sizes_t),                 INTENT(IN)    :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(timer_t),                 INTENT(INOUT) :: timer

    ! Timer
    CALL timer_start(timer)

    ! initialise
    CALL ale_kn_initbasisnd(sizes%nnd2,dh(store3id)%raddr,dh(store4id)%raddr,  &
&                           dh(store2id)%raddr)
 
    ! construct pre/post nodal volumes and pre nodal/corner mass
    CALL ale_kn_calcbasisnd(sizes%nel2,sizes%nnd2,dh(ielndid)%iaddr,           &
&                           dh(ielsort2id)%iaddr,dh(store1id)%raddr,           &
&                           dh(elvolumeid)%raddr,dh(cnmassid)%raddr,           &
&                           dh(store3id)%raddr,dh(store4id)%raddr,             &
&                           dh(store2id)%raddr,dh(rwork3id)%raddr)
 
    ! construct volume and mass flux
    CALL ale_kn_fluxbasisnd(id1,id2,sizes%nel2,dh(ielelid)%iaddr,              &
&                           dh(ielfcid)%iaddr,dh(ielsort2id)%iaddr,            &
&                           dh(fcdvid)%raddr,dh(fcdmid)%raddr,                 &
&                           dh(rwork1id)%raddr,dh(rwork2id)%raddr,             &
&                           dh(fluxid)%raddr)
 
    ! construct post nodal/corner mass
    CALL utils_kn_copy(sizes%nnd2,dh(store2id)%raddr,dh(store1id)%raddr)
    CALL ale_kn_massbasisnd(sizes%nel2,sizes%nnd2,dh(ielndid)%iaddr,           &
&                           dh(ielsort2id)%iaddr,dh(fluxid)%raddr,             &
&                           dh(cnmassid)%raddr,dh(store1id)%raddr)
 
    ! construct cut-offs
    CALL ale_kn_cutbasisnd(sizes%nnd,ale%global%zerocut,ale%global%dencut,     &
&                          dh(store3id)%raddr,dh(store5id)%raddr,              &
&                          dh(store6id)%raddr)

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE ale_dr_advectbasisnd

  SUBROUTINE ale_dr_advectvarnd(sizes,dh,timer)

    ! Argument list
    TYPE(sizes_t),             INTENT(IN)    :: sizes
    TYPE(data_t), DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(timer_t),             INTENT(INOUT) :: timer

    ! Timer
    CALL timer_start(timer)

    ! momentum (mass weighted)
    CALL ale_kn_activend(sizes%nnd,-1_ink,dh(indstatusid)%iaddr,               &
&                        dh(indtypeid)%iaddr,dh(zactiveid)%zaddr)
    CALL ale_kn_fluxndvl(sizes%nel1,sizes%nel2,dh(ielelid)%iaddr,              &
&                        dh(ielfcid)%iaddr,dh(rwork3id)%raddr,                 &
&                        dh(rwork2id)%raddr,dh(cnuid)%raddr,dh(fluxid)%raddr)
    CALL ale_kn_updatend(sizes%nnd,sizes%nel1,sizes%nnd1,dh(ielndid)%iaddr,    &
&                        dh(store2id)%raddr,dh(store1id)%raddr,                &
&                        dh(store6id)%raddr,dh(zactiveid)%zaddr,               &
&                        dh(fluxid)%raddr,dh(fcdmid)%raddr,dh(nduid)%raddr)
    CALL ale_kn_activend(sizes%nnd,-2_ink,dh(indstatusid)%iaddr,               &
&                        dh(indtypeid)%iaddr,dh(zactiveid)%zaddr)
    CALL ale_kn_fluxndvl(sizes%nel1,sizes%nel2,dh(ielelid)%iaddr,              &
&                        dh(ielfcid)%iaddr,dh(rwork3id)%raddr,                 &
&                        dh(rwork2id)%raddr,dh(cnvid)%raddr,dh(fluxid)%raddr)
    CALL ale_kn_updatend(sizes%nnd,sizes%nel1,sizes%nnd1,dh(ielndid)%iaddr,    &
&                        dh(store2id)%raddr,dh(store1id)%raddr,                &
&                        dh(store6id)%raddr,dh(zactiveid)%zaddr,               &
&                        dh(fluxid)%raddr,dh(fcdmid)%raddr,dh(ndvid)%raddr)

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE ale_dr_advectvarnd

END MODULE ale_dr_advect_mod
