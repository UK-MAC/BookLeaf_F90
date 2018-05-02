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
MODULE dataAPI_types_mod

  ! Internal
  USE dataAPI_kinds_mod, ONLY: ink,lak,lok,rlk
  USE dataAPI_params_mod,ONLY: SLEN,NLEN
  USE time_cf_types_mod, ONLY: time_t
  USE hydro_cf_types_mod,ONLY: hydro_t
  USE ale_cf_types_mod,  ONLY: ale_t
  USE io_cf_types_mod,   ONLY: io_t
  USE comms_types_mod,   ONLY: comms_t,comm_t
  USE global_types_mod,  ONLY: global_t
  USE eos_cf_types_mod,  ONLY: eos_t

  TYPE,PUBLIC :: data_t
    CHARACTER(LEN=20)                         :: dname
    INTEGER(KIND=ink)                         :: taddr
    REAL(KIND=rlk),      DIMENSION(:),POINTER :: raddr
    INTEGER(KIND=ink),   DIMENSION(:),POINTER :: iaddr
    LOGICAL(KIND=lak),   DIMENSION(:),POINTER :: zaddr
    INTEGER(KIND=ink)                         :: dsize
  END TYPE data_t

  TYPE,PUBLIC :: sizes_t
    INTEGER(KIND=ink) :: nel,nel1,nel2,nnd,nnd1,nnd2,nsz,nmat,nreg,ncp,nmx,mcp,&
&                        mmx
  END TYPE sizes_t

  TYPE,PUBLIC :: timestep_t
    INTEGER(KIND=ink)   :: nstep,idtel
    LOGICAL(KIND=lok)   :: zcorrector
    CHARACTER(LEN=8)    :: sdt
    CHARACTER(LEN=NLEN) :: mdt
    REAL(KIND=rlk)      :: time,dt,dts
  END TYPE timestep_t

  TYPE,PUBLIC :: dt_t
    REAL(KIND=rlk)              :: rdt
    INTEGER(KIND=ink)           :: idt
    CHARACTER(LEN=8)            :: sdt
    CHARACTER(LEN=NLEN)         :: mdt
    TYPE(dt_t),         POINTER :: next=>NULL()
  END TYPE dt_t

  TYPE,PUBLIC :: config_t
    TYPE(time_t)           :: time
    TYPE(hydro_t)          :: hydro
    TYPE(eos_t),   POINTER :: eos
    TYPE(ale_t)            :: ale
    TYPE(io_t),    POINTER :: io
    TYPE(comms_t)          :: comm
    TYPE(global_t),POINTER :: global
  END TYPE config_t

  TYPE,PUBLIC :: runtime_t
    TYPE(timestep_t) :: timestep
    TYPE(sizes_t)    :: sizes
  END TYPE runtime_t

  TYPE,PUBLIC :: error_t
    INTEGER(KIND=ink)   :: ierr,iout
    CHARACTER(LEN=SLEN) :: serr
  END TYPE error_t

END MODULE dataAPI_types_mod

