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
MODULE io_dr_silo_mod

  USE dataAPI_kinds_mod,        ONLY: ink,rlk,lok
  USE dataAPI_types_mod,        ONLY: comm_t,sizes_t,io_t,data_t,error_t
  USE dataAPI_id_mod,           ONLY: ielmatid,ielndid,ndxid,ndyid,nduid,ndvid,&
&                                     eldensityid,elenergyid,elpressureid,     &
&                                     ielregid,icpscratch11id,frvolumeid,      &
&                                     icpmatid,icpnextid,imxelid,imxfcpid,     &
&                                     imxncpid,cpdensityid,cpenergyid,         &
&                                     cppressureid,iscratch11id
  USE dataAPI_params_mod,       ONLY: SLEN,SUCCESS,FAILURE,HALT_SINGLE
  USE timerAPI_types_mod,       ONLY: timer_t
  USE timer_advance_mod,        ONLY: timer_start,timer_end
  USE io_kn_silo_mod,           ONLY: io_kn_set_silo_material,io_kn_silo_close,&
&                                     io_kn_silo_Hcreate,io_kn_silo_Hmesh,     &
&                                     io_kn_silo_Hmaterial,io_kn_silo_Hvar,    &
&                                     io_kn_silo_Dcreate,io_kn_silo_Dmesh,     &
&                                     io_kn_silo_Dmaterial,io_kn_silo_DRvar,   &
&                                     io_kn_silo_DIvar,io_kn_silo_Hstring,     &
&                                     io_kn_set_silo_cp
  USE utils_kn_copy_mod,        ONLY: utils_kn_copy
  USE typhon_API_mod,           ONLY: TYPH_barrier
  USE,INTRINSIC:: iso_c_binding,ONLY: c_int

  IMPLICIT NONE

  ! parameters
  CHARACTER(LEN=SLEN),PRIVATE,PARAMETER :: cDataName='domain',cMesh='Mesh',    &
&                                          cHeaderFile='Top.silo',             &
&                                          cMaterial='Material',               &
&                                          cNodeList='NodeList'

  ! bindings
  INTERFACE
    FUNCTION cutils_mkdir(path) BIND(C,NAME="UTILS_mkdir") RESULT(err)
      USE iso_c_binding
      CHARACTER(C_CHAR) :: path
      INTEGER(C_INT)    :: err
    END FUNCTION cutils_mkdir

    FUNCTION cutils_ln(path,lpath) BIND(C,NAME="UTILS_ln") RESULT(err)
      USE iso_c_binding
      CHARACTER(C_CHAR) :: path,lpath
      INTEGER(C_INT)    :: err
    END FUNCTION cutils_ln
  END INTERFACE

  PRIVATE :: io_dr_write_silo_header,io_dr_write_silo_dump
  PUBLIC  :: io_dr_silodump

CONTAINS

  SUBROUTINE io_dr_silodump(cfolder,io,comm,sizes,timer,dh,error)

    ! Argument list
    CHARACTER(LEN=*),             INTENT(IN)    :: cfolder
    TYPE(io_t),                   INTENT(IN)    :: io
    TYPE(comm_t),                 INTENT(IN)    :: comm
    TYPE(sizes_t),                INTENT(IN)    :: sizes
    TYPE(timer_t),                INTENT(INOUT) :: timer
    TYPE(data_t),    DIMENSION(:),INTENT(IN)    :: dh    
    TYPE(error_t),                INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=c_int)                       :: ierr
    INTEGER(KIND=ink),  DIMENSION(sizes%nmat) :: imat,lmat
    CHARACTER(LEN=SLEN),DIMENSION(sizes%nmat) :: cmat

    ! Timer
    CALL timer_start(timer)

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE
    
    ! make directory
    IF (comm%zmproc) THEN
      ierr=cutils_mkdir(TRIM(cfolder)//CHAR(0))
      IF (ierr.NE.0_c_int) THEN
        error%ierr=FAILURE
        error%serr="ERROR: failed to make silo directory: "//TRIM(cfolder)
        RETURN
      ENDIF
    ENDIF

    ! setup silo
    CALL io_kn_set_silo_material(sizes%nmat,io%smaterial,SLEN,imat,lmat,cmat)

    ! write header file on Master
    IF (comm%zmproc) THEN
      CALL io_dr_write_silo_header(cfolder,imat,lmat,cmat,comm,dh,error)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ierr=TYPH_barrier()

    ! write data file on each Domain
    CALL io_dr_write_silo_dump(cfolder,imat,lmat,cmat,comm,sizes,dh,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! create symbolic link
    IF (comm%zmproc) THEN
      ierr=cutils_ln(TRIM(cfolder)//'/'//TRIM(cheaderfile)//CHAR(0),           &
&                    TRIM(cfolder)//'.silo'//CHAR(0))
      IF (ierr.NE.0_c_int) THEN
        error%ierr=FAILURE
        error%serr="ERROR: failed to create silo link: "//TRIM(cfolder)        &
&        //".silo"
        RETURN
      ENDIF
    ENDIF

    ! Timing data
    CALL timer_end(timer)

  END SUBROUTINE io_dr_silodump

  SUBROUTINE io_dr_write_silo_header(cDirName,imat,lmat,cmat,comm,dh,error)

    ! Argument list
    CHARACTER(LEN=*),              INTENT(IN)  :: cDirName
    INTEGER(KIND=ink),DIMENSION(:),INTENT(IN)  :: imat,lmat
    CHARACTER(LEN=*), DIMENSION(:),INTENT(IN)  :: cmat
    TYPE(comm_t),                  INTENT(IN)  :: comm
    TYPE(data_t),     DIMENSION(:),INTENT(IN)  :: dh
    TYPE(error_t),                 INTENT(OUT) :: error
    ! Local
    INTEGER(KIND=ink)                         :: iFileID
    CHARACTER(LEN=SLEN),DIMENSION(comm%nproc) :: cString

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! create header file
    CALL io_kn_silo_Hcreate(cDirName,cHeaderFile,iFileID)
    IF (iFileID.EQ.-1) THEN
      error%ierr=FAILURE
      error%serr="ERROR: failed to create silo header file"
      RETURN
    ENDIF

    ! create header string
    CALL io_kn_silo_Hstring(comm%nproc,cDirName,cDataName,cString)

    ! write mesh header
    CALL io_kn_silo_Hmesh(iFileID,comm%nproc,cMesh,cString)

    ! write material header
    CALL io_kn_silo_Hmaterial(iFileID,comm%nproc,SIZE(imat),imat,lmat,cmat,    &
&                             cString,cMesh,cMaterial)

    ! write variables header
    CALL io_kn_silo_Hvar(iFileID,comm%nproc,dh(eldensityid)%dname,cString)
    CALL io_kn_silo_Hvar(iFileID,comm%nproc,dh(elenergyid)%dname,cString)
    CALL io_kn_silo_Hvar(iFileID,comm%nproc,dh(elpressureid)%dname,cString)
    CALL io_kn_silo_Hvar(iFileID,comm%nproc,dh(nduid)%dname,cString)
    CALL io_kn_silo_Hvar(iFileID,comm%nproc,dh(ndvid)%dname,cString)
    CALL io_kn_silo_Hvar(iFileID,comm%nproc,dh(ielregid)%dname,cString)

    ! close header file
    CALL io_kn_silo_close(iFileID)

  END SUBROUTINE io_dr_write_silo_header

  SUBROUTINE io_dr_write_silo_dump(cDirName,imat,lmat,cmat,comm,sizes,dh,error)

    ! Argument list
    CHARACTER(LEN=*),              INTENT(IN)  :: cDirName
    INTEGER(KIND=ink),DIMENSION(:),INTENT(IN)  :: imat,lmat
    CHARACTER(LEN=*), DIMENSION(:),INTENT(IN)  :: cmat
    TYPE(comm_t),                  INTENT(IN)  :: comm
    TYPE(sizes_t),                 INTENT(IN)  :: sizes
    TYPE(data_t),     DIMENSION(:),INTENT(IN)  :: dh
    TYPE(error_t),                 INTENT(OUT) :: error
    ! Local
    INTEGER(KIND=ink) :: iFileID

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! create data file on each PE
    CALL io_kn_silo_Dcreate(comm%rank,cDirName,cDataName,iFileID)
    IF (iFileID.EQ.-1) THEN
      error%ierr=FAILURE
      error%serr="ERROR: failed to create silo dump file"
      RETURN
    ENDIF

    ! write mesh
    CALL io_kn_silo_Dmesh(iFileID,cMesh,sizes%nel,sizes%nnd,cNodeList,         &
&                         dh(ielndid)%iaddr,dh(ndxid)%raddr,dh(ndyid)%raddr)

    IF (sizes%ncp.GT.0_ink) THEN
      ! convert multi-material data to SILO format
      CALL utils_kn_copy(sizes%nel,dh(ielmatid)%iaddr,dh(iscratch11id)%iaddr)
      CALL io_kn_set_silo_cp(sizes%nmx,dh(imxelid)%iaddr,dh(imxfcpid)%iaddr,   &
&                            dh(imxncpid)%iaddr,sizes%ncp,dh(icpnextid)%iaddr, &
&                            dh(icpscratch11id)%iaddr,sizes%nel,               &
&                            dh(iscratch11id)%iaddr)

      ! write material
      CALL io_kn_silo_Dmaterial(iFileID,sizes%nmat,iMat,cMat,lMat,cMaterial,   &
&                               cMesh,sizes%nel,dh(iscratch11id)%iaddr,        &
&                               sizes%ncp,dh(icpnextid)%iaddr,                 &
&                               dh(icpmatid)%iaddr,dh(icpscratch11id)%iaddr,   &
&                               dh(frvolumeid)%raddr)

      ! write thermodynamic variables
      CALL io_kn_silo_DRvar(iFileID,cMesh,dh(eldensityid)%dname,               &
&                          dh(eldensityid)%raddr,sizes%nel,                    &
&                          dh(cpdensityid)%raddr,sizes%ncp,.TRUE._lok)
      CALL io_kn_silo_DRvar(iFileID,cMesh,dh(elenergyid)%dname,                &
&                          dh(elenergyid)%raddr,sizes%nel,                     &
&                          dh(cpenergyid)%raddr,sizes%ncp,.TRUE._lok)
      CALL io_kn_silo_DRvar(iFileID,cMesh,dh(elpressureid)%dname,              &
&                          dh(elpressureid)%raddr,sizes%nel,                   &
&                          dh(cppressureid)%raddr,sizes%ncp,.TRUE._lok)
    ELSE
      ! write material
      CALL io_kn_silo_Dmaterial(iFileID,sizes%nmat,iMat,cMat,lMat,cMaterial,   &
&                               cMesh,sizes%nel,dh(ielmatid)%iaddr)

      ! write thermodynamic variables
      CALL io_kn_silo_DRvar(iFileID,cMesh,dh(eldensityid)%dname,               &
&                           dh(eldensityid)%raddr,sizes%nel,.TRUE._lok)
      CALL io_kn_silo_DRvar(iFileID,cMesh,dh(elenergyid)%dname,                &
&                           dh(elenergyid)%raddr,sizes%nel,.TRUE._lok)
      CALL io_kn_silo_DRvar(iFileID,cMesh,dh(elpressureid)%dname,              &
&                           dh(elpressureid)%raddr,sizes%nel,.TRUE._lok)
    ENDIF

    ! write pure variables
    CALL io_kn_silo_DRvar(iFileID,cMesh,dh(nduid)%dname,dh(nduid)%raddr,       &
&                         sizes%nnd,.FALSE._lok)
    CALL io_kn_silo_DRvar(iFileID,cMesh,dh(ndvid)%dname,dh(ndvid)%raddr,       &
&                         sizes%nnd,.FALSE._lok)
    CALL io_kn_silo_DIvar(iFileID,cMesh,dh(ielregid)%dname,dh(ielregid)%iaddr, &
&                         sizes%nel,.TRUE._lok)

    ! close data file
    CALL io_kn_silo_close(iFileID)

  END SUBROUTINE io_dr_write_silo_dump

END MODULE io_dr_silo_mod
