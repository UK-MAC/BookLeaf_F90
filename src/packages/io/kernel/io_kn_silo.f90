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
MODULE io_kn_silo_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk,lok
  USE dataAPI_params_mod,ONLY: SLEN,OSTREAM

  IMPLICIT NONE

  INTERFACE io_kn_silo_Dmaterial
    MODULE PROCEDURE io_kn_silo_DPmaterial,io_kn_silo_DMmaterial
  END INTERFACE

  INTERFACE io_kn_silo_DRvar
    MODULE PROCEDURE io_kn_silo_DRPvar,io_kn_silo_DRMvar
  END INTERFACE

  PRIVATE :: io_kn_silo_DPmaterial,io_kn_silo_DMmaterial,io_kn_silo_DRPvar,    &
&            io_kn_silo_DRMvar
  PUBLIC  :: io_kn_set_silo_material,io_kn_silo_close,io_kn_silo_Hcreate,      &
&            io_kn_silo_Hmesh,io_kn_silo_Hmaterial,io_kn_silo_Hvar,            &
&            io_kn_silo_Dcreate,io_kn_silo_Dmesh,io_kn_silo_Dmaterial,         &
&            io_kn_silo_DRvar,io_kn_silo_DIvar,io_kn_set_silo_cp

CONTAINS

  SUBROUTINE io_kn_set_silo_material(nMat,sMat,SLEN,iMat,lMat,cMat)

    ! Argument list
    INTEGER(KIND=ink),                  INTENT(IN)   :: nMat,SLEN
    CHARACTER(LEN=*),   DIMENSION(nMat),INTENT(IN)   :: sMat
    INTEGER(KIND=ink),  DIMENSION(nMat),INTENT(OUT)  :: iMat,lMat
    CHARACTER(LEN=SLEN),DIMENSION(nMat),INTENT(OUT)  :: cMat
    ! Local
    INTEGER(KIND=ink) :: ii

    ! set material information
    DO ii=1,nMat
      iMat(ii)=ii
      cMat(ii)=TRIM(ADJUSTL(sMat(ii)))
      lMat(ii)=LEN_TRIM(cMat(ii))
    ENDDO

  END SUBROUTINE io_kn_set_silo_material

  SUBROUTINE io_kn_set_silo_cp(nmx,imxel,imxfcp,imxncp,ncp,icpnext,icpel,nel,  &
&                              ielmat)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nmx,ncp,nel
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(IN)    :: imxel,imxfcp,imxncp
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(IN)    :: icpnext
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(OUT)   :: icpel
    INTEGER(KIND=ink),DIMENSION(nel),INTENT(INOUT) :: ielmat
    ! Local
    INTEGER(KIND=ink) :: imix,icp,iel,ii

    DO imix=1,nmx
      iel=imxel(imix)
      icp=imxfcp(imix)
      ielmat(iel)=-icp
      DO ii=1,imxncp(imix)
        icpel(icp)=iel
        icp=icpnext(icp)
      ENDDO
    ENDDO

  END SUBROUTINE io_kn_set_silo_cp

  SUBROUTINE io_kn_silo_close(iFileID)

    ! External
    INCLUDE "silo.inc"

    ! Argument list
    INTEGER(KIND=ink),INTENT(INOUT) :: iFileID
    ! Local
    INTEGER(KIND=ink) :: iErr

    iErr=DBClose(iFileID)

  END SUBROUTINE io_kn_silo_close

  SUBROUTINE io_kn_silo_Hcreate(cDirName,cHeaderFile,iFileID)

    ! External
    INCLUDE 'silo.inc'

    ! Argument list
    CHARACTER(LEN=*), INTENT(IN)  :: cDirName,cHeaderFile
    INTEGER(KIND=ink),INTENT(OUT) :: iFileID
    ! Local
    INTEGER(KIND=ink)   :: iErr
    CHARACTER(LEN=SLEN) :: cString

    ! log
    WRITE(OSTREAM,*) 'Writing SILO file: '//TRIM(cDirName)
    WRITE(OSTREAM,*) ' '

    ! remove deprecated warning messages
    iErr=DBSetDepWarn(0)

    ! create header file
    cString=TRIM(cDirName)//'/'//TRIM(cHeaderFile)
    iErr=DBCreate(TRIM(cString),LEN_TRIM(cString),DB_CLOBBER,DB_LOCAL,         &
&                 'SILO file from Bookleaf',23,DB_HDF5,iFileID)
    iErr=DBSet2DstrLen(SLEN)

  END SUBROUTINE io_kn_silo_Hcreate

  SUBROUTINE io_kn_silo_Hstring(nProc,cDirName,cDataName,cString)

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN)  :: nProc
    CHARACTER(LEN=*),                    INTENT(IN)  :: cDirName,cDataName
    CHARACTER(LEN=SLEN),DIMENSION(nProc),INTENT(OUT) :: cString
    ! Local
    INTEGER(KIND=ink) :: ii
    CHARACTER(LEN=4)  :: cNum

    DO ii=1,nProc
      WRITE(cNum,'(i4)') ii+999_ink
      cString(ii)=TRIM(cDirName)//'/'//TRIM(cDataName)//cNum(2:4)//'.silo'
    ENDDO

  END SUBROUTINE io_kn_silo_Hstring

  SUBROUTINE io_kn_silo_Hmesh(iFileID,nProc,cMesh,cString)

    ! External
    INCLUDE 'silo.inc'

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN) :: iFileID,nProc
    CHARACTER(LEN=*),              INTENT(IN) :: cMesh
    CHARACTER(LEN=*), DIMENSION(:),INTENT(IN) :: cString
    ! Local
    INTEGER(KIND=ink)                    :: ii,iErr
    CHARACTER(LEN=SLEN),DIMENSION(nProc) :: cName
    INTEGER(KIND=ink),  DIMENSION(nProc) :: iName,iType

    DO ii=1,nProc
      cName(ii)=TRIM(cString(ii))//':'//TRIM(cMesh)
      iName(ii)=LEN_TRIM(cName(ii))
      iType(ii)=DB_UCDMESH
    ENDDO
    iErr=DBPutMMesh(iFileID,TRIM(cMesh),LEN_TRIM(cMesh),NProc,cName,iName,     &
&                   iType,DB_F77NULL,ii)

  END SUBROUTINE io_kn_silo_Hmesh

  SUBROUTINE io_kn_silo_Hmaterial(iFileID,nProc,nMat,iMat,lMat,cMat,cString,   &
&                                 cMesh,cMaterial)

    ! External
    INCLUDE 'silo.inc'

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN) :: iFileID,nProc,nMat
    INTEGER(KIND=ink),DIMENSION(:),INTENT(IN) :: iMat,lMat
    CHARACTER(LEN=*), DIMENSION(:),INTENT(IN) :: cMat,cString
    CHARACTER(LEN=*),              INTENT(IN) :: cMesh,cMaterial
    ! Local
    INTEGER(KIND=ink)                    :: iErr,iOptionID,ii
    CHARACTER(LEN=SLEN),DIMENSION(nProc) :: cName
    INTEGER(KIND=ink),  DIMENSION(nProc) :: iName

    iErr=DBMkOptList(4,iOptionID)
    iErr=DBAddIOpt(iOptionID,DBOPT_NMATNOS,nMat)
    iErr=DBAddIOpt(iOptionID,DBOPT_MATNOS,iMat)
    iErr=DBAddCAOpt(iOptionID,DBOPT_MATNAMES,nMat,cMat,lMat)
    iErr=DBAddCOpt(iOptionID,DBOPT_MMESH_NAME,TRIM(cMesh),LEN_TRIM(cMesh))
    DO ii=1,nProc
      cName(ii)=TRIM(cString(ii))//':'//TRIM(cMaterial)
      iName(ii)=LEN_TRIM(cName(ii))
    ENDDO
    iErr=DBPutMMat(iFileID,TRIM(cMaterial),LEN_TRIM(cMaterial),nProc,cName,    &
&                  iName,iOptionID,ii)
    iErr=DBFreeOptList(iOptionID)

  END SUBROUTINE io_kn_silo_Hmaterial

  SUBROUTINE io_kn_silo_Hvar(iFileID,nProc,cVar,cString)

    ! External
    INCLUDE 'silo.inc'

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN) :: iFileID,nProc
    CHARACTER(LEN=*),              INTENT(IN) :: cVar
    CHARACTER(LEN=*), DIMENSION(:),INTENT(IN) :: cString
    ! Local
    INTEGER(KIND=ink)                    :: iErr,ii
    INTEGER(KIND=ink),  DIMENSION(nProc) :: iName,iType
    CHARACTER(LEN=SLEN),DIMENSION(nProc) :: cName

    DO ii=1,nProc
      cName(ii)=TRIM(cString(ii))//':'//TRIM(cVar(3:))
      iName(ii)=LEN_TRIM(cName(ii))
      iType(ii)=DB_UCDVAR
    ENDDO
    iErr=DBPutMVar(iFileID,TRIM(cVar(3:)),LEN_TRIM(cVar(3:)),nProc,cName,iName,&
&                  iType,DB_F77NULL,ii)

  END SUBROUTINE io_kn_silo_Hvar

  SUBROUTINE io_kn_silo_Dcreate(rankW,cDirName,cDataName,iFileID)

    ! External
    INCLUDE "silo.inc"

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN)  :: rankW
    CHARACTER(LEN=*), INTENT(IN)  :: cDirName,cDataName
    INTEGER(KIND=ink),INTENT(OUT) :: iFileID
    ! Local
    INTEGER(KIND=ink)   :: iErr
    CHARACTER(LEN=4)    :: cNum
    CHARACTER(LEN=SLEN) :: cString

    ! remove deprecated warning messages
    iErr=DBSetDepWarn(0)

    ! create data file on each PE
    WRITE(cNum,'(i4)') rankW+1000_ink
    cString=TRIM(cDirName)//'/'//TRIM(cDataName)//cNum(2:4)//'.silo'
    iErr=DBCreate(TRIM(cString),LEN_TRIM(cString),DB_CLOBBER,DB_LOCAL,         &
&                 'SILO file from bookleaf',23,DB_HDF5,iFileID)
    iErr=DBSet2DstrLen(SLEN)
  
  END SUBROUTINE io_kn_silo_DCreate
      
  SUBROUTINE io_kn_silo_Dmesh(iFileID,cMesh,nEl,nNd,cNodeList,iNodeList,ndx,   &
&                             ndy)

    ! External
    INCLUDE "silo.inc"

    ! Parameters
    INTEGER(KIND=ink),PARAMETER :: NSHAPE=1_ink,NSIZE=4_ink
    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN) :: iFileID,nEl,nNd
    INTEGER(KIND=ink),DIMENSION(NSIZE*nEl),INTENT(IN) :: iNodeList
    CHARACTER(LEN=*),                      INTENT(IN) :: cNodeList,cMesh
    REAL(KIND=rlk),   DIMENSION(nNd),      INTENT(IN) :: ndx,ndy
    ! Local
    INTEGER(KIND=ink)                     :: iErr,iOptionID,ii
    CHARACTER(LEN=SLEN)                   :: cString
    INTEGER(KIND=ink),  DIMENSION(NSHAPE) :: iShapeType,iShapeSize,iShapeCount

    iErr=DBMkOptList(3,iOptionID)
    cString(1:1)='X'
    cString(2:2)='Y'
    iErr=DBAddCOpt(iOptionID,DBOPT_XLABEL,cString(1:1),1)
    iErr=DBAddCOpt(iOptionID,DBOPT_YLABEL,cString(2:2),1)
    iErr=DBAddIOpt(iOptionID,DBOPT_COORDSYS,DB_CARTESIAN)
    iShapeType(1)=DB_ZONETYPE_QUAD
    iShapeSize(1)=NSIZE
    iShapeCount(1)=nEl
    iErr=DBPutZl2(iFileID,TRIM(cNodeList),LEN_TRIM(cNodeList),nEl,2,iNodeList, &
&                 SIZE(iNodeList),1,0,0,iShapeType,iShapeSize,iShapeCount,     &
&                 NSHAPE,DB_F77NULL,ii)
    iErr=DBPutUM(iFileID,TRIM(cMesh),LEN_TRIM(cMesh),2,ndx(1:nNd),ndy(1:nNd),  &
&                DB_F77NULL,cString(1:1),1,cString(2:2),1,DB_F77NULL,0,        &
&                DB_DOUBLE,nNd,nEl,TRIM(cNodeList),LEN_TRIM(cNodeList),        &
&                DB_F77NULL,0,iOptionID,ii)
    iErr=DBFreeOptList(iOptionID)

  END SUBROUTINE io_kn_silo_Dmesh

  SUBROUTINE io_kn_silo_DPmaterial(iFileID,nMat,iMat,cMat,lMat,cMaterial,cMesh,&
&                                  nEl,iElMat)

    ! External
    INCLUDE 'silo.inc'

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN) :: iFileID,nMat,nEl
    CHARACTER(LEN=*),                INTENT(IN) :: cMaterial,cMesh
    CHARACTER(LEN=*), DIMENSION(:),  INTENT(IN) :: cMat
    INTEGER(KIND=ink),DIMENSION(:),  INTENT(IN) :: iMat,lMat
    INTEGER(KIND=ink),DIMENSION(nEl),INTENT(IN) :: iElMat
    ! Local
    INTEGER(KIND=ink) :: iErr,iOptionID,ii

    iErr=DBMkOptList(2,iOptionID)
    iErr=DBAddIOpt(iOptionID,DBOPT_ORIGIN,0)
    iErr=DBAddCAOpt(iOptionID,DBOPT_MATNAMES,nMat,cMat,lMat)
    iErr=DBPutMat(iFileID,TRIM(cMaterial),LEN_TRIM(cMaterial),TRIM(cMesh),     &
&                 LEN_TRIM(cMesh),nMat,iMat,iElMat(1:nEl),[nEl],1,DB_F77NULL,  &
&                 DB_F77NULL,DB_F77NULL,DB_F77NULL,0,DB_DOUBLE,iOptionID,ii)
    iErr=DBFreeOptList(iOptionID)

  END SUBROUTINE io_kn_silo_DPmaterial

  SUBROUTINE io_kn_silo_DMmaterial(iFileID,nMat,iMat,cMat,lMat,cMaterial,cMesh,&
&                                  nEl,iElMat,nMx,iMxNext,iMxMat,iMxEl,rVf)

    ! External
    INCLUDE 'silo.inc'

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN) :: iFileID,nMat,nEl,nMx
    CHARACTER(LEN=*),                INTENT(IN) :: cMaterial,cMesh
    CHARACTER(LEN=*), DIMENSION(:),  INTENT(IN) :: cMat
    INTEGER(KIND=ink),DIMENSION(:),  INTENT(IN) :: iMat,lMat
    INTEGER(KIND=ink),DIMENSION(nEl),INTENT(IN) :: iElMat
    INTEGER(KIND=ink),DIMENSION(nMx),INTENT(IN) :: iMxNext,iMxMat,iMxEl
    REAL(KIND=rlk),   DIMENSION(nMx),INTENT(IN) :: rVf
    ! Local
    INTEGER(KIND=ink) :: iErr,iOptionID,ii

    iErr=DBMkOptList(2,iOptionID)
    iErr=DBAddIOpt(iOptionID,DBOPT_ORIGIN,0)
    iErr=DBAddCAOpt(iOptionID,DBOPT_MATNAMES,nMat,cMat,lMat)
    iErr=DBPutMat(iFileID,TRIM(cMaterial),LEN_TRIM(cMaterial),TRIM(cMesh),     &
&                 LEN_TRIM(cMesh),nMat,iMat,iElMat(1:nEl),[nEl],1,             &
&                 iMxNext(1:nMx),iMxMat(1:nMx),iMxEl(1:nMx),rVf(1:nMx),nMx,    &
&                 DB_DOUBLE,iOptionID,ii)
    iErr=DBFreeOptList(iOptionID)

  END SUBROUTINE io_kn_silo_DMmaterial

  SUBROUTINE io_kn_silo_DRPvar(iFileID,cMesh,cVar,rVar,nSize,zCentre)

    ! External
    INCLUDE 'silo.inc'

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN) :: iFileID,nSize
    CHARACTER(LEN=*),                    INTENT(IN) :: cVar,cMesh
    LOGICAL(KIND=lok),                   INTENT(IN) :: zCentre
    REAL(KIND=rlk),   DIMENSION(1:nSize),INTENT(IN) :: rVar
    ! Local
    INTEGER(KIND=ink) :: iErr,ii,iCentre

    IF (zCentre) THEN
      iCentre=DB_ZONECENT
    ELSE
      iCentre=DB_NODECENT
    ENDIF
    iErr=DBPutUV1(iFileID,TRIM(cVar(3:)),LEN_TRIM(cVar(3:)),TRIM(cMesh),       &
&                 LEN_TRIM(cMesh),rVar(1:nSize),nSize,DB_F77NULL,0,DB_DOUBLE,  &
&                 iCentre,DB_F77NULL,ii)

  END SUBROUTINE io_kn_silo_DRPvar

  SUBROUTINE io_kn_silo_DRMvar(iFileID,cMesh,cVar,rVar,nSize,rMVar,nMSize,     &
&                              zCentre)

    ! External
    INCLUDE 'silo.inc'

    ! Argument list
    INTEGER(KIND=ink),                    INTENT(IN) :: iFileID,nSize,nMSize
    CHARACTER(LEN=*),                     INTENT(IN) :: cVar,cMesh
    LOGICAL(KIND=lok),                    INTENT(IN) :: zCentre
    REAL(KIND=rlk),   DIMENSION(1:nSize), INTENT(IN) :: rVar
    REAL(KIND=rlk),   DIMENSION(1:nMSize),INTENT(IN) :: rMVar
    ! Local
    INTEGER(KIND=ink) :: iErr,ii,iCentre

    IF (zCentre) THEN
      iCentre=DB_ZONECENT
    ELSE
      iCentre=DB_NODECENT
    ENDIF
    iErr=DBPutUV1(iFileID,TRIM(cVar(3:)),LEN_TRIM(cVar(3:)),TRIM(cMesh),       &
&                 LEN_TRIM(cMesh),rVar(1:nSize),nSize,rMVar(1:nMSize),nMSize,  &
&                 DB_DOUBLE,iCentre,DB_F77NULL,ii)

  END SUBROUTINE io_kn_silo_DRMvar

  SUBROUTINE io_kn_silo_DIvar(iFileID,cMesh,cVar,iVar,nSize,zCentre)

    ! External
    INCLUDE 'silo.inc'

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN) :: iFileID,nSize
    CHARACTER(LEN=*),                    INTENT(IN) :: cVar,cMesh
    LOGICAL(KIND=lok),                   INTENT(IN) :: zCentre
    INTEGER(KIND=ink),DIMENSION(1:nSize),INTENT(IN) :: iVar
    ! Local
    INTEGER(KIND=ink) :: iErr,ii,iCentre

    IF (zCentre) THEN
      iCentre=DB_ZONECENT
    ELSE
      iCentre=DB_NODECENT
    ENDIF
    iErr=DBPutUV1(iFileID,TRIM(cVar(3:)),LEN_TRIM(cVar(3:)),TRIM(cMesh),       &
&                 LEN_TRIM(cMesh),iVar(1:nSize),nSize,DB_F77NULL,0,DB_INT,     &
&                 iCentre,DB_F77NULL,ii)

  END SUBROUTINE io_kn_silo_DIvar

END MODULE io_kn_silo_mod  
