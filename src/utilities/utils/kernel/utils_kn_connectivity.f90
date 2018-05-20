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
MODULE utils_kn_connectivity_mod

  USE dataAPI_kinds_mod, ONLY: ink,ilk
  USE dataAPI_params_mod,ONLY: NFACE,NCORN
  USE utils_kn_sort_mod, ONLY: utils_kn_sort,utils_kn_arth

  IMPLICIT NONE

  PUBLIC :: utils_kn_getconn,utils_kn_getsconn,utils_kn_corrconn

CONTAINS

  PURE FUNCTION utils_kn_getconn(nCell,e2v) RESULT(e2e)

    ! argument list
    INTEGER(KIND=ink),                       INTENT(IN) :: nCell
    INTEGER(KIND=ink),DIMENSION(NCORN,nCell),INTENT(IN) :: e2v
    ! result
    INTEGER(KIND=ink),DIMENSION(NFACE,nCell)            :: e2e
    ! local
    INTEGER(KIND=ink)                                   :: nSz,i1,i2,i3,i4,i5,i6
    INTEGER(KIND=ink),DIMENSION(nCell*NFACE)            :: iConn,iWork1,iWork2, &
                                                           iWork3
    INTEGER(KIND=ilk),DIMENSION(nCell*NFACE)            :: iUind

    ! initialise
    e2e=0_ink

    ! set loop size
    nSz=NFACE*nCell

    ! set work and initial connectivity
    DO i1=1,nCell
      i2=nCell+i1
      i3=nCell+i2
      i4=nCell+i3
      iWork1(i1)=e2v(1,i1)
      iWork2(i1)=e2v(2,i1)
      iWork1(i2)=e2v(2,i1)
      iWork2(i2)=e2v(3,i1)
      iWork1(i3)=e2v(3,i1)
      iWork2(i3)=e2v(4,i1)
      iWork1(i4)=e2v(4,i1)
      iWork2(i4)=e2v(1,i1)
      iConn(i1)=i1
      iConn(i2)=i1
      iConn(i3)=i1
      iConn(i4)=i1
    ENDDO

    ! set unique index
    i4=MAXVAL(e2v)
    DO i1=1,nSz
      i5=iWork1(i1)
      i6=iWork2(i1)
      i2=MAX(i5,i6)
      i3=MIN(i5,i6)-1_ink
      iUind(i1)=INT(i3, KIND=ilk) * INT(i4, KIND=ilk) + INT(i2, KIND=ilk)
    ENDDO

    ! sort unique index
    iWork1=utils_kn_sort(iUind)
    IF (iWork1(1).EQ.-HUGE(1_ilk)) THEN
      e2e(1,1)=-HUGE(1_ink)
      RETURN
    ENDIF

    ! find matches
    i2=0_ink
    DO i1=1,nSz-1
      i3=iWork1(i1)
      i4=iWork1(i1+1)
      IF (iUind(i3).EQ.iUind(i4)) THEN
        i2=i2+1_ink
        iWork2(i2)=i1
      ENDIF
    ENDDO

    ! insert matches into connectivity table
    iWork3=utils_kn_arth(1_ink,1_ink,nSz)
    iWork3=iWork3(iWork1)
    iConn=iConn(iWork1)
    iWork1=0_ink
    DO i1=1,i2
      i3=iWork2(i1)
      i4=i3+1_ink
      iWork1(iWork3(i3))=iConn(i4)
      iWork1(iWork3(i4))=iConn(i3)
    ENDDO

    ! copy to result
    i4=0_ink
    DO i1=1,nFace
      DO i3=1,nCell
        i4=i4+1_ink
        e2e(i1,i3)=INT(iWork1(i4), KIND=ink)
      ENDDO
    ENDDO

  END FUNCTION utils_kn_getconn

  PURE FUNCTION utils_kn_getsconn(nCell,e2e) RESULT(e2s)

    ! argument list
    INTEGER(KIND=ink),                       INTENT(IN) :: nCell
    INTEGER(KIND=ink),DIMENSION(NFACE,nCell),INTENT(IN) :: e2e
    ! result
    INTEGER(KIND=ink),DIMENSION(NFACE,nCell)            :: e2s
    ! local
    INTEGER(KIND=ink)                                   :: iel,i1,i2,ineigh
    INTEGER(KIND=ink),DIMENSION(0:nCell)                :: istore

    ! initialise
    e2s=0_ink

    ! set side connectivity
    DO iel=1,nCell
      DO i1=1,NFACE
        ineigh=e2e(i1,iel)
        IF (ineigh.GT.0_ink) THEN
          DO i2=1,nFace
            istore(e2e(i2,ineigh))=i2
          ENDDO
          e2s(i1,iel)=istore(iel)
        ENDIF
      ENDDO
    ENDDO

  END FUNCTION utils_kn_getsconn

  SUBROUTINE utils_kn_corrconn(nCell,e2e,e2s)

    ! Argument list
    INTEGER(KIND=ink),                       INTENT(IN)    :: nCell
    INTEGER(KIND=ink),DIMENSION(NFACE,nCell),INTENT(INOUT) :: e2e,e2s
    ! Local
    INTEGER(KIND=ink)                                      :: iel,i1,i2

    DO iel=1,nCell
      DO i1=1,NFACE/2
        i2=i1+2_ink
        IF (e2e(i1,iel).EQ.0_ink) THEN
          e2e(i1,iel)=iel
          e2s(i1,iel)=i2
        ENDIF
        IF (e2e(i2,iel).EQ.0_ink) THEN
          e2e(i2,iel)=iel
          e2s(i2,iel)=i1
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE utils_kn_corrconn

END MODULE utils_kn_connectivity_mod
