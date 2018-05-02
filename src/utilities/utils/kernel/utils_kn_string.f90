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
MODULE utils_kn_string_mod

  USE dataAPI_kinds_mod, ONLY: ink,lok
  USE dataAPI_params_mod,ONLY: SUCCESS,FAILURE

  IMPLICIT NONE

  PUBLIC :: utils_kn_convupper,utils_kn_findstr,utils_kn_matchstr

CONTAINS

  SUBROUTINE utils_kn_convupper(str)

    ! Argument list
    CHARACTER(LEN=*),INTENT(INOUT) :: str
    ! Local
    INTEGER(KIND=ink)              :: ii,ic

    DO ii=1,LEN(str)
      ic=IACHAR(str(ii:ii))
      IF ((ic.GE.97_ink).AND.(ic.LE.122)) THEN
        ic=ic-32_ink
        str(ii:ii)=ACHAR(ic)
      ENDIF
    ENDDO

  END SUBROUTINE utils_kn_convupper

  FUNCTION utils_kn_findstr(str,iunit,ln) RESULT(zflag)


    ! Argument list
    CHARACTER(LEN=*), INTENT(IN) :: str
    INTEGER(KIND=ink),INTENT(IN) :: iunit,ln
    ! Result
    LOGICAL(KIND=lok)            :: zflag
    ! Local
    INTEGER(KIND=ink)            :: il,ierr
    CHARACTER(LEN=ln)            :: sline
    CHARACTER(LEN=LEN(str))      :: stru

    ! Initialise
    zflag=.FALSE._lok
    ! search for string
    il=LEN(str)
    stru=str
    CALL utils_kn_convupper(stru)
    REWIND(UNIT=iunit)
    l1:DO
      READ(UNIT=iunit,FMT=*,IOSTAT=ierr) sline
      IF (ierr.LT.0_ink) RETURN
      sline=TRIM(ADJUSTL(sline))
      IF (sline(1:1).EQ.'&') THEN
        CALL utils_kn_convupper(sline(2:il+1))
        IF (stru(1:il).EQ.sline(2:il+1)) THEN
          zflag=.TRUE._lok
          RETURN
        ENDIF
      ENDIF
    ENDDO l1

  END FUNCTION utils_kn_findstr

  FUNCTION utils_kn_matchstr(pattern,str) RESULT(iflag)

    ! Argument list
    CHARACTER(LEN=*),INTENT(IN) :: pattern,str
    ! Result
    INTEGER(KIND=ink) :: iflag
    ! Local
    INTEGER(KIND=ink)                :: isz_pattern,isz_str,ii
    CHARACTER(LEN=LEN_TRIM(pattern)) :: patternU
    CHARACTER(LEN=LEN_TRIM(str))     :: strU

    ! initialise
    iflag=SUCCESS

    ! checks
    isz_pattern=LEN_TRIM(pattern)
    isz_str=LEN_TRIM(str)
    IF (isz_pattern.GE.isz_str) THEN
      iflag=FAILURE
      RETURN
    ENDIF

    ! search
    patternU=pattern
    CALL utils_kn_convupper(patternU)
    strU=str
    CALL utils_kn_convupper(strU)
    search:DO ii=1,isz_str-isz_pattern+1
      IF (strU(ii:ii+isz_pattern-1).EQ.patternU) THEN
        iflag=ii
        RETURN
      ENDIF
    ENDDO search

  END FUNCTION utils_kn_matchstr

END MODULE utils_kn_string_mod
