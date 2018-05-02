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
MODULE check_dr_validate_mod

  USE dataAPI_kinds_mod,  ONLY: ink,rlk
  USE dataAPI_params_mod, ONLY: SUCCESS,FAILURE,HALT_SINGLE,OSTREAM
  USE dataAPI_types_mod,  ONLY: config_t,runtime_t,data_t,error_t
  USE check_dr_tests_mod, ONLY: check_dr_sod
  USE utils_kn_string_mod,ONLY: utils_kn_matchstr

  IMPLICIT NONE

  INTEGER(KIND=ink),PARAMETER,PRIVATE :: EMPTY=0_ink,SOD=1_ink

  PRIVATE :: check_dr_test,check_dr_calculate,check_dr_print
  PUBLIC  :: check_dr_validate
 
CONTAINS

  SUBROUTINE check_dr_validate(sfile,config,runtime,dh,error)

    ! Argument list
    CHARACTER(LEN=*),             INTENT(IN)    :: sfile
    TYPE(config_t),               INTENT(IN)    :: config
    TYPE(runtime_t),              INTENT(IN)    :: runtime
    TYPE(data_t),    DIMENSION(:),INTENT(INOUT) :: dh
    TYPE(error_t),                INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink) :: ivalid
    REAL(KIND=rlk)    :: norm

    ! test whether a validation is required
    CALL check_dr_test(sfile,ivalid,error)
    IF ((ivalid.EQ.EMPTY).OR.(error%ierr.NE.SUCCESS)) RETURN

    ! perform validation
    CALL check_dr_calculate(ivalid,config,runtime,dh,norm,error)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! print result
    IF (config%comm%world%zmproc) CALL check_dr_print(ivalid,norm)

  END SUBROUTINE check_dr_validate

  SUBROUTINE check_dr_test(sfile,ivalid,error)

    ! Argument list
    CHARACTER(LEN=*), INTENT(IN)  :: sfile
    INTEGER(KIND=ink),INTENT(OUT) :: ivalid
    TYPE(error_t),    INTENT(OUT) :: error
    ! Local
    INTEGER(KIND=ink) :: iflag

    ! initialise
    ivalid=EMPTY
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! Sod
    iflag=utils_kn_matchstr('sod',TRIM(ADJUSTL(sfile)))
    IF (iflag.GT.EMPTY) THEN
      ivalid=SOD
      RETURN
    ELSEIF (iflag.EQ.FAILURE) THEN
      error%ierr=FAILURE
      error%serr="ERROR: failed to match string sod in check_dr_test"
      RETURN
    ENDIF

  END SUBROUTINE check_dr_test

  SUBROUTINE check_dr_calculate(ivalid,config,runtime,dh,norm,error)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)    :: ivalid
    TYPE(config_t),                INTENT(IN)    :: config
    TYPE(runtime_t),               INTENT(IN)    :: runtime
    TYPE(data_t),     DIMENSION(:),INTENT(INOUT) :: dh
    REAL(KIND=rlk),                INTENT(OUT)   :: norm
    TYPE(error_t),                 INTENT(OUT)   :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    SELECT CASE(ivalid)
      CASE(SOD)
        CALL check_dr_sod(config,runtime,dh,norm)
      CASE DEFAULT
        error%ierr=FAILURE
        error%serr="ERROR: invalid value of ivalid in check_dr_calculate"
        RETURN
    END SELECT

  END SUBROUTINE check_dr_calculate

  SUBROUTINE check_dr_print(ivalid,norm)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN) :: ivalid
    REAL(KIND=rlk),   INTENT(IN) :: norm

    WRITE(OSTREAM,'(a132)')' #################################################'&
&    //'######################################################################'&
&    //'############'
    WRITE(OSTREAM,'(a11)') ' VALIDATION'
    SELECT CASE(ivalid)
      CASE(SOD)
        WRITE(OSTREAM,'(a25)')       '  Sod shocktube test case'
        WRITE(OSTREAM,'(a24,e16.9)') '   L1 norm. of density: ',norm
      CASE DEFAULT
    END SELECT

  END SUBROUTINE check_dr_print

END MODULE check_dr_validate_mod
