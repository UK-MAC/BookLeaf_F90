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
MODULE global_cf_mod

  USE dataAPI_kinds_mod, ONLY: rlk
  USE dataAPI_params_mod,ONLY: OSTREAM,SUCCESS,FAILURE,HALT_SINGLE
  USE dataAPI_types_mod, ONLY: error_t
  USE global_types_mod,  ONLY: global_t

  IMPLICIT NONE

  PUBLIC :: global_cf_defaults,global_cf_print,global_cf_rationalise,          &
&           global_cf_get,global_cf_set

CONTAINS  

  SUBROUTINE global_cf_defaults(global)

    ! Argument list
    TYPE(global_t),INTENT(INOUT) :: global

    global%zcut=1.0e-8_rlk
    global%zerocut=1.0e-40_rlk
    global%dencut=1.0e-6_rlk
    global%accut=1.0e-6_rlk

  END SUBROUTINE global_cf_defaults

  SUBROUTINE global_cf_rationalise(global,error)

    ! Argument list
    TYPE(global_t),INTENT(INOUT) :: global
    TYPE(error_t), INTENT(OUT)   :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! rationalise
    IF (global%zcut.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: zcut < 0"
      RETURN
    ENDIF
    IF (global%zerocut.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: zerocut < 0"
      RETURN
    ENDIF
    IF (global%dencut.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: dencut < 0"
      RETURN
    ENDIF
    IF (global%accut.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: accut < 0"
      RETURN
    ENDIF

  END SUBROUTINE global_cf_rationalise

  SUBROUTINE global_cf_print(global)

    ! Argument list
    TYPE(global_t),INTENT(IN) :: global

    WRITE(OSTREAM,'(a15)') ' GLOBAL OPTIONS'
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Rounding precision cut-off:           '&
&    //'                                      zcut ',global%zcut
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Underflow cut-off:                    '&
&    //'                                   zerocut ',global%zerocut
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Density cut-off:                      '&
&    //'                                    dencut ',global%dencut
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Acceleration cut-off:                 '&
&    //'                                     accut ',global%accut
    WRITE(OSTREAM,'(a132)') ' ################################################'&
&    //'######################################################################'&
&    //'#############'
  END SUBROUTINE global_cf_print

  SUBROUTINE global_cf_get(global,zcut,zerocut,dencut,accut)

    ! Argument list
    TYPE(global_t),         INTENT(IN)  :: global
    REAL(KIND=rlk),OPTIONAL,INTENT(OUT) :: zcut,zerocut,dencut,accut

    IF (PRESENT(zcut))    zcut   =global%zcut
    IF (PRESENT(zerocut)) zerocut=global%zerocut
    IF (PRESENT(dencut))  dencut =global%dencut
    IF (PRESENT(accut))   accut  =global%accut

  END SUBROUTINE global_cf_get

  SUBROUTINE global_cf_set(global,zcut,zerocut,dencut,accut)

    ! Argument list
    TYPE(global_t),         INTENT(INOUT) :: global
    REAL(KIND=rlk),OPTIONAL,INTENT(IN)    :: zcut,zerocut,dencut,accut

    IF (PRESENT(zcut))    global%zcut   =zcut
    IF (PRESENT(zerocut)) global%zerocut=zerocut
    IF (PRESENT(dencut))  global%dencut =dencut
    IF (PRESENT(accut))   global%accut  =accut

  END SUBROUTINE global_cf_set

END MODULE global_cf_mod    
