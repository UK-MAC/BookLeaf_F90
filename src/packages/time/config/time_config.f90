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
MODULE time_cf_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_types_mod, ONLY: time_t,error_t,comm_t,io_t
  USE dataAPI_params_mod,ONLY: OSTREAM,SUCCESS,FAILURE,HALT_SINGLE

  IMPLICIT NONE

  PUBLIC :: time_cf_defaults,time_cf_get,time_cf_set,time_cf_rationalise,      &
&           time_cf_print

CONTAINS

  SUBROUTINE time_cf_defaults(time)

    ! Argument list
    TYPE(time_t),INTENT(INOUT) :: time

    ! set Time defaults
    time%time_start=0.0_rlk
    time%time_end=1.0_rlk
    time%dt_initial=1.0e-5_rlk
    time%dt_g=1.02_rlk
    time%dt_min=1.0e-8_rlk
    time%dt_max=1.0e-1_rlk

    ! nullify pointers to other sections of config
    NULLIFY(time%comm,time%io)

  END SUBROUTINE time_cf_defaults

  SUBROUTINE time_cf_get(time,time_start,time_end,dt_g,dt_min,dt_max,          &
&                        dt_initial)

    ! Argument list
    TYPE(time_t),           INTENT(IN)  :: time
    REAL(KIND=rlk),OPTIONAL,INTENT(OUT) :: time_start,time_end,dt_g,dt_min,    &
&                                          dt_max,dt_initial

    IF (PRESENT(time_start)) time_start=time%time_start
    IF (PRESENT(time_end))   time_end  =time%time_end
    IF (PRESENT(dt_g))       dt_g      =time%dt_g
    IF (PRESENT(dt_min))     dt_min    =time%dt_min
    IF (PRESENT(dt_max))     dt_max    =time%dt_max
    IF (PRESENT(dt_initial)) dt_initial=time%dt_initial

  END SUBROUTINE time_cf_get

  SUBROUTINE time_cf_set(time,time_start,time_end,dt_g,dt_min,dt_max,          &
&                        dt_initial)

    ! Argument list
    TYPE(time_t),           INTENT(INOUT) :: time
    REAL(KIND=rlk),OPTIONAL,INTENT(IN)    :: time_start,time_end,dt_g,dt_min,  &
&                                            dt_max,dt_initial

    IF (PRESENT(time_start)) time%time_start=time_start
    IF (PRESENT(time_end))   time%time_end  =time_end
    IF (PRESENT(dt_g))       time%dt_g      =dt_g
    IF (PRESENT(dt_min))     time%dt_min    =dt_min
    IF (PRESENT(dt_max))     time%dt_max    =dt_max
    IF (PRESENT(dt_initial)) time%dt_initial=dt_initial

  END SUBROUTINE time_cf_set

  SUBROUTINE time_cf_rationalise(time,comm,io,error)

    ! Argument list
    TYPE(time_t),         INTENT(INOUT) :: time
    TYPE(comm_t), POINTER,INTENT(IN)    :: comm
    TYPE(io_t),   POINTER,INTENT(IN)    :: io
    TYPE(error_t),        INTENT(OUT)   :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! rationalise
    IF (time%time_start.GT.time%time_end) THEN
      error%ierr=FAILURE
      error%serr="ERROR: time_start > time_end"
      RETURN
    ENDIF
    IF (time%dt_g.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: dt_g < 0"
      RETURN
    ENDIF
    IF (time%dt_min.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: dt_min < 0"
      RETURN
    ENDIF
    IF (time%dt_max.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: dt_max < 0"
      RETURN
    ENDIF
    IF (time%dt_min.GT.time%dt_max) THEN
      error%ierr=FAILURE
      error%serr="ERROR: dt_min > dt_max"
      RETURN
    ENDIF
    IF (time%dt_initial.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: dt_initial < 0"
      RETURN
    ENDIF
    NULLIFY(time%comm)
    time%comm=>comm
    NULLIFY(time%io)
    time%io=>io

  END SUBROUTINE time_cf_rationalise

  SUBROUTINE time_cf_print(time)

    ! Argument list
    TYPE(time_t),INTENT(IN) :: time

    WRITE(OSTREAM,'(a13)') ' TIME OPTIONS'
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Time at which calculation starts:     '&
&    //'                                time_start ',time%time_start
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Time at which calculation ends:       '&
&    //'                                  time_end ',time%time_end
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Initial timestep:                     '&
&    //'                                dt_initial ',time%dt_initial
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Minimum allowed timestep:             '&
&    //'                                    dt_min ',time%dt_min
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Maximum allowed timestep:             '&
&    //'                                    dt_max ',time%dt_max
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Timestep growth factor:               '&
&    //'                                      dt_g ',time%dt_g
    WRITE(OSTREAM,'(a132)')' #################################################'&
&    //'######################################################################'&
&    //'############'

  END SUBROUTINE time_cf_print

END MODULE time_cf_mod
