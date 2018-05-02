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
MODULE timer_control_mod

  USE dataAPI_kinds_mod,  ONLY: rlk,lok,ink
  USE dataAPI_types_mod,  ONLY: error_t
  USE dataAPI_params_mod, ONLY: SUCCESS,HALT_SINGLE
  USE timerAPI_types_mod, ONLY: timer_t
  USE timerAPI_id_mod,    ONLY: NTIMERS,TTOTALID,TINITID,TGETEOSIID,           &
&                               TGETGEOMETRYIID,TCOMMREGISTERID,TMESHGENID,    &
&                               TMESHPARTITIONID,TSETUPICID,TSOLVERID,TGETDTID,&
&                               TGETVISCOSITYID,TCOLLECTIVETID,TCOMMTID,       &
&                               TLAGSTEPID,TGETEOSLID,TGETACCELERATIONID,      &
&                               TGETGEOMETRYLID,TGETENERGYID,TGETFORCEID,      &
&                               TGETHGID,TGETSPID,TCOMMLID,TALESTEPID,         &
&                               TALEGETMESHSTATUSID,TALEGETFLUXVOLUMEID,       &
&                               TALEADVECTID,TALEADVECTELID,                   &
&                               TALEADVECTBASISELID,TALEADVECTVARELID,         &
&                               TALEADVECTNDID,TALEADVECTBASISNDID,            &
&                               TALEADVECTVARNDID,TALEUPDATEID,TGETEOSAID,     &
&                               TGETGEOMETRYAID,TCOMMAID,TSTEPIOID,TIOID
  USE timer_advance_mod,  ONLY: timer_start

  IMPLICIT NONE

  PUBLIC :: timer_init,timer_finish

CONTAINS

  SUBROUTINE timer_init(timer,error)

    ! Argument list
    TYPE(timer_t),DIMENSION(:),ALLOCATABLE,INTENT(OUT) :: timer
    TYPE(error_t),                         INTENT(OUT) :: error
    ! Local
    INTEGER(KIND=ink) :: ii

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! allocate timer
    ALLOCATE(timer(NTIMERS),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN

    ! initialise timer data
    DO ii=LBOUND(timer,DIM=1,KIND=ink),UBOUND(timer,DIM=1,KIND=ink)
      timer(ii)%start=-1.0_rlk
      timer(ii)%time=0.0_rlk
      timer(ii)%zactive=.FALSE._lok
    ENDDO

    ! register timers
    timer(TTOTALID)%string=           ' total run time                    '
    timer(TINITID)%string=            '   time in initialisation          '
    timer(TGETEOSIID)%string=         '     time in geteos                '
    timer(TGETGEOMETRYIID)%string=    '     time in getgeometry           '    
    timer(TCOMMREGISTERID)%string=    '     time in register              '    
    timer(TMESHGENID)%string=         '     time in mesh generator        '    
    timer(TMESHPARTITIONID)%string=   '     time in mesh partition        '    
    timer(TSETUPICID)%string=         '     time in initial conditions    '
    timer(TSOLVERID)%string=          '   time in solver                  '
    timer(TGETDTID)%string=           '     time in getdt                 '    
    timer(TGETVISCOSITYID)%string=    '       time in getviscosity        '
    timer(TCOMMTID)%string=           '       time in MPI exchanges       '
    timer(TCOLLECTIVETID)%string=     '       time in MPI collectives     '
    timer(TLAGSTEPID)%string=         '     time in lagstep               '
    timer(TGETEOSLID)%string=         '       time in geteos              '    
    timer(TGETACCELERATIONID)%string= '       time in getacceleration     '    
    timer(TGETGEOMETRYLID)%string=    '       time in getgeometry         '    
    timer(TGETENERGYID)%string=       '       time in getenergy           '    
    timer(TGETFORCEID)%string=        '       time in getforce            '    
    timer(TGETHGID)%string=           '       time in gethg               '
    timer(TGETSPID)%string=           '       time in getsp               '
    timer(TCOMMLID)%string=           '       time in MPI exchanges       '
    timer(TALESTEPID)%string=         '     time in alestep               '    
    timer(TALEGETMESHSTATUSID)%string='       time in getmeshstatus       '    
    timer(TALEGETFLUXVOLUMEID)%string='       time in getfluxvolume       '    
    timer(TALEADVECTID)%string=       '       time in advect              '    
    timer(TALEADVECTELID)%string=     '          time in advectel         '
    timer(TALEADVECTBASISELID)%string='             time in advectbasisel '
    timer(TALEADVECTVARELID)%string=  '             time in advectvarel   '
    timer(TALEADVECTNDID)%string=     '          time in advectnd         '
    timer(TALEADVECTBASISNDID)%string='             time in advectbasisnd '
    timer(TALEADVECTVARNDID)%string=  '             time in advectvarnd   '
    timer(TALEUPDATEID)%string=       '       time in update              '
    timer(TGETEOSAID)%string=         '          time in geteos           '
    timer(TGETGEOMETRYAID)%string=    '          time in getgeometry      '
    timer(TCOMMAID)%string=           '       time in MPI exchanges       '
    timer(TSTEPIOID)%string=          '     time in step IO               '
    timer(TIOID)%string=              '     time in output dumps          '

    ! start initial timers
    CALL timer_start(timer(TTOTALID))
    CALL timer_start(timer(TINITID))

  END SUBROUTINE timer_init

  SUBROUTINE timer_finish(timer,error)

    ! Argument list
    TYPE(timer_t),DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: timer
    TYPE(error_t),                         INTENT(OUT)   :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! shutdown timer
    DEALLOCATE(timer,STAT=error%ierr,ERRMSG=error%serr)

  END SUBROUTINE timer_finish

END MODULE timer_control_mod
