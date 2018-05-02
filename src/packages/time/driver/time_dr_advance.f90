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
MODULE time_dr_advance_mod

  USE dataAPI_types_mod,  ONLY: runtime_t,timestep_t,time_t,dt_t,error_t,io_t, &
&                               sizes_t,data_t
  USE dataAPI_kinds_mod,  ONLY: ink,rlk
  USE dataAPI_params_mod, ONLY: SUCCESS,FAILURE,HALT_ALL
  USE dataAPI_id_mod,     ONLY: ielmatid,iellocglobid
  USE timerAPI_types_mod, ONLY: timer_t
  USE timerAPI_id_mod,    ONLY: TGETDTID,TCOLLECTIVETID
  USE timer_advance_mod,  ONLY: timer_start,timer_end
  USE typhon_API_mod,     ONLY: TYPH_Reduce_dt
  USE utils_kn_access_mod,ONLY: utils_kn_get

  IMPLICIT NONE

  PRIVATE :: time_dr_setmaterial,time_dr_setparallel
  PUBLIC  :: time_dr_calc,time_dr_end

CONTAINS

  SUBROUTINE time_dr_calc(time,timestep,timer,first,current)

    ! Argument list
    TYPE(time_t),            INTENT(IN)    :: time
    TYPE(timestep_t),        INTENT(IN)    :: timestep
    TYPE(timer_t),           INTENT(INOUT) :: timer
    TYPE(dt_t),      POINTER,INTENT(INOUT) :: current,first

    ! Timer
    CALL timer_start(timer)

    ! Maximum
    ALLOCATE(current)
    first=>current
    current%rdt=time%dt_max
    current%idt=-1_ink
    current%sdt=' MAXIMUM'

    ! Growth
    ALLOCATE(current%next)
    current=>current%next
    current%rdt=time%dt_g*timestep%dt
    current%idt=-1_ink
    current%sdt='  GROWTH'

    ! Initial
    IF (timestep%nstep.EQ.0_ink) THEN
      ALLOCATE(current%next)
      current=>current%next
      current%rdt=time%dt_initial
      current%idt=-1_ink
      current%sdt=' INITIAL'
    ENDIF

  END SUBROUTINE time_dr_calc  

  SUBROUTINE time_dr_end(time,runtime,timer,dh,current,next,error)

    ! Argument list
    TYPE(time_t),                 INTENT(IN)    :: time
    TYPE(runtime_t),              INTENT(INOUT) :: runtime
    TYPE(timer_t),   DIMENSION(:),INTENT(INOUT) :: timer
    TYPE(data_t),    DIMENSION(:),INTENT(IN)    :: dh
    TYPE(dt_t),      POINTER,     INTENT(INOUT) :: current,next
    TYPE(error_t),                INTENT(OUT)   :: error
    ! Local
    TYPE(dt_t) :: mindt

    ! Initialise
    error%ierr=SUCCESS

    ! Find smallest timestep
    mindt=current
    NULLIFY(next)
    next=>current%next
    search:DO
      DEALLOCATE(current)
      IF (.NOT.ASSOCIATED(next)) EXIT search
      current=>next
      IF (current%rdt.LT.mindt%rdt) mindt=current
      next=>current%next
    ENDDO search
    CALL time_dr_setmaterial(time%io,runtime%sizes,dh(ielmatid),mindt)
    IF (time%comm%nproc.GT.1_ink) THEN
      CALL time_dr_setparallel(runtime%sizes,dh(iellocglobid),mindt%idt)
      CALL timer_start(timer(TCOLLECTIVETID))
      error%ierr=TYPH_Reduce_dt(mindt,Comm=time%comm%comm)
      CALL timer_end(timer(TCOLLECTIVETID))
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to minimise dt"
        error%iout=HALT_ALL
        RETURN
      ENDIF
    ENDIF

    ! set runtime data
    runtime%timestep%nstep=runtime%timestep%nstep+1_ink
    runtime%timestep%dt=mindt%rdt
    runtime%timestep%sdt=mindt%sdt
    runtime%timestep%mdt=mindt%mdt
    runtime%timestep%idtel=mindt%idt
    runtime%timestep%time=runtime%timestep%time+runtime%timestep%dt

    ! Check minimum
    IF (runtime%timestep%dt.LT.time%dt_min) THEN
      error%ierr=FAILURE
      error%iout=HALT_ALL
      error%serr="ERROR: dt < dt_min"
      RETURN
    ENDIF

    ! Timing data
    CALL timer_end(timer(TGETDTID))

  END SUBROUTINE time_dr_end

  SUBROUTINE time_dr_setmaterial(io,sizes,mat,dt)

    ! Argument list
    TYPE(io_t),   INTENT(IN)    :: io
    TYPE(sizes_t),INTENT(IN)    :: sizes
    TYPE(data_t), INTENT(IN)    :: mat
    TYPE(dt_t),   INTENT(INOUT) :: dt
    ! Local
    INTEGER(KIND=ink) :: ii

    IF (dt%idt.GT.0_ink) THEN
      ii=utils_kn_get(dt%idt,sizes%nel,mat%iaddr)
      IF (ii.GT.0_ink) THEN
        dt%mdt=io%smaterial(ii)
      ELSE
        dt%mdt='     MIXED'
      ENDIF
    ELSE
      dt%mdt='   UNKNOWN'
    ENDIF

  END SUBROUTINE time_dr_setmaterial

  SUBROUTINE time_dr_setparallel(sizes,loc2glob,idt)

    ! Argument list
    TYPE(sizes_t),    INTENT(IN)    :: sizes
    TYPE(data_t),     INTENT(IN)    :: loc2glob
    INTEGER(KIND=ink),INTENT(INOUT) :: idt

    IF (idt.GT.0_ink) idt=utils_kn_get(idt,sizes%nel,loc2glob%iaddr)

  END SUBROUTINE time_dr_setparallel

END MODULE time_dr_advance_mod
