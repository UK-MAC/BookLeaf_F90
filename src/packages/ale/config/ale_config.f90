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
MODULE ale_cf_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk,lok
  USE dataAPI_params_mod,ONLY: OSTREAM,SUCCESS,FAILURE,HALT_SINGLE
  USE dataAPI_types_mod, ONLY: ale_t,error_t,global_t,eos_t,comm_t,time_t

  IMPLICIT NONE

  PUBLIC :: ale_cf_defaults,ale_cf_get,ale_cf_set,ale_cf_rationalise,          &
&           ale_cf_print

CONTAINS

  SUBROUTINE ale_cf_defaults(ale,error)

    ! Argument list
    TYPE(ale_t),  INTENT(OUT) :: ale
    TYPE(error_t),INTENT(OUT) :: error
    
    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set ALE defaults
    ale%npatch  =0_ink
    ale%adv_type=1_ink
    ale%mintime =HUGE(1.0_rlk)
    ale%maxtime =HUGE(1.0_rlk)
    ale%sf      =0.5_rlk
    ale%zexist  =.FALSE._lok
    ale%zon     =.FALSE._lok
    ale%zeul    =.FALSE._lok
    IF (ALLOCATED(ale%patch_type)) THEN
      DEALLOCATE(ale%patch_type,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(ale%patch_type(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    ale%patch_type(:)=0_ink
    IF (ALLOCATED(ale%patch_motion)) THEN
      DEALLOCATE(ale%patch_motion,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(ale%patch_motion(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    ale%patch_motion(:)=1_ink
    IF (ALLOCATED(ale%patch_ntrigger)) THEN
      DEALLOCATE(ale%patch_ntrigger,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(ale%patch_ntrigger(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    ale%patch_ntrigger(:)=0_ink
    IF (ALLOCATED(ale%patch_ontime)) THEN
      DEALLOCATE(ale%patch_ontime,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(ale%patch_ontime(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    ale%patch_ontime(:)=HUGE(1.0_rlk)
    IF (ALLOCATED(ale%patch_offtime)) THEN
      DEALLOCATE(ale%patch_offtime,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(ale%patch_offtime(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    ale%patch_offtime(:)=HUGE(1.0_rlk)
    IF (ALLOCATED(ale%patch_minvel)) THEN
      DEALLOCATE(ale%patch_minvel,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(ale%patch_minvel(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    ale%patch_minvel(:)=HUGE(1.0_rlk)
    IF (ALLOCATED(ale%patch_maxvel)) THEN
      DEALLOCATE(ale%patch_maxvel,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(ale%patch_maxvel(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    ale%patch_maxvel(:)=HUGE(1.0_rlk)
    IF (ALLOCATED(ale%patch_om)) THEN
      DEALLOCATE(ale%patch_om,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(ale%patch_om(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    ale%patch_om(:)=1.0_rlk
    IF (ALLOCATED(ale%patch_trigger)) THEN
      DEALLOCATE(ale%patch_trigger,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(ale%patch_trigger(1,1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    ale%patch_trigger(1,1)=-1_ink

    ! nullify pointers to other sections of config
    NULLIFY(ale%global,ale%comm,ale%eos)

  END SUBROUTINE ale_cf_defaults

  SUBROUTINE ale_cf_get(ale,npatch,adv_type,mintime,maxtime,sf,zexist,zon,zeul,&
&                       patch_type,patch_motion,patch_ntrigger,patch_trigger,  &
&                       patch_ontime,patch_offtime,patch_minvel,patch_maxvel,  &
&                       patch_om,error)

    ! Argument list
    TYPE(ale_t),                                          INTENT(IN)  :: ale
    INTEGER(KIND=ink),                           OPTIONAL,INTENT(OUT) ::       &
&    npatch,adv_type
    REAL(KIND=rlk),                              OPTIONAL,INTENT(OUT) :: sf,   &
&    mintime,maxtime
    LOGICAL(KIND=lok),                           OPTIONAL,INTENT(OUT) :: zon,  &
&    zeul,zexist
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,OPTIONAL,INTENT(OUT) ::       &
&    patch_type,patch_motion,patch_ntrigger
    REAL(KIND=rlk),   DIMENSION(:),  ALLOCATABLE,OPTIONAL,INTENT(OUT) ::       &
&    patch_ontime,patch_offtime,patch_minvel,patch_maxvel,patch_om
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,OPTIONAL,INTENT(OUT) ::       &
&    patch_trigger
    TYPE(error_t),                                        INTENT(OUT) :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! get
    IF (PRESENT(npatch))   npatch  =ale%npatch
    IF (PRESENT(adv_type)) adv_type=ale%adv_type
    IF (PRESENT(mintime))  mintime =ale%mintime
    IF (PRESENT(maxtime))  maxtime =ale%maxtime
    IF (PRESENT(sf))       sf      =ale%sf
    IF (PRESENT(zexist))   zexist  =ale%zexist
    IF (PRESENT(zon))      zon     =ale%zon
    IF (PRESENT(zeul))     zeul    =ale%zeul
    IF (PRESENT(patch_type)) THEN
      IF (ALLOCATED(patch_type)) THEN
        DEALLOCATE(patch_type,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate patch_type"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(patch_type(SIZE(ale%patch_type)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate patch_type"
        RETURN
      ENDIF
      patch_type(:)=ale%patch_type(:)
    ENDIF
    IF (PRESENT(patch_motion)) THEN
      IF (ALLOCATED(patch_motion)) THEN
        DEALLOCATE(patch_motion,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate patch_motion"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(patch_motion(SIZE(ale%patch_motion)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate patch_motion"
        RETURN
      ENDIF
      patch_motion(:)=ale%patch_motion(:)
    ENDIF
    IF (PRESENT(patch_ntrigger)) THEN
      IF (ALLOCATED(patch_ntrigger)) THEN
        DEALLOCATE(patch_ntrigger,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate patch_ntrigger"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(patch_ntrigger(SIZE(ale%patch_ntrigger)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate patch_ntrigger"
        RETURN
      ENDIF
      patch_ntrigger(:)=ale%patch_ntrigger(:)
    ENDIF
    IF (PRESENT(patch_ontime)) THEN
      IF (ALLOCATED(patch_ontime)) THEN
        DEALLOCATE(patch_ontime,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate patch_ontime"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(patch_ontime(SIZE(ale%patch_ontime)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate patch_ontime"
        RETURN
      ENDIF
      patch_ontime(:)=ale%patch_ontime(:)
    ENDIF
    IF (PRESENT(patch_offtime)) THEN
      IF (ALLOCATED(patch_offtime)) THEN
        DEALLOCATE(patch_offtime,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate patch_offtime"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(patch_offtime(SIZE(ale%patch_offtime)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate patch_offtime"
        RETURN
      ENDIF
      patch_offtime(:)=ale%patch_offtime(:)
    ENDIF
    IF (PRESENT(patch_minvel)) THEN
      IF (ALLOCATED(patch_minvel)) THEN
        DEALLOCATE(patch_minvel,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate patch_minvel"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(patch_minvel(SIZE(ale%patch_minvel)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate patch_minvel"
        RETURN
      ENDIF
      patch_minvel(:)=ale%patch_minvel(:)
    ENDIF
    IF (PRESENT(patch_maxvel)) THEN
      IF (ALLOCATED(patch_maxvel)) THEN
        DEALLOCATE(patch_maxvel,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate patch_maxvel"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(patch_maxvel(SIZE(ale%patch_maxvel)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate patch_maxvel"
        RETURN
      ENDIF
      patch_maxvel(:)=ale%patch_maxvel(:)
    ENDIF
    IF (PRESENT(patch_om)) THEN
      IF (ALLOCATED(patch_om)) THEN
        DEALLOCATE(patch_om,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate patch_om"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(patch_om(SIZE(ale%patch_om)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate patch_om"
        RETURN
      ENDIF
      patch_om(:)=ale%patch_om(:)
    ENDIF
    IF (PRESENT(patch_trigger)) THEN
      IF (ALLOCATED(patch_trigger)) THEN
        DEALLOCATE(patch_trigger,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate patch_trigger"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(patch_trigger(SIZE(ale%patch_trigger,DIM=1),                    &
&                            SIZE(ale%patch_trigger,DIM=2)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate patch_trigger"
        RETURN
      ENDIF
      patch_trigger(:,:)=ale%patch_trigger(:,:)
    ENDIF

  END SUBROUTINE ale_cf_get

  SUBROUTINE ale_cf_set(ale,npatch,adv_type,mintime,maxtime,sf,zexist,zon,zeul,&
&                       patch_type,patch_motion,patch_ntrigger,patch_trigger,  &
&                       patch_ontime,patch_offtime,patch_minvel,patch_maxvel,  &
&                       patch_om,error)

    ! Argument list
    TYPE(ale_t),                                          INTENT(INOUT) :: ale
    INTEGER(KIND=ink),                           OPTIONAL,INTENT(IN)    ::     &
&    npatch,adv_type
    REAL(KIND=rlk),                              OPTIONAL,INTENT(IN)    :: sf, &
&    mintime,maxtime
    LOGICAL(KIND=lok),                           OPTIONAL,INTENT(IN)    :: zon,&
&    zeul,zexist
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE,OPTIONAL,INTENT(INOUT) ::     &
&    patch_type,patch_motion,patch_ntrigger
    REAL(KIND=rlk),   DIMENSION(:),  ALLOCATABLE,OPTIONAL,INTENT(INOUT) ::     &
&    patch_ontime,patch_offtime,patch_minvel,patch_maxvel,patch_om
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE,OPTIONAL,INTENT(INOUT) ::     &
&    patch_trigger
    TYPE(error_t),                                        INTENT(OUT)   ::     &
&    error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set
    IF (PRESENT(npatch))   ale%npatch  =npatch
    IF (PRESENT(adv_type)) ale%adv_type=adv_type
    IF (PRESENT(mintime))  ale%mintime =mintime
    IF (PRESENT(maxtime))  ale%maxtime =maxtime
    IF (PRESENT(sf))       ale%sf      =sf
    IF (PRESENT(zexist))   ale%zexist  =zexist
    IF (PRESENT(zon))      ale%zon     =zon
    IF (PRESENT(zeul))     ale%zeul    =zeul
    IF (PRESENT(patch_type)) THEN
      IF (ALLOCATED(ale%patch_type)) THEN
        DEALLOCATE(ale%patch_type,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate ale%patch_type"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(ale%patch_type(SIZE(patch_type)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate ale%patch_type"
        RETURN
      ENDIF
      ale%patch_type(:)=patch_type(:)
      DEALLOCATE(patch_type,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate patch_type"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(patch_motion)) THEN
      IF (ALLOCATED(ale%patch_motion)) THEN
        DEALLOCATE(ale%patch_motion,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate ale%patch_motion"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(ale%patch_motion(SIZE(patch_motion)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate ale%patch_motion"
        RETURN
      ENDIF
      ale%patch_motion(:)=patch_motion(:)
      DEALLOCATE(patch_motion,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate patch_motion"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(patch_ntrigger)) THEN
      IF (ALLOCATED(ale%patch_ntrigger)) THEN
        DEALLOCATE(ale%patch_ntrigger,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate ale%patch_ntrigger"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(ale%patch_ntrigger(SIZE(patch_ntrigger)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate ale%patch_ntrigger"
        RETURN
      ENDIF
      ale%patch_ntrigger(:)=patch_ntrigger(:)
      DEALLOCATE(patch_ntrigger,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate patch_ntrigger"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(patch_ontime)) THEN
      IF (ALLOCATED(ale%patch_ontime)) THEN
        DEALLOCATE(ale%patch_ontime,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate ale%patch_ontime"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(ale%patch_ontime(SIZE(patch_ontime)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate ale%patch_ontime"
        RETURN
      ENDIF
      ale%patch_ontime(:)=patch_ontime(:)
      DEALLOCATE(patch_ontime,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate patch_ontime"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(patch_offtime)) THEN
      IF (ALLOCATED(ale%patch_offtime)) THEN
        DEALLOCATE(ale%patch_offtime,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate ale%patch_offtime"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(ale%patch_offtime(SIZE(patch_offtime)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate ale%patch_offtime"
        RETURN
      ENDIF
      ale%patch_offtime(:)=patch_offtime(:)
      DEALLOCATE(patch_offtime,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate patch_offtime"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(patch_minvel)) THEN
      IF (ALLOCATED(ale%patch_minvel)) THEN
        DEALLOCATE(ale%patch_minvel,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate ale%patch_minvel"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(ale%patch_minvel(SIZE(patch_minvel)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate ale%patch_minvel"
        RETURN
      ENDIF
      ale%patch_minvel(:)=patch_minvel(:)
      DEALLOCATE(patch_minvel,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate patch_minvel"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(patch_maxvel)) THEN
      IF (ALLOCATED(ale%patch_maxvel)) THEN
        DEALLOCATE(ale%patch_maxvel,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate ale%patch_maxvel"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(ale%patch_maxvel(SIZE(patch_maxvel)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate ale%patch_maxvel"
        RETURN
      ENDIF
      ale%patch_maxvel(:)=patch_maxvel(:)
      DEALLOCATE(patch_maxvel,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate patch_maxvel"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(patch_om)) THEN
      IF (ALLOCATED(ale%patch_om)) THEN
        DEALLOCATE(ale%patch_om,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate ale%patch_om"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(ale%patch_om(SIZE(patch_om)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate ale%patch_om"
        RETURN
      ENDIF
      ale%patch_om(:)=patch_om(:)
      DEALLOCATE(patch_om,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate patch_om"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(patch_trigger)) THEN
      IF (ALLOCATED(ale%patch_trigger)) THEN
        DEALLOCATE(ale%patch_trigger,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate ale%patch_trigger"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(ale%patch_trigger(SIZE(patch_trigger,DIM=1),                    &
&                                SIZE(patch_trigger,DIM=2)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate ale%patch_trigger"
        RETURN
      ENDIF
      ale%patch_trigger(:,:)=patch_trigger(:,:)
      DEALLOCATE(patch_trigger,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate patch_trigger"
        RETURN
      ENDIF
    ENDIF

  END SUBROUTINE ale_cf_set

  SUBROUTINE ale_cf_rationalise(ale,time,global,comm,eos,error)

    ! Argument list
    TYPE(ale_t),           INTENT(INOUT) :: ale
    TYPE(time_t),          INTENT(IN)    :: time
    TYPE(global_t),POINTER,INTENT(IN)    :: global
    TYPE(comm_t),  POINTER,INTENT(IN)    :: comm
    TYPE(eos_t),   POINTER,INTENT(IN)    :: eos
    TYPE(error_t),         INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink)                            :: ntrigger
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: patch_type,patch_motion,   &
&                                                   patch_ntrigger
    INTEGER(KIND=ink),DIMENSION(:,:),ALLOCATABLE :: patch_trigger
    REAL(KIND=rlk),   DIMENSION(:),  ALLOCATABLE :: patch_ontime,patch_offtime,&
&                                                   patch_minvel,patch_maxvel, &
&                                                   patch_om

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! rationalise
    IF ((ale%npatch.LT.0_ink).AND.(.NOT.ale%zeul)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: npatch < 0"
      RETURN
    ENDIF
    IF (ale%zeul) THEN
      ale%npatch=1_ink
      ale%mintime=-HUGE(1.0_rlk)
      ale%maxtime=HUGE(1.0_rlk)
      ale%zexist=.TRUE._lok
      DEALLOCATE(ale%patch_type,ale%patch_motion,ale%patch_ntrigger,           &
&                ale%patch_trigger,ale%patch_ontime,ale%patch_offtime,         &
&                ale%patch_minvel,ale%patch_maxvel,ale%patch_om,               &
&                STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate ALE configuration"
        RETURN
      ENDIF
    ELSE
      IF (ale%npatch.GT.0_ink) THEN
        IF (.NOT.ALLOCATED(ale%patch_type)) THEN
          error%ierr=FAILURE
          error%serr="ERROR: patch_type not allocated"
          RETURN
        ENDIF
        IF (SIZE(ale%patch_type).LT.ale%npatch) THEN
          error%ierr=FAILURE
          error%serr="ERROR: inconsistent no. patches for ALE"
          RETURN
        ELSEIF (SIZE(ale%patch_type).GT.ale%npatch) THEN
          ALLOCATE(patch_type(ale%npatch),STAT=error%ierr)
          IF (error%ierr.NE.SUCCESS) THEN
            error%serr="ERROR: failed to allocate patch_type"
            RETURN
          ENDIF
          patch_type(:)=ale%patch_type(1:ale%npatch)
          CALL MOVE_ALLOC(FROM=patch_type,TO=ale%patch_type)
        ENDIF
        IF (.NOT.ALLOCATED(ale%patch_motion)) THEN
          error%ierr=FAILURE
          error%serr="ERROR: patch_motion not allocated"
          RETURN
        ENDIF
        IF (SIZE(ale%patch_motion).LT.ale%npatch) THEN
          error%ierr=FAILURE
          error%serr="ERROR: inconsistent no. patches for ALE"
          RETURN
        ELSEIF (SIZE(ale%patch_motion).GT.ale%npatch) THEN
          ALLOCATE(patch_motion(ale%npatch),STAT=error%ierr)
          IF (error%ierr.NE.SUCCESS) THEN
            error%serr="ERROR: failed to allocate patch_motion"
            RETURN
          ENDIF
          patch_motion(:)=ale%patch_motion(1:ale%npatch)
          CALL MOVE_ALLOC(FROM=patch_motion,TO=ale%patch_motion)
        ENDIF
        IF (.NOT.ALLOCATED(ale%patch_ntrigger)) THEN
          error%ierr=FAILURE
          error%serr="ERROR: patch_ntrigger not allocated"
          RETURN
        ENDIF
        IF (SIZE(ale%patch_ntrigger).LT.ale%npatch) THEN
          error%ierr=FAILURE
          error%serr="ERROR: inconsistent no. patches for ALE"
          RETURN
        ELSEIF (SIZE(ale%patch_ntrigger).GT.ale%npatch) THEN
          ALLOCATE(patch_ntrigger(ale%npatch),STAT=error%ierr)
          IF (error%ierr.NE.SUCCESS) THEN
            error%serr="ERROR: failed to allocate patch_ntrigger"
            RETURN
          ENDIF
          patch_ntrigger(:)=ale%patch_ntrigger(1:ale%npatch)
          CALL MOVE_ALLOC(FROM=patch_ntrigger,TO=ale%patch_ntrigger)
        ENDIF
        IF (.NOT.ALLOCATED(ale%patch_ontime)) THEN
          error%ierr=FAILURE
          error%serr="ERROR: patch_ontime not allocated"
          RETURN
        ENDIF
        IF (SIZE(ale%patch_ontime).LT.ale%npatch) THEN
          error%ierr=FAILURE
          error%serr="ERROR: inconsistent no. patches for ALE"
          RETURN
        ELSEIF (SIZE(ale%patch_ontime).GT.ale%npatch) THEN
          ALLOCATE(patch_ontime(ale%npatch),STAT=error%ierr)
          IF (error%ierr.NE.SUCCESS) THEN
            error%serr="ERROR: failed to allocate patch_ontime"
            RETURN
          ENDIF
          patch_ontime(:)=ale%patch_ontime(1:ale%npatch)
          CALL MOVE_ALLOC(FROM=patch_ontime,TO=ale%patch_ontime)
        ENDIF
        IF (.NOT.ALLOCATED(ale%patch_offtime)) THEN
          error%ierr=FAILURE
          error%serr="ERROR: patch_offtime not allocated"
          RETURN
        ENDIF
        IF (SIZE(ale%patch_offtime).LT.ale%npatch) THEN
          error%ierr=FAILURE
          error%serr="ERROR: inconsistent no. patches for ALE"
          RETURN
        ELSEIF (SIZE(ale%patch_offtime).GT.ale%npatch) THEN
          ALLOCATE(patch_offtime(ale%npatch),STAT=error%ierr)
          IF (error%ierr.NE.SUCCESS) THEN
            error%serr="ERROR: failed to allocate patch_offtime"
            RETURN
          ENDIF
          patch_offtime(:)=ale%patch_offtime(1:ale%npatch)
          CALL MOVE_ALLOC(FROM=patch_offtime,TO=ale%patch_offtime)
        ENDIF
        IF (.NOT.ALLOCATED(ale%patch_om)) THEN
          error%ierr=FAILURE
          error%serr="ERROR: patch_om not allocated"
          RETURN
        ENDIF
        IF (SIZE(ale%patch_om).LT.ale%npatch) THEN
          error%ierr=FAILURE
          error%serr="ERROR: inconsistent no. patches for ALE"
          RETURN
        ELSEIF (SIZE(ale%patch_om).GT.ale%npatch) THEN
          ALLOCATE(patch_om(ale%npatch),STAT=error%ierr)
          IF (error%ierr.NE.SUCCESS) THEN
            error%serr="ERROR: failed to allocate patch_om"
            RETURN
          ENDIF
          patch_om(:)=ale%patch_om(1:ale%npatch)
          CALL MOVE_ALLOC(FROM=patch_om,TO=ale%patch_om)
        ENDIF
        IF (.NOT.ALLOCATED(ale%patch_minvel)) THEN
          error%ierr=FAILURE
          error%serr="ERROR: patch_minvel not allocated"
          RETURN
        ENDIF
        IF (SIZE(ale%patch_minvel).LT.ale%npatch) THEN
          error%ierr=FAILURE
          error%serr="ERROR: inconsistent no. patches for ALE"
          RETURN
        ELSEIF (SIZE(ale%patch_minvel).GT.ale%npatch) THEN
          ALLOCATE(patch_minvel(ale%npatch),STAT=error%ierr)
          IF (error%ierr.NE.SUCCESS) THEN
            error%serr="ERROR: failed to allocate patch_minvel"
            RETURN
          ENDIF
          patch_minvel(:)=ale%patch_minvel(1:ale%npatch)
          CALL MOVE_ALLOC(FROM=patch_minvel,TO=ale%patch_minvel)
        ENDIF
        IF (.NOT.ALLOCATED(ale%patch_maxvel)) THEN
          error%ierr=FAILURE
          error%serr="ERROR: patch_maxvel not allocated"
          RETURN
        ENDIF
        IF (SIZE(ale%patch_maxvel).LT.ale%npatch) THEN
          error%ierr=FAILURE
          error%serr="ERROR: inconsistent no. patches for ALE"
          RETURN
        ELSEIF (SIZE(ale%patch_maxvel).GT.ale%npatch) THEN
          ALLOCATE(patch_maxvel(ale%npatch),STAT=error%ierr)
          IF (error%ierr.NE.SUCCESS) THEN
            error%serr="ERROR: failed to allocate patch_maxvel"
            RETURN
          ENDIF
          patch_maxvel(:)=ale%patch_maxvel(1:ale%npatch)
          CALL MOVE_ALLOC(FROM=patch_maxvel,TO=ale%patch_maxvel)
        ENDIF
        IF (.NOT.ALLOCATED(ale%patch_trigger)) THEN
          error%ierr=FAILURE
          error%serr="ERROR: patch_trigger not allocated"
          RETURN
        ENDIF
        ntrigger=MAXVAL(ale%patch_ntrigger(1:ale%npatch))
        IF (SIZE(ale%patch_trigger,DIM=1).LT.ntrigger) THEN
          error%ierr=FAILURE
          error%serr="ERROR: inconsistent no. triggers for ALE"
          RETURN
        ENDIF
        IF (SIZE(ale%patch_trigger,DIM=2).LT.ale%npatch) THEN
          error%ierr=FAILURE
          error%serr="ERROR: inconsistent no. patches for ALE"
          RETURN
        ENDIF
        IF ((SIZE(ale%patch_trigger,DIM=1).GT.ntrigger).OR.                    &
&           (SIZE(ale%patch_trigger,DIM=2).GT.ale%npatch)) THEN
          ALLOCATE(patch_trigger(ntrigger,ale%npatch),STAT=error%ierr)
          IF (error%ierr.NE.SUCCESS) THEN
            error%serr="ERROR: failed to allocate patch_trigger"
            RETURN
          ENDIF
          patch_trigger(:,:)=ale%patch_trigger(1:ntrigger,1:ale%npatch)
          CALL MOVE_ALLOC(FROM=patch_trigger,TO=ale%patch_trigger)
        ENDIF
        ale%mintime=MINVAL(ale%patch_ontime(:))
        ale%maxtime=MAXVAL(ale%patch_offtime(:))
        ale%zexist=(ale%mintime.LT.time%time_start).AND.                       &
&                  (ale%maxtime.GT.time%time_end)
        ale%zexist=ale%mintime.LT.ale%maxtime
        IF (.NOT.ale%zexist) THEN
          DEALLOCATE(ale%patch_type,ale%patch_motion,ale%patch_ntrigger,       &
&                    ale%patch_trigger,ale%patch_ontime,ale%patch_offtime,     &
&                    ale%patch_minvel,ale%patch_maxvel,ale%patch_om,           &
&                    STAT=error%ierr)
          IF (error%ierr.NE.SUCCESS) THEN
            error%serr="ERROR: failed to deallocate ALE configuration"
            RETURN
          ENDIF
        ENDIF
      ELSE
        ale%zexist=.FALSE._lok
        DEALLOCATE(ale%patch_type,ale%patch_motion,ale%patch_ntrigger,         &
&                  ale%patch_trigger,ale%patch_ontime,ale%patch_offtime,       &
&                  ale%patch_minvel,ale%patch_maxvel,ale%patch_om,             &
&                  STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate ALE configuration"
          RETURN
        ENDIF
      ENDIF
    ENDIF
    NULLIFY(ale%global)
    ale%global=>global
    NULLIFY(ale%comm)
    ale%comm=>comm
    NULLIFY(ale%eos)
    ale%eos=>eos

  END SUBROUTINE ale_cf_rationalise

  SUBROUTINE ale_cf_print(ale)

    ! Argument list
    TYPE(ale_t),INTENT(IN) :: ale

    WRITE(OSTREAM,'(a12)') ' ALE OPTIONS'
    IF (ale%zexist) THEN
      WRITE(OSTREAM,'(a83,44X,a5)') '  ALE exists:                            '&
&      //'                                   zexist ',' TRUE'
      IF (ale%zeul) THEN
        WRITE(OSTREAM,'(a83,44X,a5)') '  Eulerian frame selected:             '&
&        //'                                       zeul ',' TRUE'
      ELSE
        ! other options not yet implemented
      ENDIF
      IF (ale%zon) THEN
        WRITE(OSTREAM,'(a83,44X,a5)') '  ALE currently active:                '&
&        //'                                        zon ',' TRUE'
      ELSE
        WRITE(OSTREAM,'(a83,44X,a5)') '  ALE currently active:                '&
&        //'                                        zon ','FALSE'
      ENDIF
      IF (ale%adv_type.EQ.1_ink) THEN
        WRITE(OSTREAM,'(a83,33X,i16)') '  Unsplit advection:                  '&
&        //'                                    adv_type ',ale%adv_type
      ELSEIF (ale%adv_type.EQ.2_ink) THEN
        WRITE(OSTREAM,'(a83,33X,i16)') '  Split advection:                    '&
&        //'                                    adv_type ',ale%adv_type
      ENDIF
      WRITE(OSTREAM,'(a83,33X,e16.9)') '  ALE timestep safety factor:         '&
&      //'                                          sf ',ale%sf
    ELSE
      WRITE(OSTREAM,'(a83,44X,a5)') '  ALE exists:                            '&
&      //'                                    zexist ','FALSE'
    ENDIF
    WRITE(OSTREAM,'(a132)')' #################################################'&
&    //'######################################################################'&
&    //'############'

  END SUBROUTINE ale_cf_print

END MODULE ale_cf_mod
