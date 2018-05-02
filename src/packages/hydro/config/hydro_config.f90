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
MODULE hydro_cf_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk,lok
  USE dataAPI_types_mod, ONLY: hydro_t,comm_t,eos_t,global_t,io_t,sizes_t,     &
&                              error_t
  USE dataAPI_params_mod,ONLY: SUCCESS,FAILURE,HALT_SINGLE,OSTREAM

  IMPLICIT NONE

  PUBLIC :: hydro_cf_defaults,hydro_cf_get,hydro_cf_set,hydro_cf_rationalise,  &
&           hydro_cf_print

CONTAINS

  SUBROUTINE hydro_cf_defaults(hydro,error)

    ! Argument list
    TYPE(hydro_t),INTENT(OUT) :: hydro
    TYPE(error_t),INTENT(OUT) :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set Hydro defaults
    hydro%cvisc1=0.5_rlk
    hydro%cvisc2=0.75_rlk
    hydro%cfl_sf=0.5_rlk
    hydro%div_sf=0.25_rlk
    hydro%zhg=.FALSE._lok
    hydro%zsp=.FALSE._lok
    hydro%ztq=.FALSE._lok
    IF (ALLOCATED(hydro%kappareg)) THEN
      DEALLOCATE(hydro%kappareg,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(hydro%kappareg(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    hydro%kappareg(1)=0.0_rlk
    IF (ALLOCATED(hydro%pmeritreg)) THEN
      DEALLOCATE(hydro%pmeritreg,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(hydro%pmeritreg(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    hydro%pmeritreg(1)=0.0_rlk
    IF (ALLOCATED(hydro%zdtnotreg)) THEN
      DEALLOCATE(hydro%zdtnotreg,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(hydro%zdtnotreg(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    hydro%zdtnotreg(1)=.FALSE._lok
    IF (ALLOCATED(hydro%zmidlength)) THEN
      DEALLOCATE(hydro%zmidlength,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(hydro%zmidlength(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    hydro%zmidlength(1)=.FALSE._lok

    ! nullify pointers to other sections of config
    NULLIFY(hydro%global,hydro%comm,hydro%eos,hydro%io)

  END SUBROUTINE hydro_cf_defaults

  SUBROUTINE hydro_cf_get(hydro,cvisc1,cvisc2,cfl_sf,div_sf,kappareg,pmeritreg,&
&                         ztq,zhg,zsp,zmidlength,zdtnotreg,error)

    ! Argument list
    TYPE(hydro_t),                                      INTENT(IN)  :: hydro
    REAL(KIND=rlk),                            OPTIONAL,INTENT(OUT) :: cvisc1, &
&                                                                      cvisc2, &
&                                                                      cfl_sf, &
&                                                                      div_sf
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE,OPTIONAL,INTENT(OUT) :: kappareg&
&                                                                    ,pmeritreg
    LOGICAL(KIND=lok),                         OPTIONAL,INTENT(OUT) :: zhg,zsp,&
&                                                                      ztq
    LOGICAL(KIND=lok),DIMENSION(:),ALLOCATABLE,OPTIONAL,INTENT(OUT) ::         &
&                                                                   zmidlength,&
&                                                                     zdtnotreg
    TYPE(error_t),                                      INTENT(OUT) :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! get
    IF (PRESENT(cvisc1)) cvisc1=hydro%cvisc1
    IF (PRESENT(cvisc2)) cvisc2=hydro%cvisc2
    IF (PRESENT(cfl_sf)) cfl_sf=hydro%cfl_sf
    IF (PRESENT(div_sf)) div_sf=hydro%div_sf
    IF (PRESENT(ztq))    ztq   =hydro%ztq
    IF (PRESENT(zhg))    zhg   =hydro%zhg
    IF (PRESENT(zsp))    zsp   =hydro%zsp
    IF (PRESENT(kappareg)) THEN
      IF (ALLOCATED(kappareg)) THEN
        DEALLOCATE(kappareg,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate kappareg"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(kappareg(SIZE(hydro%kappareg)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate kappareg"
        RETURN
      ENDIF
      kappareg(:)=hydro%kappareg(:)
    ENDIF
    IF (PRESENT(pmeritreg)) THEN
      IF (ALLOCATED(pmeritreg)) THEN
        DEALLOCATE(pmeritreg,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate pmeritreg"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(pmeritreg(SIZE(hydro%pmeritreg)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate pmeritreg"
        RETURN
      ENDIF
      pmeritreg(:)=hydro%pmeritreg(:)
    ENDIF
    IF (PRESENT(zhg)) zhg=hydro%zhg
    IF (PRESENT(zsp)) zsp=hydro%zsp
    IF (PRESENT(zdtnotreg)) THEN
      IF (ALLOCATED(zdtnotreg)) THEN
        DEALLOCATE(zdtnotreg,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate zdtnotreg"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(zdtnotreg(SIZE(hydro%zdtnotreg)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate zdtnotreg"
        RETURN
      ENDIF
      zdtnotreg(:)=hydro%zdtnotreg(:)
    ENDIF
    IF (PRESENT(zmidlength)) THEN
      IF (ALLOCATED(zmidlength)) THEN
        DEALLOCATE(zmidlength,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate zmidlength"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(zmidlength(SIZE(hydro%zmidlength)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate zmidlength"
        RETURN
      ENDIF
      zmidlength(:)=hydro%zmidlength(:)
    ENDIF

  END SUBROUTINE hydro_cf_get

  SUBROUTINE hydro_cf_set(hydro,cvisc1,cvisc2,cfl_sf,div_sf,kappareg,pmeritreg,&
&                         ztq,zhg,zsp,zmidlength,zdtnotreg,error)

    ! Argument list
    TYPE(hydro_t),                                      INTENT(INOUT) :: hydro
    REAL(KIND=rlk),                            OPTIONAL,INTENT(IN)    :: cvisc1,&
&                                                                        cvisc2,&
&                                                                        cfl_sf,&
&                                                                        div_sf
    LOGICAL(KIND=lok),                         OPTIONAL,INTENT(IN)    :: zhg,  &
&                                                                        zsp,  &
&                                                                        ztq
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE,OPTIONAL,INTENT(INOUT) :: kappareg,&
&                                                                        pmeritreg
    LOGICAL(KIND=lok),DIMENSION(:),ALLOCATABLE,OPTIONAL,INTENT(INOUT) :: zmidlength,&
&                                                                        zdtnotreg
    TYPE(error_t),                                      INTENT(OUT)   :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set
    IF (PRESENT(cvisc1)) hydro%cvisc1=cvisc1
    IF (PRESENT(cvisc2)) hydro%cvisc2=cvisc2
    IF (PRESENT(cfl_sf)) hydro%cfl_sf=cfl_sf
    IF (PRESENT(div_sf)) hydro%div_sf=div_sf
    IF (PRESENT(ztq))    hydro%ztq   =ztq
    IF (PRESENT(zhg))    hydro%zhg   =zhg
    IF (PRESENT(zsp))    hydro%zsp   =zsp
    IF (PRESENT(kappareg)) THEN
      IF (ALLOCATED(hydro%kappareg)) THEN
        DEALLOCATE(hydro%kappareg,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate hydro%kappareg"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(hydro%kappareg(SIZE(kappareg)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate hydro%kappareg"
        RETURN
      ENDIF
      hydro%kappareg(:)=kappareg(:)
      DEALLOCATE(kappareg,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate kappareg"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(pmeritreg)) THEN
      IF (ALLOCATED(hydro%pmeritreg)) THEN
        DEALLOCATE(hydro%pmeritreg,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate hydro%pmeritreg"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(hydro%pmeritreg(SIZE(pmeritreg)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate hydro%pmeritreg"
        RETURN
      ENDIF
      hydro%pmeritreg(:)=pmeritreg(:)
      DEALLOCATE(pmeritreg,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate pmeritreg"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(zmidlength)) THEN
      IF (ALLOCATED(hydro%zmidlength)) THEN
        DEALLOCATE(hydro%zmidlength,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate hydro%zmidlength"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(hydro%zmidlength(SIZE(zmidlength)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate hydro%zmidlength"
        RETURN
      ENDIF
      hydro%zmidlength(:)=zmidlength(:)
      DEALLOCATE(zmidlength,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate zmidlength"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(zdtnotreg)) THEN
      IF (ALLOCATED(hydro%zdtnotreg)) THEN
        DEALLOCATE(hydro%zdtnotreg,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate hydro%zdtnotreg"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(hydro%zdtnotreg(SIZE(zdtnotreg)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate hydro%zdtnotreg"
        RETURN
      ENDIF
      hydro%zdtnotreg(:)=zdtnotreg(:)
      DEALLOCATE(zdtnotreg,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate zdtnotreg"
        RETURN
      ENDIF
    ENDIF

  END SUBROUTINE hydro_cf_set

  SUBROUTINE hydro_cf_rationalise(hydro,comm,eos,io,global,sizes,error)

    ! Argument list
    TYPE(hydro_t),         INTENT(INOUT) :: hydro
    TYPE(comm_t),  POINTER,INTENT(IN)    :: comm
    TYPE(eos_t),   POINTER,INTENT(IN)    :: eos
    TYPE(io_t),    POINTER,INTENT(IN)    :: io
    TYPE(global_t),POINTER,INTENT(IN)    :: global
    TYPE(sizes_t),         INTENT(IN)    :: sizes
    TYPE(error_t),         INTENT(OUT)   :: error
    ! Local
    REAL(KIND=rlk),   DIMENSION(:),ALLOCATABLE :: kappareg,pmeritreg
    LOGICAL(KIND=lok),DIMENSION(:),ALLOCATABLE :: zdtnotreg,zmidlength

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! rationalise
    IF (hydro%cvisc1.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: cvisc1 < 0"
      RETURN
    ENDIF
    IF (hydro%cvisc2.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: cvisc2 < 0"
      RETURN
    ENDIF
    IF (hydro%cfl_sf.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: cfl_sf < 0"
      RETURN
    ENDIF
    IF (hydro%div_sf.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: div_sf < 0"
      RETURN
    ENDIF
    IF (.NOT.ALLOCATED(hydro%kappareg)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: kappareg not allocated"
      RETURN
    ENDIF
    IF (SIZE(hydro%kappareg).LT.sizes%nreg) THEN
      error%ierr=FAILURE
      error%serr="ERROR: inconsistent no. regions for hourglass control"
      RETURN
    ELSEIF (SIZE(hydro%kappareg).GT.sizes%nreg) THEN
      ALLOCATE(kappareg(sizes%nreg),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate kappareg"
        RETURN
      ENDIF
      kappareg(:)=hydro%kappareg(1:sizes%nreg)
      CALL MOVE_ALLOC(FROM=kappareg,TO=hydro%kappareg)
    ENDIF
    hydro%zhg=ANY(hydro%kappareg.GT.0.0_rlk)
    IF (.NOT.ALLOCATED(hydro%pmeritreg)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: pmeritreg not allocated"
      RETURN
    ENDIF
    IF (SIZE(hydro%pmeritreg).LT.sizes%nreg) THEN
      error%ierr=FAILURE
      error%serr="ERROR: inconsistent no. regions for hourglass control"
      RETURN
    ELSEIF (SIZE(hydro%pmeritreg).GT.sizes%nreg) THEN
      ALLOCATE(pmeritreg(sizes%nreg),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate pmeritreg"
        RETURN
      ENDIF
      pmeritreg(:)=hydro%pmeritreg(1:sizes%nreg)
      CALL MOVE_ALLOC(FROM=pmeritreg,TO=hydro%pmeritreg)
    ENDIF
    hydro%zsp=ANY(hydro%pmeritreg.GT.0.0_rlk)
    IF (.NOT.ALLOCATED(hydro%zdtnotreg)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: zdtnotreg not allocated"
      RETURN
    ENDIF
    IF (SIZE(hydro%zdtnotreg).LT.sizes%nreg) THEN
      error%ierr=FAILURE
      error%serr="ERROR: inconsistent no. regions for timestep"
      RETURN
    ELSEIF (SIZE(hydro%zdtnotreg).GT.sizes%nreg) THEN
      ALLOCATE(zdtnotreg(sizes%nreg),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate zdtnotreg"
        RETURN
      ENDIF
      zdtnotreg(:)=hydro%zdtnotreg(1:sizes%nreg)
      CALL MOVE_ALLOC(FROM=zdtnotreg,TO=hydro%zdtnotreg)
    ENDIF
    IF (.NOT.ALLOCATED(hydro%zmidlength)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: zmidlength not allocated"
      RETURN
    ENDIF
    IF (SIZE(hydro%zmidlength).LT.sizes%nreg) THEN
      error%ierr=FAILURE
      error%serr="ERROR: inconsistent no. regions for timestep"
      RETURN
    ELSEIF (SIZE(hydro%zmidlength).GT.sizes%nreg) THEN
      ALLOCATE(zmidlength(sizes%nreg),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate zmidlength"
        RETURN
      ENDIF
      zmidlength(:)=hydro%zmidlength(1:sizes%nreg)
      CALL MOVE_ALLOC(FROM=zmidlength,TO=hydro%zmidlength)
    ENDIF
    NULLIFY(hydro%global)
    hydro%global=>global
    NULLIFY(hydro%comm)
    hydro%comm=>comm
    NULLIFY(hydro%eos)
    hydro%eos=>eos
    NULLIFY(hydro%io)
    hydro%io=>io

  END SUBROUTINE hydro_cf_rationalise

  SUBROUTINE hydro_cf_print(hydro)

    ! Argument list
    TYPE(hydro_t),INTENT(IN) :: hydro
    ! Local
    INTEGER(KIND=ink) :: ii

    WRITE(OSTREAM,'(a14)') ' HYDRO OPTIONS'
    IF (hydro%ztq) THEN
      WRITE(OSTREAM,'(a83,43X,a6)')  '  Artificial viscosity type:            '&
&      //'                                      visc ','TENSOR'
    ELSE
      WRITE(OSTREAM,'(a83,43X,a6)')  '  Artificial viscosity type:            '&
&      //'                                      visc ','  EDGE'
    ENDIF
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Linear artificial viscosity '          &
&    //'coefficient:                                  cvisc1 ',hydro%cvisc1
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Quadratic artificial viscosity '       &
&    //'coefficient:                               cvisc2 ',hydro%cvisc2
    IF (hydro%zhg) THEN
      WRITE(OSTREAM,'(a83,44X,a5)')  '  Hourglass filters:              '      &
&      //'                                             zhg ',' TRUE'
    ELSE
      WRITE(OSTREAM,'(a83,44X,a5)')  '  Hourglass filters:              '      &
&      //'                                             zhg ','FALSE'
    ENDIF
    IF (hydro%zsp) THEN
      WRITE(OSTREAM,'(a83,44X,a5)')  '  Sub-zonal pressures:            '      &
&      //'                                             zsp ',' TRUE'
    ELSE
      WRITE(OSTREAM,'(a83,44X,a5)')  '  Sub-zonal pressures:            '      &
&      //'                                             zsp ','FALSE'
    ENDIF
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  CFL safety factor:                    '&
&    //'                                    cfl_sf ',hydro%cfl_sf
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Divergence safety factor:             '&
&    //'                                    div_sf ',hydro%div_sf
    WRITE(OSTREAM,'(a83)') '  Mid-length or projection for CFL length scale:  '&
&    //'                      zmidlength '
    WRITE(OSTREAM,'(a83)') '  Exclude region from CFL calculation:            '&
&    //'                       zdtnotreg '
    IF (hydro%zhg) THEN
      WRITE(OSTREAM,*) ' '
      WRITE(OSTREAM,'(a8,6X,a11)') '  region','   kappareg'
      DO ii=1,SIZE(hydro%kappareg)
        WRITE(OSTREAM,'(i8,1X,e16.9)') ii,hydro%kappareg(ii)
      ENDDO
    ENDIF
    IF (hydro%zsp) THEN
      WRITE(OSTREAM,*) ' '
      WRITE(OSTREAM,'(a8,6X,a11)') '  region','  pmeritreg'
      DO ii=1,SIZE(hydro%pmeritreg)
        WRITE(OSTREAM,'(i8,1X,e16.9)') ii,hydro%pmeritreg(ii)
      ENDDO
    ENDIF
    WRITE(OSTREAM,'(a1)') ' '
    WRITE(OSTREAM,'(a8,a11)') '  region',' mid-length'
    DO ii=1,SIZE(hydro%zmidlength)
      IF (hydro%zmidlength(ii)) THEN
        WRITE(OSTREAM,'(i8,6X,a5)') ii,' TRUE'
      ELSE
        WRITE(OSTREAM,'(i8,6X,a5)') ii,'FALSE'
      ENDIF
    ENDDO
    WRITE(OSTREAM,'(a1)') ' '
    WRITE(OSTREAM,'(a8,a11)') '  region','  cfl calc.'
    DO ii=1,SIZE(hydro%zdtnotreg)
      IF (hydro%zdtnotreg(ii)) THEN
        WRITE(OSTREAM,'(i8,6X,a5)') ii,'FALSE'
      ELSE
        WRITE(OSTREAM,'(i8,6X,a5)') ii,' TRUE'
      ENDIF
    ENDDO
    WRITE(OSTREAM,'(a132)')' #################################################'&
&    //'######################################################################'&
&    //'############'
    
  END SUBROUTINE hydro_cf_print

END MODULE hydro_cf_mod
