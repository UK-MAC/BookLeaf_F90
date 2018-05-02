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
MODULE io_cf_mod

  USE dataAPI_kinds_mod, ONLY: ink
  USE dataAPI_params_mod,ONLY: OSTREAM,SUCCESS,FAILURE,HALT_SINGLE,NLEN
  USE dataAPI_types_mod, ONLY: io_t,sizes_t,error_t

  IMPLICIT NONE

  PUBLIC :: io_cf_defaults,io_cf_get,io_cf_set,io_cf_rationalise,              &
&           io_cf_print

CONTAINS

  SUBROUTINE io_cf_defaults(io,error)

    ! Argument list
    TYPE(io_t),   INTENT(OUT) :: io 
    TYPE(error_t),INTENT(OUT) :: error
    
    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set IO defaults
    IF (ALLOCATED(io%sregion)) THEN
      DEALLOCATE(io%sregion,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(io%sregion(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    io%sregion(:)='   UNKNOWN'
    IF (ALLOCATED(io%smaterial)) THEN
      DEALLOCATE(io%smaterial,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(io%smaterial(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    io%smaterial(:)='   UNKNOWN'

  END SUBROUTINE io_cf_defaults

  SUBROUTINE io_cf_get(io,sregion,smaterial,error)

    ! Argument list
    TYPE(io_t),                                           INTENT(IN)  :: io
    CHARACTER(LEN=NLEN),DIMENSION(:),ALLOCATABLE,OPTIONAL,INTENT(OUT) ::       &
&    sregion,smaterial
    TYPE(error_t),                                      INTENT(OUT) :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! get
    IF (PRESENT(sregion)) THEN
      IF (ALLOCATED(sregion)) THEN
        DEALLOCATE(sregion,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate sregion"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(sregion(SIZE(io%sregion)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate sregion"
        RETURN
      ENDIF
      sregion(:)=io%sregion(:)
    ENDIF
    IF (PRESENT(smaterial)) THEN
      IF (ALLOCATED(smaterial)) THEN
        DEALLOCATE(smaterial,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate smaterial"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(smaterial(SIZE(io%smaterial)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate smaterial"
        RETURN
      ENDIF
      smaterial(:)=io%smaterial(:)
    ENDIF

  END SUBROUTINE io_cf_get

  SUBROUTINE io_cf_set(io,sregion,smaterial,error)

    ! Argument list
    TYPE(io_t),                                           INTENT(INOUT) :: io
    CHARACTER(LEN=NLEN),DIMENSION(:),ALLOCATABLE,OPTIONAL,INTENT(INOUT) ::     &  
&    sregion,smaterial
    TYPE(error_t),                                        INTENT(OUT)   ::     &
&    error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set
    IF (PRESENT(sregion)) THEN
      IF (ALLOCATED(io%sregion)) THEN
        DEALLOCATE(io%sregion,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate io%sregion"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(io%sregion(SIZE(sregion)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate io%sregion"
        RETURN
      ENDIF
      io%sregion(:)=sregion(:)
      DEALLOCATE(sregion,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate sregion"
        RETURN
      ENDIF
    ENDIF
    IF (PRESENT(smaterial)) THEN
      IF (ALLOCATED(io%smaterial)) THEN
        DEALLOCATE(io%smaterial,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate io%smaterial"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(io%smaterial(SIZE(smaterial)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate io%smaterial"
        RETURN
      ENDIF
      io%smaterial(:)=smaterial(:)
      DEALLOCATE(smaterial,STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to deallocate smaterial"
        RETURN
      ENDIF
    ENDIF

  END SUBROUTINE io_cf_set

  SUBROUTINE io_cf_rationalise(io,sizes,error)

    ! Argument list
    TYPE(io_t),   INTENT(INOUT) :: io
    TYPE(sizes_t),INTENT(IN)    :: sizes
    TYPE(error_t),INTENT(OUT)   :: error
    ! Local
    CHARACTER(LEN=NLEN),DIMENSION(:),ALLOCATABLE :: sregion,smaterial
    INTEGER(KIND=ink)                            :: ii

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! rationalise
    IF (.NOT.ALLOCATED(io%sregion)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: sregion not allocated"
      RETURN
    ENDIF
    IF (SIZE(io%sregion).LT.sizes%nreg) THEN
      error%ierr=FAILURE
      error%serr="ERROR: inconsistent no. regions for IO"
      RETURN
    ELSEIF (SIZE(io%sregion).GT.sizes%nreg) THEN
      ALLOCATE(sregion(sizes%nreg),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate sregion"
        RETURN
      ENDIF
      sregion(:)=io%sregion(1:sizes%nreg)
      CALL MOVE_ALLOC(FROM=sregion,TO=io%sregion)
    ENDIF
    DO ii=1,sizes%nreg
      IF (TRIM(ADJUSTL(io%sregion(ii))).EQ."UNKNOWN") THEN
        error%ierr=FAILURE
        error%serr="ERROR: undefined region"
        RETURN
      ENDIF
    ENDDO
    IF (.NOT.ALLOCATED(io%smaterial)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: smaterial not allocated"
      RETURN
    ENDIF
    IF (SIZE(io%smaterial).LT.sizes%nmat) THEN
      error%ierr=FAILURE
      error%serr="ERROR: inconsistent no. materials for IO"
      RETURN
    ELSEIF (SIZE(io%smaterial).GT.sizes%nmat) THEN
      ALLOCATE(smaterial(sizes%nmat),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate smaterial"
        RETURN
      ENDIF
      smaterial(:)=io%smaterial(1:sizes%nmat)
      CALL MOVE_ALLOC(FROM=smaterial,TO=io%smaterial)
    ENDIF
    DO ii=1,sizes%nmat
      IF (TRIM(ADJUSTL(io%smaterial(ii))).EQ."UNKNOWN") THEN
        error%ierr=FAILURE
        error%serr="ERROR: undefined material"
        RETURN
      ENDIF
    ENDDO

  END SUBROUTINE io_cf_rationalise

  SUBROUTINE io_cf_print(io)

    ! Argument list
    TYPE(io_t),INTENT(IN) :: io
    ! Local
    INTEGER(KIND=ink) :: ii

    WRITE(OSTREAM,'(a11)') ' IO OPTIONS'
    WRITE(OSTREAM,*) ' '
    WRITE(OSTREAM,'(a10,7X,a10)') '  region  ','   name   '
    DO ii=1,SIZE(io%sregion)
      WRITE(OSTREAM,'(i10,7X,a10)') ii,io%sregion(ii)
    ENDDO
    WRITE(OSTREAM,*) ' '
    WRITE(OSTREAM,'(a10,7X,a10)') '  material','   name   '
    DO ii=1,SIZE(io%smaterial)
      WRITE(OSTREAM,'(i10,7X,a10)') ii,io%smaterial(ii)
    ENDDO
    WRITE(OSTREAM,'(a132)')' #################################################'&
&    //'######################################################################'&
&    //'############'

  END SUBROUTINE io_cf_print

END MODULE io_cf_mod
