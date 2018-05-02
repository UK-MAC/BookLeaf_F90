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
MODULE eos_cf_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_types_mod, ONLY: eos_t,error_t,sizes_t
  USE dataAPI_params_mod,ONLY: SUCCESS,FAILURE,HALT_SINGLE,OSTREAM

  IMPLICIT NONE

  PUBLIC :: eos_cf_defaults,eos_cf_get,eos_cf_set,eos_cf_rationalise,          &
&           eos_cf_print

CONTAINS

  SUBROUTINE eos_cf_defaults(eos,error)

    ! Argument list
    TYPE(eos_t),  INTENT(INOUT) :: eos
    TYPE(error_t),INTENT(OUT)   :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set EoS defaults
    eos%ccut=1.0e-6_rlk
    eos%pcut=1.0e-8_rlk
    IF (ALLOCATED(eos%itype)) THEN
      DEALLOCATE(eos%itype,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(eos%itype(1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    eos%itype(1)=-1_ink
    IF (ALLOCATED(eos%param)) THEN
      DEALLOCATE(eos%param,STAT=error%ierr,ERRMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF
    ALLOCATE(eos%param(2,1),STAT=error%ierr,ERRMSG=error%serr)
    IF (error%ierr.NE.SUCCESS) RETURN
    eos%param(1,1)=-1.0_rlk
    eos%param(2,1)=-1.0_rlk

  END SUBROUTINE eos_cf_defaults

  SUBROUTINE eos_cf_get(eos,ccut,pcut,eos_type,eos_param,error)

    ! Argument list
    TYPE(eos_t),                              INTENT(IN)  :: eos
    REAL(KIND=rlk),                  OPTIONAL,INTENT(OUT) :: ccut,pcut
    INTEGER(KIND=ink),DIMENSION(:),  OPTIONAL,INTENT(OUT) :: eos_type
    REAL(KIND=rlk),   DIMENSION(:,:),OPTIONAL,INTENT(OUT) :: eos_param
    TYPE(error_t),                            INTENT(OUT) :: error
    ! Local
    INTEGER(KIND=ink) :: ii,is1,is2

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! get
    IF (PRESENT(ccut)) ccut=eos%ccut
    IF (PRESENT(pcut)) pcut=eos%pcut
    IF (PRESENT(eos_type)) THEN
      IF (.NOT.ALLOCATED(eos%itype)) THEN
        error%ierr=FAILURE
        error%serr="ERROR: eos%itype not allocated in eos_cf_get"
        RETURN
      ENDIF
      IF (SIZE(eos_type).LE.SIZE(eos%itype)) THEN
        eos_type(:)=eos%itype(1:SIZE(eos_type))
      ELSE
        DO ii=1,SIZE(eos_type)/SIZE(eos%itype)
          eos_type((ii-1)*SIZE(eos%itype)+1:ii*SIZE(eos%itype))=eos%itype(:)
        ENDDO
      ENDIF
    ENDIF
    IF (PRESENT(eos_param)) THEN
      IF (.NOT.ALLOCATED(eos%param)) THEN
        error%ierr=FAILURE
        error%serr="ERROR: eos%param not allocated in eos_cf_set"
      ENDIF
      is1=SIZE(eos_param,DIM=2)
      is2=SIZE(eos%param,DIM=2)
      IF (is1.LE.is2) THEN
        eos_param(1:SIZE(eos%param,DIM=1),:)=eos%param(:,1:is1)
      ELSE
        DO ii=1,is1/is2
          eos_param(1:SIZE(eos%param,DIM=1),(ii-1)*is2+1:ii*is2)=              &
&          eos%param(:,:)
        ENDDO
      ENDIF
    ENDIF

  END SUBROUTINE eos_cf_get

  SUBROUTINE eos_cf_set(eos,ccut,pcut,eos_type,eos_param,error)

    ! Argument list
    TYPE(eos_t),                              INTENT(INOUT) :: eos
    REAL(KIND=rlk),                  OPTIONAL,INTENT(IN)    :: ccut,pcut
    INTEGER(KIND=ink),DIMENSION(:),  OPTIONAL,INTENT(IN)    :: eos_type
    REAL(KIND=rlk),   DIMENSION(:,:),OPTIONAL,INTENT(IN)    :: eos_param
    TYPE(error_t),                            INTENT(OUT)   :: error

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! set
    IF (PRESENT(ccut)) eos%ccut=ccut
    IF (PRESENT(pcut)) eos%pcut=pcut
    IF (PRESENT(eos_type)) THEN
      IF (ALLOCATED(eos%itype)) THEN
        DEALLOCATE(eos%itype,STAT=error%ierr)
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate eos%itype"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(eos%itype(SIZE(eos_type)),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate eos%itype"
        RETURN
      ENDIF
      eos%itype(:)=eos_type(:)
    ENDIF
    IF (PRESENT(eos_param)) THEN
      IF (ALLOCATED(eos%param)) THEN
        DEALLOCATE(eos%param,STAT=error%ierr) 
        IF (error%ierr.NE.SUCCESS) THEN
          error%serr="ERROR: failed to deallocate eos%param"
          RETURN
        ENDIF
      ENDIF
      ALLOCATE(eos%param(SIZE(eos_param,DIM=1),SIZE(eos_param,DIM=2)),         &
&              STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate eos%param"
        RETURN
      ENDIF
      eos%param(:,:)=eos_param(:,:)
    ENDIF

  END SUBROUTINE eos_cf_set

  SUBROUTINE eos_cf_rationalise(eos,sizes,error)

    ! Argument list
    TYPE(eos_t),  INTENT(INOUT) :: eos
    TYPE(sizes_t),INTENT(IN)    :: sizes
    TYPE(error_t),INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink),DIMENSION(:),  ALLOCATABLE :: eos_type
    REAL(KIND=rlk),   DIMENSION(:,:),ALLOCATABLE :: eos_param

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! rationalise
    IF (eos%ccut.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: ccut < 0"
      RETURN
    ENDIF
    IF (eos%pcut.LT.0.0_rlk) THEN
      error%ierr=FAILURE
      error%serr="ERROR: pcut < 0"
    ENDIF
    IF (.NOT.ALLOCATED(eos%itype)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: eos%itype not allocated"
      RETURN
    ENDIF
    IF (SIZE(eos%itype).LT.sizes%nmat) THEN
      error%ierr=FAILURE
      error%serr="ERROR: inconsistent no. materials for eos"
      RETURN
    ELSEIF (SIZE(eos%itype).GE.sizes%nmat) THEN
      ALLOCATE(eos_type(0:sizes%nmat),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate eos_type"
        RETURN
      ENDIF
      eos_type(0)=0_ink
      eos_type(1:)=eos%itype(1:sizes%nmat)
      CALL MOVE_ALLOC(FROM=eos_type,TO=eos%itype)
    ENDIF
    IF (.NOT.ALLOCATED(eos%param)) THEN
      error%ierr=FAILURE
      error%serr="ERROR: eos%param not allocated"
      RETURN
    ENDIF
    IF (SIZE(eos%param,DIM=2).LT.sizes%nmat) THEN
      error%ierr=FAILURE
      error%serr="ERROR: inconsistent no. materials for eos"
      RETURN
    ELSEIF (SIZE(eos%param,DIM=2).GE.sizes%nmat) THEN
      ALLOCATE(eos_param(SIZE(eos%param,DIM=1),0:sizes%nmat),STAT=error%ierr)
      IF (error%ierr.NE.SUCCESS) THEN
        error%serr="ERROR: failed to allocate eos_param"
        RETURN
      ENDIF
      eos_param(:,0)=0.0_rlk
      eos_param(:,1:)=eos%param(:,1:sizes%nmat)
      CALL MOVE_ALLOC(FROM=eos_param,TO=eos%param)
    ENDIF

  END SUBROUTINE eos_cf_rationalise

  SUBROUTINE eos_cf_print(eos)

    ! Argument list
    TYPE(eos_t),INTENT(IN) :: eos
    ! Local
    INTEGER(KIND=ink) :: ii

    WRITE(OSTREAM,'(a12)') ' EOS OPTIONS'
    DO ii=1,UBOUND(eos%itype,DIM=1,KIND=ink)
      WRITE(OSTREAM,'(a12,i3)') '  Material: ',ii
      SELECT CASE(eos%itype(ii))
        CASE(0)
          WRITE(OSTREAM,'(a83,33X,a16)') '  EOS:                              '&
&          //'                                         itype ',                &
&          '            VOID'
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   Void pressure (p0):     '&
&          //'                                           param(1,',ii,') ',    &
&          eos%param(1,ii)
        CASE(1)
          WRITE(OSTREAM,'(a83,33X,a16)') '  EOS:                              '&
&          //'                                         itype ',                &
&          '       IDEAL GAS'
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   Ideal gas gamma:        '&
&          //'                                           param(1,',ii,') ',    &
&          eos%param(1,ii)
        CASE(2)
          WRITE(OSTREAM,'(a83,33X,a16)') '  EOS:                              '&
&          //'                                         itype ',                &
&          '            TAIT'
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   Tait a:                 '&
&          //'                                           param(1,',ii,') ',    &
&          eos%param(1,ii)
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   Tait b:                 '&
&          //'                                           param(2,',ii,') ',    &
&          eos%param(2,ii)
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   Tait rho0:              '&
&          //'                                           param(3,',ii,') ',    &
&          eos%param(3,ii)
        CASE(3)
          WRITE(OSTREAM,'(a83,33X,a16)') '  EOS:                              '&
&          //'                                         itype ',                &
&          '             JWL'
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   JWL omega:              '&
&          //'                                           param(1,',ii,') ',    &
&          eos%param(1,ii)
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   JWL a:                  '&
&          //'                                           param(2,',ii,') ',    &
&          eos%param(2,ii)
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   JWL b:                  '&
&          //'                                           param(3,',ii,') ',    &
&          eos%param(3,ii)
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   JWL r1:                 '&
&          //'                                           param(4,',ii,') ',    &
&          eos%param(4,ii)
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   JWL r2:                 '&
&          //'                                           param(5,',ii,') ',    &
&          eos%param(5,ii)
          WRITE(OSTREAM,'(a78,i3,a2,33X,f16.10)') '   JWL rho0:               '&
&          //'                                           param(6,',ii,') ',    &
&          eos%param(6,ii)
      END SELECT
    ENDDO
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Pressure cut-off:                     '&
&    //'                                      pcut ',eos%pcut
    WRITE(OSTREAM,'(a83,33X,e16.9)') '  Sound speed cut-off:                  '&
&    //'                                      ccut ',eos%ccut
    WRITE(OSTREAM,'(a132)') ' ################################################'&
&    //'######################################################################'&
&    //'#############'

  END SUBROUTINE eos_cf_print

END MODULE eos_cf_mod
