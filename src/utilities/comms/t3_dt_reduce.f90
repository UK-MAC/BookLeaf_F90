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
MODULE TYPH_dt_reduce_mod

  USE TYPH_Types_mod

  IMPLICIT NONE

  INTEGER(kind=ink) :: dt_reduce_type
  INTEGER(kind=ink) :: dt_reduce_op

  TYPE :: reduce_t
    REAL   (KIND=rlk) :: rdt
    INTEGER(KIND=ink) :: idt
    CHARACTER(LEN=8)  :: sdt
    CHARACTER(LEN=10) :: mdt
  END TYPE reduce_t

  PRIVATE
  PUBLIC :: TYPH_add_reduce_dt,TYPH_reduce_dt

CONTAINS

  ! Procedure to perform the reduction operation: a min
  SUBROUTINE TYPH_dtreduceop(INVEC, INOUTVEC, NSZ, DATATYPE)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: NSZ
    TYPE(reduce_t),   DIMENSION(NSZ),INTENT(IN)    :: INVEC
    TYPE(reduce_t),   DIMENSION(NSZ),INTENT(INOUT) :: INOUTVEC
    INTEGER(KIND=ink),               INTENT(IN)    :: DATATYPE
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii = 1, NSZ
      IF (INOUTVEC(ii)%rdt .GT. INVEC(ii)%rdt) THEN
        INOUTVEC(ii)%rdt = INVEC(ii)%rdt
        INOUTVEC(ii)%idt = INVEC(ii)%idt
        INOUTVEC(ii)%sdt = INVEC(ii)%sdt
        INOUTVEC(ii)%mdt = INVEC(ii)%mdt
      END IF
    END DO

  END SUBROUTINE TYPH_dtreduceop

  ! Procedure to register the type and operator with MPI
  INTEGER(KIND=ink) FUNCTION TYPH_add_reduce_dt() RESULT(fres)

    INTEGER(KIND=ink),              PARAMETER         :: ntypes=4_ink
    INTEGER(KIND=ink),              DIMENSION(ntypes) :: blocklen
    INTEGER(kind=MPI_ADDRESS_KIND), DIMENSION(ntypes) :: disp
    INTEGER(KIND=ink),              DIMENSION(ntypes) :: types
    INTEGER(KIND=ink)                                 :: ierr
    TYPE(reduce_t)                                    :: dummyType
    LOGICAL(KIND=lok),              PARAMETER         :: commutativity=.true.

    CALL MPI_GET_ADDRESS(dummyType%rdt, disp(1), ierr)
    CALL MPI_GET_ADDRESS(dummyType%idt, disp(2), ierr)
    CALL MPI_GET_ADDRESS(dummyType%sdt, disp(3), ierr)
    CALL MPI_GET_ADDRESS(dummyType%mdt, disp(4), ierr)

    disp(4) = disp(4) - disp(1)
    disp(3) = disp(3) - disp(1)
    disp(2) = disp(2) - disp(1)
    disp(1) = disp(1) - disp(1)

    blocklen(1) = 1
    blocklen(2) = 1
    blocklen(3) = 8
    blocklen(4) = 10

    types(1) = MPI_DOUBLE_PRECISION
    types(2) = MPI_INTEGER
    types(3) = MPI_CHARACTER
    types(4) = MPI_CHARACTER

    CALL MPI_TYPE_CREATE_STRUCT(ntypes, blocklen, disp, types, dt_reduce_type, ierr)
    CALL MPI_TYPE_COMMIT(dt_reduce_type, ierr)
    CALL MPI_OP_CREATE(TYPH_dtreduceop, commutativity, dt_reduce_op, ierr)

    fres=ierr

  END FUNCTION TYPH_add_reduce_dt

  INTEGER(KIND=ink) FUNCTION TYPH_reduce_dt(Val, Comm) RESULT(fres)

    TYPE(dt_t),        INTENT(INOUT) :: Val
    INTEGER(KIND=ink), INTENT(IN)    :: Comm

    INTEGER(KIND=ink) :: irc
    TYPE(reduce_t)    :: ival,rval

    ival%rdt = Val%rdt
    ival%idt = Val%idt
    ival%sdt = Val%sdt
    ival%mdt = Val%mdt

    CALL MPI_ALLREDUCE(IVal, RVal, 1, dt_reduce_type, dt_reduce_op, Comm, irc)

    Val%rdt = rval%rdt
    Val%idt = rval%idt
    Val%sdt = rval%sdt
    Val%mdt = rval%mdt

    fres = irc

  END FUNCTION TYPH_reduce_dt

END MODULE TYPH_dt_reduce_mod
