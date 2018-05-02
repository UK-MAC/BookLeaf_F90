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
MODULE setup_metis_mod

  USE dataAPI_kinds_mod,  ONLY: ink,rlk
  USE dataAPI_params_mod, ONLY: NCORN,SUCCESS,FAILURE,HALT_SINGLE
  USE dataAPI_types_mod,  ONLY: error_t
  USE typhon_API_mod,     ONLY: TYPH_Gather,TYPH_Reduce,TYPH_OP_SUM
  USE iso_c_binding,      ONLY: c_int,c_double,c_ptr,c_null_ptr

  IMPLICIT NONE

  PUBLIC :: setup_metis

CONTAINS  

  SUBROUTINE setup_metis(nel,nprocw,rankw,commw,conndata,coldata,error)

    ! Argument list
    INTEGER(KIND=ink),                            INTENT(IN)    :: nel,nprocw, &
&                                                                  rankw,commw
    INTEGER(KIND=ink),DIMENSION(:,:),             INTENT(IN)    :: conndata
    INTEGER(KIND=ink),DIMENSION(:),   ALLOCATABLE,INTENT(INOUT) :: coldata
    TYPE(error_t),                                INTENT(OUT)   :: error
    ! Local
    INTEGER(KIND=ink)                          :: ii,jj,kk
    INTEGER(KIND=ink),DIMENSION(0:nprocw-1)    :: nelg,sumg
    ! c bindings for Parmetis
    INTEGER(KIND=c_int),PARAMETER              :: NCON=1_c_int
    REAL(KIND=c_double),PARAMETER              :: UBVEC_VAL=1.05_c_double
    INTEGER(KIND=c_int)                        :: edgecut,nndcomm,numflag,     &
&                                                 wgtflag,proc,comm
    REAL(KIND=c_double),DIMENSION(NCON,nprocw) :: tpgwgts
    REAL(KIND=c_double),DIMENSION(NCON)        :: ubvec
    INTEGER(KIND=c_int),DIMENSION(nprocw+1)    :: elmdist
    INTEGER(KIND=c_int),DIMENSION(nel+1)       :: eptr
    INTEGER(KIND=c_int),DIMENSION(NCORN*nel)   :: eind
    TYPE(c_ptr)                                :: elmwgt
    INTEGER(KIND=c_int),DIMENSION(3)           :: options

    ! initialise
    error%ierr=SUCCESS
    comm=commw
    proc=nprocw
    edgecut=0_c_int
    elmwgt=c_null_ptr
    nndcomm=2_c_int
    numflag=1_c_int
    wgtflag=0_c_int
    options(:)=0_c_int
    tpgwgts(:,:)=1.0_c_double/nprocw
    ubvec(:)=UBVEC_VAL
    ii=TYPH_Gather(nel,nelg,commw)
    elmdist(1)=1_c_int
    elmdist(2:nprocw+1)=nelg(:)
    DO ii=2,nprocw+1
      elmdist(ii)=elmdist(ii-1)+elmdist(ii)
    ENDDO

    ! convert global mesh into a format understood by Metis
    jj=1_ink
    eptr(1)=1_c_int
    DO ii=1,nel
      DO kk=4,NCORN+3
        eind(jj)=conndata(kk,ii)
        jj=jj+1_ink
      ENDDO
      eptr(ii+1)=jj
    ENDDO

    ! set initial partition (NB. Metis expecting (1:) indexing)
    ALLOCATE(coldata(nel))
    coldata(:)=rankw+1_ink
    CALL ParMetis_V3_PartMeshKway(elmdist,eptr,eind,elmwgt,wgtflag,numflag,    &
&                                 NCON,nndcomm,proc,tpgwgts,ubvec,options,     &
&                                 edgecut,coldata,comm)

    ! convert back to (0:) indexing
    DO ii=1,nel
      coldata(ii)=coldata(ii)-1_ink
    ENDDO

    ! sanity check that ParMetis has not left an empty processor
    nelg(:)=0_ink
    DO ii=1,nel
      nelg(coldata(ii))=nelg(coldata(ii))+1_ink
    ENDDO
    ii=TYPH_Reduce(nelg,sumg,TYPH_OP_SUM,commw)
    DO ii=0,nprocw-1
      IF (sumg(ii).EQ.0_ink) THEN
        error%ierr=FAILURE
        error%iout=HALT_SINGLE
        error%serr="ERROR: Metis has given a processor no work"
        RETURN
      ENDIF
    ENDDO

  END SUBROUTINE setup_metis

END MODULE setup_metis_mod
