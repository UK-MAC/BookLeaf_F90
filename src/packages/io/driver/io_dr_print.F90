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
MODULE io_dr_print_mod

  USE dataAPI_kinds_mod, ONLY: ink,lok
  USE dataAPI_params_mod,ONLY: OSTREAM,SLEN,SUCCESS,HALT_SINGLE
  USE dataAPI_types_mod, ONLY: comm_t,error_t,config_t
  USE eos_cf_mod,        ONLY: eos_cf_print
  USE io_cf_mod,         ONLY: io_cf_print
  USE global_cf_mod,     ONLY: global_cf_print

  IMPLICIT NONE

  PUBLIC :: io_dr_banner,io_dr_header,io_dr_preprocess,io_dr_utils,io_dr_print,&
&           io_dr_spacer,io_dr_echo

CONTAINS

  SUBROUTINE io_dr_banner(comm)

    ! Argument list
    TYPE(comm_t),INTENT(IN) :: comm

    ! binding
    IF (.NOT.comm%zmproc) RETURN

    CALL io_dr_spacer()
    WRITE(OSTREAM,*) ' Bookleaf:   ','Light-weight FE hydro scheme'
    WRITE(OSTREAM,*) ' Author:     ','R. Kevis, D. Harris'
    WRITE(OSTREAM,*) ' Revision:   ','Unversioned directory'

  END SUBROUTINE io_dr_banner

  SUBROUTINE io_dr_header(sfile)

    ! Argument list
    CHARACTER(LEN=*),INTENT(IN) :: sfile
    ! Local
    CHARACTER(LEN=8)  :: dat
    CHARACTER(LEN=10) :: tim
    CHARACTER(LEN=5)  :: zon

    CALL DATE_AND_TIME(DATE=dat,TIME=tim,ZONE=zon)

    CALL io_dr_spacer()
    WRITE(OSTREAM,*) ' Input file:       ',TRIM(ADJUSTL(sfile))
    WRITE(OSTREAM,'(a29,a12,a10)') ' Time stamp:      '//dat(7:8)//'/'//       &
&    dat(5:6)//'/'//dat(1:4),' at '//tim(1:2)//':'//tim(3:4)//':'//tim(5:6),' '&
&    //zon(1:5)//' GMT'
    CALL io_dr_spacer()

  END SUBROUTINE io_dr_header

  SUBROUTINE io_dr_preprocess(nproc,nthread)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN) :: nproc,nthread

    WRITE(OSTREAM,'(a23)') ' PRE-PROCESSING OPTIONS'
#ifdef NOMPI
    WRITE(OSTREAM,'(a25)') '  No MPI routine included'
#else
    WRITE(OSTREAM,'(a26)') '  MPI parallelism included'
    WRITE(OSTREAM,'(a14,i5,a10)') '  Running on: ',nproc,' MPI tasks'
#endif
#ifdef NOOMP
    WRITE(OSTREAM,'(a24)') '  No OpenMP threads used'
#else
    WRITE(OSTREAM,'(a9,i5,a16)') '  Using: ',nthread,' Open MP threads'
#endif
#ifdef METIS
    WRITE(OSTREAM,'(a30)') '  Metis used for decomposition'
#else
    WRITE(OSTREAM,'(a34)') '  Metis not used for decomposition'
#endif
#ifdef MODY
    WRITE(OSTREAM,'(a49)') '  Additional problem specific initialisation used'
#else
    WRITE(OSTREAM,'(a52)') '  No additional problem specific initialisation used'
#endif
#ifdef SILO
    WRITE(OSTREAM,'(a38)') '  SILO visualisation dumps written out'
#else
    WRITE(OSTREAM,'(a39)') '  No SILO visualisation dumps available'
#endif
#ifdef TIO
    WRITE(OSTREAM,'(a42)') '  TyphonIO visualisation dumps written out'
#else
    WRITE(OSTREAM,'(a43)') '  No TyphonIO visualisation dumps available'
#endif
    CALL io_dr_spacer()

  END SUBROUTINE io_dr_preprocess

  SUBROUTINE io_dr_utils(config)

    ! Argument list
    TYPE(config_t),INTENT(IN) :: config

    ! EoS
    CALL eos_cf_print(config%eos)

    ! IO
    CALL io_cf_print(config%io)

    ! Global
    CALL global_cf_print(config%global)

  END SUBROUTINE io_dr_utils

  SUBROUTINE io_dr_spacer()

    WRITE(OSTREAM,'(a132)') ' ################################################'&
&    //'######################################################################'&
&    //'#############'

  END SUBROUTINE io_dr_spacer

  SUBROUTINE io_dr_print(string)

    ! Argument list
    CHARACTER(LEN=*),INTENT(IN) :: string

    WRITE(OSTREAM,'(2X,a130)') string

  END SUBROUTINE io_dr_print

  SUBROUTINE io_dr_echo(sfile,error)

    ! Argument list
    CHARACTER(LEN=*),INTENT(IN)  :: sfile
    TYPE(error_t),   INTENT(OUT) :: error
    ! Local
    LOGICAL(KIND=lok)     :: zopen,zflag
    INTEGER(KIND=ink)     :: iunit,inext,ii
    CHARACTER(LEN=SLEN-2) :: string
    CHARACTER(LEN=3)      :: ssize
    CHARACTER(LEN=9)      :: wfmt
    CHARACTER(LEN=6)      :: rfmt

    ! initialise
    error%ierr=SUCCESS
    error%iout=HALT_SINGLE

    ! open the file if required
    INQUIRE(FILE=sfile,OPENED=zopen)
    IF (zopen) THEN
      INQUIRE(FILE=sfile,NUMBER=iunit,NEXTREC=inext)
      REWIND(UNIT=iunit)
    ELSE
      iunit=30_ink
      search:DO
        INQUIRE(UNIT=iunit,OPENED=zflag)
        IF (.NOT.zflag) EXIT search
        iunit=iunit+1_ink
      ENDDO search
      OPEN(FILE=sfile,UNIT=iunit,FORM="formatted",STATUS="old",                &
&          ACCESS="sequential",ACTION='read',IOSTAT=error%ierr,                &
&          IOMSG=error%serr)
      IF (error%ierr.NE.SUCCESS) RETURN
    ENDIF

    ! echo file
    WRITE(ssize,'(i3)') SLEN-2
    wfmt='(2X,a'//TRIM(ADJUSTL(ssize))//')'
    rfmt='(a'//TRIM(ADJUSTL(ssize))//')'
    loop:DO
      READ(UNIT=iunit,IOSTAT=error%ierr,IOMSG=error%serr,FMT=rfmt) string
      IF (error%ierr.LT.0_ink) THEN
        error%ierr=SUCCESS
        error%serr=' '
        EXIT loop
      ENDIF
      IF (error%ierr.NE.SUCCESS) RETURN
      WRITE(UNIT=OSTREAM,FMT=wfmt) string
    ENDDO loop

    ! close the file if required
    IF (zopen) THEN
      REWIND(UNIT=iunit)
      DO ii=1,inext-1
        READ(UNIT=iunit,FMT=rfmt) string
      ENDDO
    ELSE
      CLOSE(UNIT=iunit)
    ENDIF

    ! spacer
    CALL io_dr_spacer()

  END SUBROUTINE io_dr_echo

END MODULE io_dr_print_mod  
