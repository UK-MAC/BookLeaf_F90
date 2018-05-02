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
MODULE setup_rcb_mod

  USE dataAPI_kinds_mod,ONLY: ink

  IMPLICIT NONE

  PUBLIC :: setup_rcb

CONTAINS  

  RECURSIVE SUBROUTINE setup_rcb(nl,nk,npartl,nparth,ipart,icolour)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)    :: nl,nk,npartl,nparth
    INTEGER(KIND=ink),                 INTENT(INOUT) :: ipart
    INTEGER(KIND=ink),DIMENSION(nl,nk),INTENT(INOUT) :: icolour
    ! Local
    INTEGER(KIND=ink)                                :: nmid,npartmid,npart    

    ! calculate the number of remaining partitions
    npart=nparth-npartl+1_ink

    ! finish
    IF (npart.EQ.1_ink) THEN
      ipart=ipart+1_ink
      icolour(1:nl,1:nk)=ipart
      RETURN 
    ENDIF

    ! set colour
    npartmid=npartl+1_ink
    IF (nl.GT.nk) THEN
      nmid=nl/npart
      IF ((npartmid-npartl).GT.0_ink) THEN
        CALL setup_rcb(nmid,nk,npartl,npartmid-1_ink,ipart,                    &
&                      icolour(1:nmid,1:nk))
      ENDIF
      IF ((nparth-npartmid+1_ink).GT.0_ink) THEN
        CALL setup_rcb(nl-nmid,nk,npartmid,nparth,ipart,                       &
&                      icolour(nmid+1_ink:nl,1:nk))
      ENDIF
    ELSE
      nmid=nk/npart
      IF ((npartmid-npartl).GT.0_ink) THEN
        CALL setup_rcb(nl,nmid,npartl,npartmid-1_ink,ipart,                    &
&                      icolour(1:nl,1:nmid))
      ENDIF
      IF ((nparth-npartmid+1_ink).GT.0_ink) THEN
        CALL setup_rcb(nl,nk-nmid,npartmid,nparth,ipart,                       &
&                      icolour(1:nl,nmid+1_ink:nk))
      ENDIF
    ENDIF

  END SUBROUTINE setup_rcb

END MODULE setup_rcb_mod
