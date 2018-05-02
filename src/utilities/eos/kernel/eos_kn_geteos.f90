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
MODULE eos_kn_geteos_mod

  USE dataAPI_kinds_mod,ONLY: ink,rlk

  IMPLICIT NONE

  PUBLIC :: eos_kn_geteos

CONTAINS

  SUBROUTINE eos_kn_geteos(nsize,eos_type,eos_param,pcut,ccut,imat,density,    &
&                          energy,pressure,cs2)

    USE eos_kn_forms_mod,ONLY: eos_kn_getpressure,eos_kn_getcs2

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)    :: nsize
    INTEGER(KIND=ink),DIMENSION(0:),   INTENT(IN)    :: eos_type
    REAL(KIND=rlk),   DIMENSION(:,0:), INTENT(IN)    :: eos_param
    REAL(KIND=rlk),                    INTENT(IN)    :: pcut,ccut
    INTEGER(KIND=ink),DIMENSION(nsize),INTENT(IN)    :: imat
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(IN)    :: density,energy
    REAL(KIND=rlk),   DIMENSION(nsize),INTENT(OUT)   :: pressure,cs2
    ! Local
    INTEGER(KIND=ink)                                :: ii

    ! update pressure and sound speed
    DO ii=1,nsize
      pressure(ii)=eos_kn_getpressure(imat(ii),density(ii),energy(ii),         &
&                                     eos_type(:),eos_param(:,:),pcut)
      cs2(ii)=eos_kn_getcs2(imat(ii),density(ii),energy(ii),eos_type(:),       &
&                           eos_param(:,:),pcut,ccut)
    ENDDO

  END SUBROUTINE eos_kn_geteos

END MODULE eos_kn_geteos_mod  
