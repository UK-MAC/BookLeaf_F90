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
MODULE check_kn_tests_mod

  USE dataAPI_kinds_mod,       ONLY: ink,rlk
  USE dataAPI_params_mod,      ONLY: NDIM,NCORN
  USE geometry_kn_centroid_mod,ONLY: geometry_kn_getcentroid

  IMPLICIT NONE

  INTEGER(KIND=ink),PARAMETER,PRIVATE :: NSOD=2_ink

  PRIVATE :: sod
  PUBLIC  :: check_kn_sod
 
CONTAINS

  SUBROUTINE check_kn_sod(nsize,time,volume,density,energy,xx,yy,basis,l1)

    ! Argument list
    INTEGER(KIND=ink),                       INTENT(IN)  :: nsize
    REAL(KIND=rlk),                          INTENT(IN)  :: time
    REAL(KIND=rlk),   DIMENSION(nsize),      INTENT(IN)  :: volume,density,energy
    REAL(KIND=rlk),   DIMENSION(NCORN,nsize),INTENT(IN)  :: xx,yy
    REAL(KIND=rlk),                          INTENT(OUT) :: basis
    REAL(KIND=rlk),   DIMENSION(NSOD),       INTENT(OUT) :: l1
    ! Local
    INTEGER(KIND=ink)                 :: ii
    REAL(KIND=rlk),   DIMENSION(NDIM) :: centroid
    REAL(KIND=rlk),   DIMENSION(NSOD) :: solution

    ! initialise
    basis=0.0_rlk
    l1(:)=0.0_rlk

    DO ii=1,nsize
      ! find centroid
      centroid(:)=geometry_kn_getcentroid(NCORN,xx(:,ii),yy(:,ii))
      ! find solution
      solution(:)=sod(centroid(1)-50.0_rlk,time)
      ! calculate components for L1 norm
      basis=basis+volume(ii)
      l1(:)=[ABS(density(ii)-solution(1))*volume(ii),                          &
&            ABS(energy(ii)-solution(2))*volume(ii)]+l1(:)
    ENDDO

  END SUBROUTINE check_kn_sod

  PURE FUNCTION sod(xx,time) RESULT(res)

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: xx,time
    ! Result
    REAL(KIND=rlk),DIMENSION(NSOD) :: res
    ! Local
    REAL(KIND=rlk) :: coeff,ss,tt,velocity

    ! initialise
    res(:)=-HUGE(1.0_rlk)

    ! calculate solution
    coeff=SQRT(1.4_rlk)
    IF (xx.LE.-coeff*time) THEN
      res(1)=1.0_rlk
      res(2)=2.5_rlk
    ELSEIF (xx.LE.-0.07027281256118326961_rlk*time) THEN
      ss=xx/time
      velocity=(5.0_rlk/6.0_rlk)*(ss+coeff)
      tt=((velocity-ss)**2)/1.4_rlk
      res(1)=tt**2.5_rlk
      res(2)=res(1)*tt/(0.4_rlk*res(1))
    ELSEIF (xx.LE.0.92745262004894994908_rlk*time) THEN
      res(1)=0.42631942817849519385_rlk
      res(2)=1.77760006942335264781_rlk
    ELSE
      IF (time.LE.28.53627624872492075028_rlk) THEN
        IF (xx.LE.1.75215573203017816370_rlk*time) THEN
          res(1)=0.26557371170530706471_rlk
          res(2)=2.85354088799095973738_rlk
        ELSE
          res(1)=0.125_rlk
          res(2)=2.0_rlk
        ENDIF
      ELSEIF (time.LT.40.68191725689148697880_rlk) THEN
        IF (xx.LE.(50.0_rlk-(time-28.53627624872492075028_rlk)                 &
&                          *1.01019363599101820444_rlk)) THEN
          res(1)=0.26557371170530706471_rlk
          res(2)=2.85354088799095973738_rlk
        ELSE
          res(1)=0.50939531774381657731_rlk
          res(2)=3.82996297061558546292_rlk
        ENDIF
      ENDIF
    ENDIF

  END FUNCTION sod

END MODULE check_kn_tests_mod
