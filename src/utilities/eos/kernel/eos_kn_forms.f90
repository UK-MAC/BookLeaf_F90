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
MODULE eos_kn_forms_mod

  USE dataAPI_kinds_mod,ONLY: ink,rlk

  IMPLICIT NONE

  PUBLIC :: eos_kn_getpressure,eos_kn_getcs2

CONTAINS

  PURE FUNCTION eos_kn_getpressure(mat,density,energy,eos_type,eos_param,pcut) &
&  RESULT(pressure)

    ! Argument list
    INTEGER(KIND=ink),                INTENT(IN) :: mat
    REAL(KIND=rlk),                   INTENT(IN) :: density,energy,pcut
    INTEGER(KIND=ink),DIMENSION(0:),  INTENT(IN) :: eos_type
    REAL(KIND=rlk),   DIMENSION(:,0:),INTENT(IN) :: eos_param
    ! Result
    REAL(KIND=rlk) :: pressure
    ! Local
    INTEGER(KIND=ink) :: imat
    REAL(KIND=rlk)    :: t1,t2,t3,t4,t5

    imat=MAX(mat,0_ink)
    SELECT CASE(eos_type(imat))
      CASE(0_ink) ! VOID
        pressure=eos_param(1,imat)
      CASE(1_ink) ! IDEAL GAS
        pressure=energy*density*(eos_param(1,imat)-1.0_rlk)
      CASE(2_ink) ! TAIT
        t1=density/eos_param(3,imat)
        pressure=eos_param(1,imat)*(t1**eos_param(2,imat)-1.0_rlk)
        pressure=MAX(pressure,eos_param(4,imat))
      CASE(3_ink) ! JWL
        t1=eos_param(4,imat)*eos_param(6,imat)/density
        t2=eos_param(5,imat)*eos_param(6,imat)/density
        t3=eos_param(1,imat)*density*energy
        t4=(1.0_rlk-eos_param(1,imat)/t1)*eos_param(2,imat)*EXP(-t1)
        t5=(1.0_rlk-eos_param(1,imat)/t2)*eos_param(3,imat)*EXP(-t2)
        pressure=t3+t4+t5
      CASE DEFAULT
        pressure=-1.0_rlk
    END SELECT
    IF (ABS(pressure).LT.pcut) pressure=0.0_rlk

  END FUNCTION eos_kn_getpressure

  PURE FUNCTION eos_kn_getcs2(mat,density,energy,eos_type,eos_param,pcut,ccut) &
&  RESULT(cs2)

    ! Argument list
    INTEGER(KIND=ink),                INTENT(IN) :: mat
    REAL(KIND=rlk),                   INTENT(IN) :: density,energy,pcut,ccut
    INTEGER(KIND=ink),DIMENSION(0:),  INTENT(IN) :: eos_type
    REAL(KIND=rlk),   DIMENSION(:,0:),INTENT(IN) :: eos_param
    ! Result
    REAL(KIND=rlk) :: cs2
    ! Local
    INTEGER(KIND=ink) :: imat
    REAL(KIND=rlk)    :: t1,t2,t3,t4,t5

    imat=MAX(mat,0_ink)
    SELECT CASE(eos_type(imat))
      CASE(0_ink) ! VOID
        cs2=ccut
      CASE(1_ink) ! IDEAL GAS
        cs2=eos_param(1,imat)*(eos_param(1,imat)-1.0_rlk)*energy
      CASE(2_ink) ! TAIT
        t1=density/eos_param(3,imat)
        t2=eos_param(2,imat)-1.0_rlk
        cs2=(eos_param(1,imat)*eos_param(2,imat))/eos_param(3,imat)
        cs2=cs2*t1**t2
      CASE(3_ink) ! JWL
        t1=eos_param(6,imat)/density
        t2=eos_kn_getpressure(imat,density,energy,eos_type,eos_param,pcut)
        t3=eos_param(4,imat)*t1
        t4=eos_param(1,imat)/eos_param(4,imat)+eos_param(1,imat)*t1-t3*t1
        t4=t4*eos_param(2,imat)*EXP(-t3)
        t3=eos_param(5,imat)*t1
        t5=eos_param(1,imat)/eos_param(5,imat)+eos_param(1,imat)*t1-t3*t1
        t5=t5*eos_param(3,imat)*EXP(-t3)
        cs2=eos_param(1,imat)*t2/density+eos_param(1,imat)*energy-t4-t5
      CASE DEFAULT
        cs2=ccut
    END SELECT
    cs2=MAX(cs2,ccut)

  END FUNCTION eos_kn_getcs2

END MODULE eos_kn_forms_mod
