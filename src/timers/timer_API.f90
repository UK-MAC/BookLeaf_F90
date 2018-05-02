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
MODULE timerAPI_types_mod

  USE dataAPI_kinds_mod,ONLY: rlk,lok

  IMPLICIT NONE

  TYPE,PUBLIC :: timer_t
    CHARACTER(LEN=35) :: string
    REAL(KIND=rlk)    :: start,time
    LOGICAL(KIND=lok) :: zactive
  END TYPE timer_t

  TYPE(timer_t),DIMENSION(:),ALLOCATABLE,SAVE,PUBLIC :: timer

END MODULE timerAPI_types_mod

MODULE timerAPI_id_mod

  USE dataAPI_kinds_mod,ONLY: ink

  IMPLICIT NONE

  INTEGER(KIND=ink),PARAMETER,PUBLIC :: TTOTALID=1_ink,TINITID=2_ink,          &
&                                       TGETEOSIID=3_ink,TGETGEOMETRYIID=4_ink,&  
&                                       TCOMMREGISTERID=5_ink,TMESHGENID=6_ink,&
&                                       TMESHPARTITIONID=7_ink,                &
&                                       TSETUPICID=8_ink,TSOLVERID=9_ink,      &
&                                       TGETDTID=10_ink,TGETVISCOSITYID=11_ink,&
&                                       TCOMMTID=12_ink,TCOLLECTIVETID=13_ink, &
&                                       TLAGSTEPID=14_ink,TGETEOSLID=15_ink,   &
&                                       TGETACCELERATIONID=16_ink,             &
&                                       TGETGEOMETRYLID=17_ink,                &
&                                       TGETENERGYID=18_ink,TGETFORCEID=19_ink,&
&                                       TGETHGID=20_ink,TGETSPID=21_ink,       &
&                                       TCOMMLID=22_ink,TALESTEPID=23_ink,     &
&                                       TALEGETMESHSTATUSID=24_ink,            &
&                                       TALEGETFLUXVOLUMEID=25_ink,            &
&                                       TALEADVECTID=26_ink,                   &
&                                       TALEADVECTELID=27_ink,                 &
&                                       TALEADVECTBASISELID=28_ink,            &
&                                       TALEADVECTVARELID=29_ink,              &
&                                       TALEADVECTNDID=30_ink,                 &
&                                       TALEADVECTBASISNDID=31_ink,            &
&                                       TALEADVECTVARNDID=32_ink,              &
&                                       TALEUPDATEID=33_ink,TGETEOSAID=34_ink, &
&                                       TGETGEOMETRYAID=35_ink,TCOMMAID=36_ink,&
&                                       TSTEPIOID=37_ink,TIOID=38_ink
  INTEGER(KIND=ink),PARAMETER,PUBLIC :: NTIMERS=TIOID

END MODULE timerAPI_id_mod
