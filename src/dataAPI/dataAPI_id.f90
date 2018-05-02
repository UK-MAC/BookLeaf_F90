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
MODULE dataAPI_id_mod

  ! Internal
  USE dataAPI_kinds_mod,ONLY: ink

  IMPLICIT NONE

  ! id
  INTEGER(KIND=ink),PARAMETER,PUBLIC :: eldensityid=1_ink,elenergyid=2_ink,    &
&                                       elpressureid=3_ink,elcs2id=4_ink,      &
&                                       elvolumeid=5_ink,a1id=6_ink,a2id=7_ink,&
&                                       a3id=8_ink,b1id=9_ink,b2id=10_ink,     &
&                                       b3id=11_ink,cnwtid=12_ink,cnxid=13_ink,&
&                                       cnyid=14_ink,rscratch21id=15_ink,      &
&                                       rscratch22id=16_ink,                   &
&                                       rscratch23id=17_ink,                   &
&                                       rscratch24id=18_ink,                   &
&                                       rscratch25id=19_ink,                   &
&                                       rscratch26id=20_ink,                   &
&                                       rscratch27id=21_ink,                   &
&                                       rscratch28id=22_ink,                   &
&                                       cnviscxid=23_ink,cnviscyid=24_ink,     &
&                                       elviscid=25_ink,elmassid=26_ink,       &
&                                       cnmassid=27_ink,rscratch11id=28_ink,   &
&                                       rscratch12id=29_ink,                   &
&                                       rscratch13id=30_ink,                   &
&                                       rscratch14id=31_ink,                   &
&                                       rscratch15id=32_ink,                   &
&                                       rscratch16id=33_ink,                   &
&                                       rscratch17id=34_ink,                   &
&                                       ndxid=35_ink,ndyid=36_ink,             &
&                                       nduid=37_ink,ndvid=38_ink,             &
&                                       indtypeid=39_ink,ielsort1id=40_ink,    &
&                                       ielndid=41_ink,ielelid=42_ink,         &
&                                       ielfcid=43_ink,ielmatid=44_ink,        &
&                                       ielregid=45_ink,ielsort2id=46_ink,     &
&                                       spmassid=47_ink,iscratch11id=48_ink,   &
&                                       zscratch11id=49_ink,                   &
&                                       iellocglobid=50_ink,                   &
&                                       indlocglobid=51_ink,                   &
&                                       cpdensityid=52_ink,cpenergyid=53_ink,  &
&                                       cppressureid=54_ink,cpcs2id=55_ink,    &
&                                       cpvolumeid=56_ink,frvolumeid=57_ink,   &
&                                       cpmassid=58_ink,frmassid=59_ink,       &
&                                       cpviscxid=60_ink,cpviscyid=61_ink,     & 
&                                       cpviscid=62_ink,cpa1id=63_ink,         &
&                                       cpa3id=64_ink,cpb1id=65_ink,           &
&                                       cpb3id=66_ink,icpmatid=67_ink,         &
&                                       icpnextid=68_ink,icpprevid=69_ink,     &
&                                       imxelid=70_ink,imxfcpid=71_ink,        &
&                                       imxncpid=72_ink,icpscratch11id=73_ink, &
&                                       icpscratch12id=74_ink,                 &
&                                       rcpscratch11id=75_ink,                 &
&                                       rcpscratch21id=76_ink,                 &
&                                       rcpscratch22id=77_ink,                 &
&                                       rcpscratch23id=78_ink,                 &
&                                       rcpscratch24id=79_ink

END MODULE dataAPI_id_mod

MODULE dataAPI_dtstepid_mod

  USE dataAPI_id_mod,ONLY: scratchid=>rscratch11id,cnvid=>rscratch22id,        &
&                          ellengthid=>rscratch12id,cnuid=>rscratch21id,       &
&                          duid=>rscratch23id,dvid=>rscratch24id,              &
&                          dxid=>rscratch25id,dyid=>rscratch26id,              &
&                          storeid=>rscratch27id

END MODULE dataAPI_dtstepid_mod

MODULE dataAPI_lagstepid_mod

  USE dataAPI_id_mod,ONLY: cnuid=>rscratch21id,cnvid=>rscratch22id,            &
&                          cnfxid=>rscratch23id,cnfyid=>rscratch24id,          &
&                          elenergy0id=>rscratch11id,ndareaid=>rscratch12id,   &
&                          ndmassid=>rscratch13id,ndubarid=>rscratch14id,      &
&                          ndvbarid=>rscratch15id,cpfxid=>rcpscratch21id,      &
&                          cpfyid=>rcpscratch22id,cpuid=>rscratch23id,         &
&                          cpvid=>rscratch24id

END MODULE dataAPI_lagstepid_mod

MODULE dataAPI_alestepid_mod

  USE dataAPI_id_mod,ONLY: store1id=>rscratch11id,store2id=>rscratch12id,      &
&                          store3id=>rscratch13id,store4id=>rscratch14id,      &
&                          store5id=>rscratch15id,store6id=>rscratch16id,      &
&                          fcdvid=>rscratch21id,fcdmid=>rscratch22id,          &
&                          fluxid=>rscratch23id,rwork1id=>rscratch24id,        &
&                          rwork2id=>rscratch25id,rwork3id=>rscratch26id,      &
&                          cnuid=>rscratch27id,cnvid=>rscratch28id,            &
&                          indstatusid=>iscratch11id,zactiveid=>zscratch11id

END MODULE dataAPI_alestepid_mod
