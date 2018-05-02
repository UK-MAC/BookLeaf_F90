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
MODULE dataAPI_params_mod

  USE dataAPI_kinds_mod,ONLY: ink
  USE iso_fortran_env,  ONLY: EUNIT=>ERROR_UNIT,OUNIT=>OUTPUT_UNIT,            &
&                             IUNIT=>INPUT_UNIT

  ! Geometry
  INTEGER(KIND=ink),                      PARAMETER,PUBLIC :: NDIM=2_ink,      &
&                                                             NFACE=4_ink,     &
&                                                             NCORN=4_ink
  INTEGER(KIND=ink),DIMENSION(NDIM,NFACE),PARAMETER,PUBLIC :: IFCND=           &
&  RESHAPE([1_ink,2_ink,2_ink,3_ink,3_ink,4_ink,4_ink,1_ink],[NDIM,NFACE])
  ! Error handling
  INTEGER(KIND=ink),                      PARAMETER,PUBLIC :: SUCCESS=0_ink,   &
&                                                             FAILURE=-1_ink,  &  
&                                                             HALT_ALL=1_ink,  &
&                                                             HALT_SINGLE=2_ink
  ! Output streams
  INTEGER,                                PARAMETER,PUBLIC :: ESTREAM=EUNIT,   &
&                                                             OSTREAM=OUNIT,   &
&                                                             ISTREAM=IUNIT
  ! String length
  INTEGER(KIND=ink),                      PARAMETER,PUBLIC :: SLEN=130_ink
  ! Name length
  INTEGER(KIND=ink),                      PARAMETER,PUBLIC :: NLEN=10_ink

END MODULE dataAPI_params_mod

