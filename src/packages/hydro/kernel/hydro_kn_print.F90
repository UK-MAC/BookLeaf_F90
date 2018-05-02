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
MODULE hydro_kn_print_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN,NLEN

  IMPLICIT NONE

  PUBLIC :: hydro_kn_calc_shortprint,hydro_kn_write_header_shortprint,         &
&           hydro_kn_write_table_shortprint,hydro_kn_write_total_shortprint,   &  
&           hydro_kn_total_shortprint,hydro_kn_average_shortprint,             &
&           hydro_kn_init_shortprint,hydro_kn_longprint

CONTAINS

  SUBROUTINE hydro_kn_init_shortprint(nflag,flag_vol,flag_mass,flag_ke,flag_ie,&
&                                     flag_dmn,flag_dmx,flag_pressure,flag_pmn,&
&                                     flag_pmx)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: nflag
    REAL(KIND=rlk),   DIMENSION(nflag),INTENT(OUT) :: flag_vol,flag_mass,      &
&                                                     flag_ke,flag_ie,flag_dmn,&
&                                                     flag_dmx,flag_pressure,  &
&                                                     flag_pmn,flag_pmx

    ! Initialise arrays
    flag_vol=0.0_rlk
    flag_ie=0.0_rlk
    flag_pressure=0.0_rlk
    flag_pmx=-HUGE(1.0_rlk)
    flag_pmn=HUGE(1.0_rlk)
    flag_mass=0.0_rlk
    flag_ke=0.0_rlk
    flag_dmx=-HUGE(1.0_rlk)
    flag_dmn=HUGE(1.0_rlk)

  END SUBROUTINE hydro_kn_init_shortprint

  SUBROUTINE hydro_kn_calc_shortprint(nflag,nsize,dencut,flag,density,energy,  &
&                                     pressure,volume,mass,cnwt,cnu,cnv,       &
&                                     flag_vol,flag_mass,flag_ke,flag_ie,      &
&                                     flag_dmn,flag_dmx,flag_pressure,flag_pmn,&
&                                     flag_pmx)

    ! Argument list
    INTEGER(KIND=ink),                       INTENT(IN)    :: nflag,nsize
    REAL(KIND=rlk),                          INTENT(IN)    :: dencut
    INTEGER(KIND=ink),DIMENSION(nsize),      INTENT(IN)    :: flag
    REAL(KIND=rlk),   DIMENSION(nsize),      INTENT(IN)    :: energy,density,  &
&                                                             mass,volume,     &
&                                                             pressure
    REAL(KIND=rlk),   DIMENSION(NCORN,nsize),INTENT(IN)    :: cnwt,cnu,cnv
    REAL(KIND=rlk),   DIMENSION(nflag),      INTENT(INOUT) :: flag_vol,        &
&                                                             flag_mass,       &
&                                                             flag_ke,flag_dmn,&
&                                                             flag_dmx,flag_ie,&
&                                                             flag_pressure,   &
&                                                             flag_pmx,flag_pmn
    ! Local
    INTEGER(KIND=ink) :: iflag,ii,ic
    REAL(KIND=rlk)    :: c1,w2,w3,w4

    DO ii=1,nsize
      ! Info
      iflag=flag(ii)
      IF (iflag.LE.0_ink) CYCLE
      ! Condition
      c1=dencut*volume(ii)
      ! Scatter element contributions to flag
      flag_vol(iflag)=flag_vol(iflag)+volume(ii)
      IF (mass(ii).GT.c1) THEN
        w2=mass(ii)
        flag_mass(iflag)=flag_mass(iflag)+w2
        w3=energy(ii)
        w3=w3*w2
        flag_ie(iflag)=flag_ie(iflag)+w3
        w4=pressure(ii)
        w3=w2*w4
        flag_pressure(iflag)=flag_pressure(iflag)+w3
        IF (w4.GT.flag_pmx(iflag)) flag_pmx(iflag)=w4
        IF (w4.LT.flag_pmn(iflag)) flag_pmn(iflag)=w4
        w4=density(ii)
        IF (w4.GT.flag_dmx(iflag)) flag_dmx(iflag)=w4
        IF (w4.LT.flag_dmn(iflag)) flag_dmn(iflag)=w4
        DO ic=1,NCORN
          w2=cnu(ic,ii)
          w3=cnv(ic,ii)
          flag_ke(iflag)=0.5_rlk*cnwt(ic,ii)*w4*(w2*w2+w3*w3)+flag_ke(iflag)
        ENDDO
      ENDIF
    ENDDO

  END SUBROUTINE hydro_kn_calc_shortprint

  SUBROUTINE hydro_kn_write_header_shortprint(iunit,nstep,time)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN) :: iunit,nstep
    REAL(KIND=rlk),   INTENT(IN) :: time

    ! Print header
    WRITE(iunit,*) ' '
    WRITE(iunit,'(a11,i7,a8,f14.7)')'  Step no. ',nstep,' Time = ',time

  END SUBROUTINE hydro_kn_write_header_shortprint

  SUBROUTINE hydro_kn_write_table_shortprint(nflag,iunit,sheader,sflag,sname,  &
&                                            flag_vol,flag_mass,flag_ie,       &
&                                            flag_ke,flag_pressure,flag_pmn,   &
&                                            flag_pmx,flag_density,flag_dmn,   &
&                                            flag_dmx)

    ! Argument list
    INTEGER(KIND=ink),                   INTENT(IN) :: nflag,iunit
    CHARACTER(LEN=24),                   INTENT(IN) :: sheader
    CHARACTER(LEN=NLEN),                 INTENT(IN) :: sflag
    CHARACTER(LEN=NLEN),DIMENSION(nflag),INTENT(IN) :: sname
    REAL(KIND=rlk),     DIMENSION(nflag),INTENT(IN) :: flag_vol,flag_mass,     &
&                                                      flag_ie,flag_ke,        &
&                                                      flag_pressure,flag_pmn, &
&                                                      flag_pmx,flag_density,  &
&                                                      flag_dmn,flag_dmx
    ! Local
    INTEGER(KIND=ink) :: ii

    ! Print header
    WRITE(iunit,*) ' '
    WRITE(iunit,'(a24)') sheader
    WRITE(iunit,*) ' '
    WRITE(iunit,1001) sflag,'vol','mass','tot ie','tot ke','press','min press',&
&                     'max press','dens','min dens','max dens'

    ! Print table
    DO ii=1,nflag
      WRITE(iunit,1002) sname(ii),flag_vol(ii),flag_mass(ii),flag_ie(ii),      &
                        flag_ke(ii),flag_pressure(ii),flag_pmn(ii),            &
&                       flag_pmx(ii),flag_density(ii),flag_dmn(ii),flag_dmx(ii)
    ENDDO

    ! Formats
 1001 FORMAT(2X,a10,9X,a3,8X,a4,6X,a6,6X,a6,7X,a5,3X,a9,3X,a9,8X,a4,4X,a8,4X,  &
&            a8)
 1002 FORMAT(2X,a10,1p10e12.4)

  END SUBROUTINE hydro_kn_write_table_shortprint

  SUBROUTINE hydro_kn_write_total_shortprint(iunit,tot_vol,tot_mass,tot_ie,    &
&                                            tot_ke,tot_pressure,tot_density)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN) :: iunit
    REAL(KIND=rlk),   INTENT(IN) :: tot_vol,tot_mass,tot_ie,tot_ke,tot_density,&
&                                   tot_pressure

    ! Print totals
    WRITE(iunit,*) ' '
    WRITE(iunit,1001) tot_vol,tot_mass,tot_ie,tot_ke,tot_pressure,tot_density
    WRITE(iunit,1002) tot_ie,tot_ke,tot_ie+tot_ke

    ! Formats
 1001 FORMAT(' Total ',5X,1p5e12.4,24X,1pe12.4,24X,/)
 1002 FORMAT('           total energy',/,' internal ',1pe12.4,/,' kinetic  ',  &
&            1pe12.4,/,' total    ',1pe12.4,/)

  END SUBROUTINE hydro_kn_write_total_shortprint

  SUBROUTINE hydro_kn_total_shortprint(nflag,flag_vol,flag_mass,flag_ie,       &
&                                      flag_ke,flag_pressure,tot_vol,tot_mass, &
&                                      tot_ie,tot_ke,tot_pressure,tot_density)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)  :: nflag
    REAL(KIND=rlk),   DIMENSION(nflag),INTENT(IN)  :: flag_vol,flag_mass,      &
&                                                     flag_ie,flag_ke,         &
&                                                     flag_pressure
    REAL(KIND=rlk),   INTENT(OUT) :: tot_vol,tot_mass,tot_ie,tot_ke,           &
&                                    tot_pressure,tot_density
    ! Local
    INTEGER(KIND=ink) :: ii

    ! initialise
    tot_vol=0.0_rlk
    tot_mass=0.0_rlk
    tot_ie=0.0_rlk
    tot_ke=0.0_rlk
    tot_pressure=0.0_rlk
    tot_density=0.0_rlk

    ! calculate totals
    DO ii=1,nflag
      tot_vol=tot_vol+flag_vol(ii)
      tot_mass=tot_mass+flag_mass(ii)
      tot_ie=tot_ie+flag_ie(ii)
      tot_ke=tot_ke+flag_ke(ii)
      tot_pressure=tot_pressure+flag_pressure(ii)*flag_mass(ii)
    ENDDO
    tot_density=tot_mass/tot_vol
    tot_pressure=tot_pressure/tot_mass

  END SUBROUTINE hydro_kn_total_shortprint

  SUBROUTINE hydro_kn_average_shortprint(nflag,dencut,flag_vol,flag_mass,      &
&                                        flag_density,flag_pressure)

    ! Argument list
    INTEGER(KIND=ink),                 INTENT(IN)    :: nflag
    REAL(KIND=rlk),                    INTENT(IN)    :: dencut
    REAL(KIND=rlk),   DIMENSION(nflag),INTENT(IN)    :: flag_vol,flag_mass
    REAL(KIND=rlk),   DIMENSION(nflag),INTENT(INOUT) :: flag_density,          &
&                                                       flag_pressure
    ! Local
    INTEGER(KIND=ink) :: ii

    DO ii=1,nflag
      IF (flag_vol(ii).GT.0.0_rlk) THEN
        flag_density(ii)=flag_mass(ii)/flag_vol(ii)
      ENDIF
      IF (flag_mass(ii).GT.(dencut*flag_vol(ii))) THEN
        flag_pressure(ii)=flag_pressure(ii)/flag_mass(ii)
      ENDIF
    ENDDO

  END SUBROUTINE hydro_kn_average_shortprint

  SUBROUTINE hydro_kn_longprint(str,iunit,nel,nnd,eldensity,elpressure,        &
&                               elenergy,elvolume,elmass,viscx,viscy,indtype,  &
&                               ndx,ndy,ndu,ndv)

    ! Argument list
    CHARACTER(LEN=*),                      INTENT(IN) :: str
    INTEGER(KIND=ink),                     INTENT(IN) :: iunit,nel,nnd
    REAL(KIND=rlk),   DIMENSION(nel),      INTENT(IN) :: eldensity,elmass,     &
&                                                        elvolume,elenergy,    &
&                                                        elpressure
    REAL(KIND=rlk),   DIMENSION(NCORN,nel),INTENT(IN) :: viscx,viscy
    INTEGER(KIND=ink),DIMENSION(nnd),      INTENT(IN) :: indtype
    REAL(KIND=rlk),   DIMENSION(nnd),      INTENT(IN) :: ndx,ndy,ndu,ndv
    ! Local
    INTEGER(KIND=ink) :: ii

    WRITE(iunit,*) TRIM(str)
    WRITE(iunit,*) ' '
    WRITE(iunit,'(a6,5(1X,a12))')'    el','     density','    pressure',       &
&    '          IE','      volume','        mass'
    DO ii=1,nel
      WRITE(iunit,'(i6,5(1X,e12.4))') ii,eldensity(ii),elpressure(ii),         &
&      elenergy(ii),elvolume(ii),elmass(ii)
    ENDDO
    WRITE(iunit,*) ' '
    WRITE(iunit,'(a6,8(1X,a12))')'    el','    viscx(1)','    viscx(2)',       &
&    '    viscx(3)','    viscx(4)','    viscy(1)','    viscy(2)',              &
&    '    viscy(3)','    viscy(4)'
    DO ii=1,nel
      WRITE(iunit,'(i6,8(1X,e12.4))') ii,viscx(1,ii),viscx(2,ii),viscx(3,ii),  &
&      viscx(4,ii),viscy(1,ii),viscy(2,ii),viscy(3,ii),viscy(4,ii)
    ENDDO
    WRITE(iunit,*) ' '
    WRITE(iunit,'(2(1X,a6),4(1X,a12))')'  node','  type','           X',       &
&    '           Y','           U','           V'
    DO ii=1,nnd
      WRITE(iunit,'(2(1X,i6),4(1X,e12.4))') ii,indtype(ii),ndx(ii),ndy(ii),    &
&      ndu(ii),ndv(ii)
    ENDDO

  END SUBROUTINE hydro_kn_longprint

END MODULE hydro_kn_print_mod
