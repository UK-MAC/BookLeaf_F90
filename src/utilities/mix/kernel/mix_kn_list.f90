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
MODULE mix_kn_list_mod

  USE dataAPI_kinds_mod,  ONLY: ink,rlk
  USE dataAPI_params_mod, ONLY: NCORN
  USE utils_kn_copy_mod,  ONLY: utils_kn_copy
  USE utils_kn_sort_mod,  ONLY: utils_kn_sort

  IMPLICIT NONE

  INTERFACE mix_kn_flattenquant
    MODULE PROCEDURE mix_kn_flattenrquant,mix_kn_flatteniquant
  END INTERFACE mix_kn_flattenquant

  PRIVATE :: mix_kn_copystate,mix_kn_flattenrquant,mix_kn_flatteniquant
  PUBLIC  :: mix_kn_addel,mix_kn_deleteel,mix_kn_addcp,mix_kn_deletecp,        &
&            mix_kn_flattenindex,mix_kn_flattenlist,mix_kn_flattenquant

CONTAINS

  SUBROUTINE mix_kn_addel(iel,nel,nmx,ielmat,imxel,imxncp,imxfcp)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: iel,nel,nmx
    INTEGER(KIND=ink),DIMENSION(nel),INTENT(INOUT) :: ielmat
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(INOUT) :: imxel,imxncp,imxfcp

    imxel(nmx)=iel
    imxncp(nmx)=0_ink
    imxfcp(nmx)=0_ink
    ielmat(iel)=-nmx

  END SUBROUTINE mix_kn_addel

  SUBROUTINE mix_kn_deleteel(imix,nmx,ncp,nel,imxfcp,imxncp,imxel,ielmat,      &
&                            icpprev,icpnext,icpmat,cpenergy,cpmass,cpdensity, &
&                            cpvolume,cpcs2,cppressure,cpvisc,cpviscx,cpviscy, &
&                            frvolume,frmass)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: imix,nmx,nel
    INTEGER(KIND=ink),                     INTENT(INOUT) :: ncp
    INTEGER(KIND=ink),DIMENSION(nel),      INTENT(INOUT) :: ielmat
    INTEGER(KIND=ink),DIMENSION(nmx),      INTENT(INOUT) :: imxfcp,imxncp,imxel
    INTEGER(KIND=ink),DIMENSION(ncp),      INTENT(INOUT) :: icpprev,icpnext,   &
&                                                           icpmat
    REAL(KIND=rlk),   DIMENSION(ncp),      INTENT(INOUT) :: cpenergy,cpmass,   &
&                                                           cpdensity,cpvolume,&
&                                                           cpcs2,cppressure,  &
&                                                           cpvisc,frvolume,   &
&                                                           frmass
    REAL(KIND=rlk),   DIMENSION(NCORN,ncp),INTENT(INOUT) :: cpviscx,cpviscy
    ! Local
    INTEGER(KIND=ink) :: ii,lcp,icp,inx

    icp=imxfcp(imix)
    lcp=imxncp(imix)
    DO ii=1,lcp
      CALL mix_kn_deletecp(icp,imix,nmx,ncp,imxfcp,imxncp,icpnext,icpprev,     &
&                          icpmat,cpenergy,cpmass,cpdensity,cpvolume,cpcs2,    &
&                          cppressure,cpvisc,cpviscx,cpviscy,frvolume,frmass,  &
&                          inx)
      icp=inx
      ncp=ncp-1_ink
    ENDDO
    IF (imix.LT.nmx) THEN
      ii=imxel(nmx)
      imxel(imix)=ii
      ielmat(ii)=-imix
      imxncp(imix)=imxncp(nmx)
      ii=imxfcp(nmx)
      imxfcp(imix)=ii
      IF (ii.NE.0_ink) icpprev(ii)=-imix
    ENDIF

  END SUBROUTINE mix_kn_deleteel

  SUBROUTINE mix_kn_addcp(imix,nmx,ncp,imxfcp,imxncp,icpprev,icpnext)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: imix,nmx,ncp
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(INOUT) :: imxfcp,imxncp
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(INOUT) :: icpprev,icpnext
    ! Local
    INTEGER(KIND=ink) :: fmix

    icpprev(ncp)=-imix
    fmix=imxfcp(imix)
    icpnext(ncp)=fmix
    IF (fmix.NE.0_ink) icpprev(fmix)=ncp
    imxfcp(imix)=ncp
    imxncp(imix)=imxncp(imix)+1_ink

  END SUBROUTINE mix_kn_addcp

  SUBROUTINE mix_kn_deletecp(icp,imix,nmx,ncp,imxfcp,imxncp,icpnext,icpprev,   &
&                            icpmat,cpenergy,cpmass,cpdensity,cpvolume,cpcs2,  &
&                            cppressure,cpvisc,cpviscx,cpviscy,frvolume,frmass,&
&                            next)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: icp,imix,nmx,ncp
    INTEGER(KIND=ink),DIMENSION(nmx),      INTENT(INOUT) :: imxfcp,imxncp
    INTEGER(KIND=ink),DIMENSION(ncp),      INTENT(INOUT) :: icpnext,icpprev,   &
&                                                           icpmat
    REAL(KIND=rlk),   DIMENSION(ncp),      INTENT(INOUT) :: cpenergy,cpmass,   &
&                                                           cpdensity,cpcs2,   &
&                                                           cpvolume,cpvisc,   &
&                                                           cppressure,frmass, &
&                                                           frvolume
    REAL(KIND=rlk),   DIMENSION(NCORN,ncp),INTENT(INOUT) :: cpviscx,cpviscy
    INTEGER(KIND=ink),                     INTENT(OUT)   :: next
    ! Local
    INTEGER(KIND=ink) :: prev,ii

    next=icpnext(icp)
    prev=icpprev(icp)
    IF (prev.LT.0_ink) THEN
      imxfcp(-prev)=next
    ELSE
      icpnext(prev)=next
    ENDIF
    IF (next.GT.0_ink) icpprev(next)=prev
    imxncp(imix)=imxncp(imix)-1_ink
    IF (icp.LT.ncp) THEN
      ii=icpnext(ncp)
      icpnext(icp)=ii
      prev=icpprev(ncp)
      icpprev(icp)=prev
      IF (ii.NE.0_ink) icpprev(ii)=icp
      IF (prev.LT.0_ink) THEN
        imxfcp(-prev)=icp
      ELSE
        icpnext(prev)=icp
      ENDIF
      CALL mix_kn_copystate(icp,ncp,ncp,icpmat,cpenergy,cpmass,cpdensity,cpcs2,&
&                           cpvolume,cppressure,cpviscx,cpviscy,cpvisc,        &
&                           frvolume,frmass)
    ENDIF
    IF (next.EQ.ncp) next=icp
    
  END SUBROUTINE mix_kn_deletecp

  SUBROUTINE mix_kn_mxcopystate(iel,icp,nel,ncp,icpmat,cpenergy,cpmass,ielmat, &
&                               elenergy,elmass)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: iel,icp,nel,ncp
    INTEGER(KIND=ink),DIMENSION(nel),INTENT(INOUT) :: ielmat
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(IN)    :: icpmat
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(INOUT) :: elenergy,elmass
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(IN)    :: cpenergy,cpmass

    ielmat(iel)=icpmat(icp)
    elenergy(iel)=cpenergy(icp)
    elmass(iel)=cpmass(icp)

  END SUBROUTINE mix_kn_mxcopystate

  SUBROUTINE mix_kn_elcopystate(iel,icp,nel,ncp,ielmat,elenergy,elmass,        &
&                               elvolume,icpmat,cpenergy,cpmass,cpvolume,      &
&                               frvolume,frmass)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: iel,icp,nel,ncp
    INTEGER(KIND=ink),DIMENSION(nel),INTENT(IN)    :: ielmat
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(INOUT) :: icpmat
    REAL(KIND=rlk),   DIMENSION(nel),INTENT(IN)    :: elenergy,elmass,elvolume
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(INOUT) :: cpenergy,cpmass,cpvolume,&
&                                                     frvolume,frmass    

    icpmat(icp)=ielmat(iel)
    cpenergy(icp)=elenergy(iel)
    cpmass(icp)=elmass(iel)
    cpvolume(icp)=elvolume(iel)
    frvolume(icp)=1.0_rlk
    frmass(icp)=1.0_rlk

  END SUBROUTINE mix_kn_elcopystate

  SUBROUTINE mix_kn_copystate(dest,from,ncp,icpmat,cpenergy,cpmass,cpdensity,  &
&                             cpcs2,cpvolume,cppressure,cpviscx,cpviscy,cpvisc,&
&                             frvolume,frmass)

    ! Argument list
    INTEGER(KIND=ink),                     INTENT(IN)    :: dest,from,ncp
    INTEGER(KIND=ink),DIMENSION(ncp),      INTENT(INOUT) :: icpmat
    REAL(KIND=rlk),   DIMENSION(ncp),      INTENT(INOUT) :: cpenergy,cpmass,   &
&                                                           cpdensity,cpvolume,&
&                                                           cppressure,cpvisc, &
&                                                           frmass,frvolume,   &
&                                                           cpcs2
    REAL(KIND=rlk),   DIMENSION(NCORN,ncp),INTENT(INOUT) :: cpviscx,cpviscy

    icpmat(dest)=icpmat(from)
    cpenergy(dest)=cpenergy(from)
    cpmass(dest)=cpmass(from)
    cpdensity(dest)=cpdensity(from)
    cpvolume(dest)=cpvolume(from)
    cppressure(dest)=cppressure(from)
    cpcs2(dest)=cpcs2(from)
    frvolume(dest)=frvolume(from)
    frmass(dest)=frmass(from)
    cpviscx(1:NCORN,dest)=cpviscx(1:NCORN,from)
    cpviscy(1:NCORN,dest)=cpviscy(1:NCORN,from)
    cpvisc(dest)=cpvisc(from)

  END SUBROUTINE mix_kn_copystate

  SUBROUTINE mix_kn_flattenindex(nmx,ncp,isort,imxfcp,imxncp,icpmat,icpnext,   &
&                                icpprev,iindex)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)  :: nmx,ncp
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(IN)  :: imxfcp,imxncp,isort
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(IN)  :: icpmat,icpnext
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(OUT) :: icpprev,iindex
    ! Local
    INTEGER(KIND=ink) :: imix,icp,lncp,ii,jj,kk,ll

    ii=0_ink
    DO imix=1,nmx
      jj=isort(imix)
      icp=imxfcp(jj)
      lncp=imxncp(jj)
      DO kk=1,lncp
        icpprev(kk)=icpmat(icp)
        icpprev(kk+lncp)=icp
        icp=icpnext(icp)
      ENDDO
      icpprev(2*lncp+1:3*lncp)=utils_kn_sort(icpprev(1:lncp))
      DO kk=1,lncp
        ii=ii+1_ink
        ll=icpprev(2*lncp+kk)
        iindex(ii)=icpprev(lncp+ll)
      ENDDO
    ENDDO

  END SUBROUTINE mix_kn_flattenindex

  SUBROUTINE mix_kn_flattenlist(nmx,ncp,isort,imxfcp,imxel,imxncp,icpprev,     &
&                               icpnext)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: nmx,ncp
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(IN)    :: isort
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(INOUT) :: imxfcp
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(INOUT) :: icpprev
    INTEGER(KIND=ink),DIMENSION(nmx),INTENT(OUT)   :: imxel,imxncp
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(OUT)   :: icpnext
    ! Local
    INTEGER(KIND=ink) :: imix,ii,jj,kk

    CALL utils_kn_copy(nmx,imxel(1:nmx),icpprev(1:nmx))
    CALL utils_kn_copy(nmx,imxncp(1:nmx),imxfcp(1:nmx))

    ii=0_ink
    DO imix=1,nmx
      jj=isort(imix)
      imxel(imix)=icpprev(jj)
      imxncp(imix)=imxfcp(jj)
      DO kk=1,imxncp(imix)
        ii=ii+1_ink
        icpnext(ii)=ii+1_ink
      ENDDO
      icpnext(ii)=0_ink
    ENDDO

    ii=0_ink
    DO imix=1,nmx
      jj=ii+1_ink
      DO kk=1,imxncp(imix)
        ii=ii+1_ink
        icpprev(ii)=ii-1_ink
      ENDDO
      imxfcp(imix)=jj
      icpprev(jj)=-imix
    ENDDO

  END SUBROUTINE mix_kn_flattenlist

  SUBROUTINE mix_kn_flattenrquant(ncp,ilist,rcopy,rquant)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: ncp
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(IN)    :: ilist
    REAL(KIND=rlk),   DIMENSION(ncp),INTENT(INOUT) :: rquant,rcopy
    ! Local
    INTEGER(KIND=ink) :: ii

    CALL utils_kn_copy(ncp,rquant,rcopy)

    DO ii=1,ncp
      rquant(ii)=rcopy(ilist(ii))
    ENDDO

  END SUBROUTINE mix_kn_flattenrquant

  SUBROUTINE mix_kn_flatteniquant(ncp,ilist,icopy,iquant)

    ! Argument list
    INTEGER(KIND=ink),               INTENT(IN)    :: ncp
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(IN)    :: ilist
    INTEGER(KIND=ink),DIMENSION(ncp),INTENT(INOUT) :: iquant,icopy
    ! Local
    INTEGER(KIND=ink) :: ii

    CALL utils_kn_copy(ncp,iquant,icopy)

    DO ii=1,ncp
      iquant(ii)=icopy(ilist(ii))
    ENDDO

  END SUBROUTINE mix_kn_flatteniquant

END MODULE mix_kn_list_mod
