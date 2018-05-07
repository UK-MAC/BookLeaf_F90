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
MODULE utils_kn_sort_mod

  USE dataAPI_kinds_mod,ONLY: ink,rlk,lok

  IMPLICIT NONE

  PUBLIC  :: utils_kn_sort,utils_kn_arth,utils_kn_sortwrapper,                 &
&            utils_kn_arthwrapper,utils_kn_sort0,utils_kn_sort1,               &
&            utils_kn_iusort,utils_kn_binary_search
  PRIVATE :: utils_kn_quicksort,utils_kn_iquicksort,                           &
&            utils_kn_indexr,utils_kn_indexi,utils_kn_sort0ir1,                &
&            utils_kn_sort0ir2,utils_kn_sort0ir1rr2,                           &
&            utils_kn_sort1ir2perm,utils_kn_sort1ir2,                          &
&            utils_kn_sort1rr2

  INTERFACE utils_kn_sort
    MODULE PROCEDURE utils_kn_quicksort,utils_kn_iquicksort
  END INTERFACE utils_kn_sort

  INTERFACE utils_kn_sort0
    MODULE PROCEDURE utils_kn_sort0ir1,utils_kn_sort0ir2,utils_kn_sort0ir1rr2
  END INTERFACE utils_kn_sort0

  INTERFACE utils_kn_sort1
    MODULE PROCEDURE utils_kn_sort1ir2,utils_kn_sort1rr2,utils_kn_sort1ir2perm
  END INTERFACE utils_kn_sort1

CONTAINS

  PURE FUNCTION utils_kn_binary_search(iarr, ival) RESULT(iindx)
      INTEGER(KIND=ink), DIMENSION(:), INTENT(IN) :: iarr
      INTEGER(KIND=ink),               INTENT(IN) :: ival
      INTEGER(KIND=ink)                           :: iindx
      INTEGER(KIND=ink)                           :: ilen,ipvt,iupp,ilow,icur,ii
      INTEGER(KIND=ink), PARAMETER                :: ICUTOFF = 10

      ilen=SIZE(iarr)
      iupp=ilen
      ilow=1_ink

      DO
        ! do a linear search when we hit the cutoff
        IF ((iupp-ilow+1).LE.ICUTOFF) THEN
            DO ii=ilow,iupp
                IF (iarr(ii).EQ.ival) THEN
                    iindx=ii
                    RETURN
                ENDIF
            ENDDO
            EXIT
        ENDIF

        ipvt=(iupp+ilow)/2_ink
        icur=iarr(ipvt)
        IF (icur.EQ.ival) THEN
            iindx=ipvt
            RETURN
        ELSE IF (ival.LT.icur) THEN
            iupp=ipvt-1
        ELSE
            ilow=ipvt+1
        ENDIF
      ENDDO

      iindx = -1_ink
  END FUNCTION

  SUBROUTINE utils_kn_sortwrapper(n,ilist,isort)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)  :: n
    INTEGER(KIND=ink),DIMENSION(n),INTENT(IN)  :: ilist
    INTEGER(KIND=ink),DIMENSION(n),INTENT(OUT) :: isort

    isort(1:n)=utils_kn_iquicksort(ilist(1:n))

  END SUBROUTINE utils_kn_sortwrapper

  PURE FUNCTION utils_kn_arth(first,increment,n) RESULT(iRes)

    ! Argument list
    INTEGER(KIND=ink),INTENT(IN) :: first,increment,n
    ! Result
    INTEGER(KIND=ink),DIMENSION(n) :: iRes
    ! Local
    INTEGER(KIND=ink)           :: k,k2,temp
    INTEGER(KIND=ink),PARAMETER :: npar_arth=16_ink,npar2_arth=8_ink

    IF (n.gt.0_ink) iRes(1)=first
    IF (n.le.npar_arth) THEN
      DO k=2,n
        iRes(k)=iRes(k-1)+increment
      ENDDO
    ELSE
      DO k=2,npar2_arth
        iRes(k)=iRes(k-1)+increment
      ENDDO
      temp=increment*npar2_arth
      k=npar2_arth
      DO
        IF (k.ge.n) EXIT
        k2=k+k
        iRes(k+1:MIN(k2,n))=temp+iRes(1:MIN(k,n-k))
        temp=temp+temp
        k=k2
      ENDDO
    ENDIF

  END FUNCTION utils_kn_arth

  SUBROUTINE utils_kn_arthwrapper(n,first,increment,iRes)

    ! Argument list
    INTEGER(KIND=ink),             INTENT(IN)  :: n,first,increment
    INTEGER(KIND=ink),DIMENSION(n),INTENT(OUT) :: iRes

    iRes(:)=utils_kn_arth(first,increment,n)

  END SUBROUTINE utils_kn_arthwrapper

  PURE FUNCTION utils_kn_quicksort(arr) RESULT(slave)

    ! Argument list
    REAL(KIND=rlk),   DIMENSION(:),INTENT(IN) :: arr
    ! Result    
    INTEGER(KIND=ink),DIMENSION(SIZE(arr))    :: slave
    !  Local
    INTEGER(KIND=ink),DIMENSION(SIZE(arr))    :: index1

    index1=utils_kn_indexr(arr)
    IF (index1(1).EQ.-HUGE(1_ink)) THEN
      slave(1)=-HUGE(1_ink)
      RETURN
    ENDIF
    slave=utils_kn_arth(1_ink,1_ink,SIZE(arr))
    slave=slave(index1)

  END FUNCTION utils_kn_quicksort

  PURE FUNCTION utils_kn_iquicksort(arr) RESULT(slave)

    ! Argument list
    INTEGER(KIND=ink),DIMENSION(:),INTENT(IN) :: arr
    ! Result    
    INTEGER(KIND=ink),DIMENSION(SIZE(arr))    :: slave
    ! Local
    INTEGER(KIND=ink),DIMENSION(SIZE(arr))    :: index1

    index1=utils_kn_indexi(arr)
    IF (index1(1).EQ.-HUGE(1_ink)) THEN
      slave(1)=-HUGE(1_ink)
      RETURN
    ENDIF
    slave=utils_kn_arth(1_ink,1_ink,SIZE(arr))
    slave=slave(index1)

  END FUNCTION utils_kn_iquicksort

  PURE FUNCTION utils_kn_indexr(arr) RESULT(index1)
     
    ! Argument list
    REAL(KIND=rlk),   DIMENSION(:),        INTENT(IN) :: arr
    ! Result    
    INTEGER(KIND=ink),DIMENSION(SIZE(arr)),TARGET     :: index1
    ! Local
    REAL(KIND=rlk)                      :: a
    INTEGER(KIND=ink),PARAMETER         :: nn=7_ink,nstack=500_ink
    INTEGER(KIND=ink)                   :: n,k,i,j,indext,jstack,l,r,sw,swap
    INTEGER(KIND=ink),POINTER           :: p1,p2,p3
    INTEGER(KIND=ink),DIMENSION(nstack) :: istack

    n=SIZE(arr)
    index1=utils_kn_arth(1_ink,1_ink,n)
    jstack=0_ink
    l=1_ink
    r=n
    p1=>NULL()
    p2=>NULL()
    p3=>NULL()
    DO
      IF (r-l.LT.nn) THEN
        DO j=l+1,r
          indext=index1(j)
          a=arr(indext)
          DO i=j-1,l,-1
            IF (arr(index1(i)).le.a) EXIT
            index1(i+1)=index1(i)
          ENDDO
          index1(i+1)=indext
        ENDDO
        IF (jstack.eq.0_ink) RETURN
        r=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2_ink
      ELSE
        k=(l+r)/2_ink
        swap=index1(k)
        index1(k)=index1(l+1)
        index1(l+1)=swap
        p1=>index1(l)
        p2=>index1(r)
        p3=>index1(l+1)
        IF (arr(p2).LT.arr(p1)) THEN
          sw=p1
          p1=p2
          p2=sw
        ENDIF
        IF (arr(p2).LT.arr(p3)) THEN
          sw=p3
          p3=p2
          p2=sw
        ENDIF
        IF (arr(p3).LT.arr(p1)) THEN
          sw=p1
          p1=p3
          p3=sw
        ENDIF
        i=l+1_ink
        j=r
        indext=index1(l+1)
        a=arr(indext)
        DO
          DO
            i=i+1_ink
            IF (arr(index1(i)).ge.a) EXIT
          ENDDO
          DO
            j=j-1_ink
            IF (arr(index1(j)).le.a) EXIT
          ENDDO
          IF (j.LT.i) EXIT
          swap=index1(i)
          index1(i)=index1(j)
          index1(j)=swap
        ENDDO
        index1(l+1)=index1(j)
        index1(j)=indext
        jstack=jstack+2_ink
        IF (jstack.gt.nstack) THEN
          index1(1)=-HUGE(1_ink)
          RETURN
        ENDIF
        IF (r-i+1.ge.j-1) THEN
          istack(jstack)=r
          istack(jstack-1)=i
          r=j-1_ink
        ELSE
          istack(jstack)=j-1_ink
          istack(jstack-1)=l
          l=i
        ENDIF
      ENDIF
    ENDDO

  END FUNCTION utils_kn_indexr

  PURE FUNCTION utils_kn_indexi(arr) RESULT(index1)

    ! Argument list
    INTEGER(KIND=ink),DIMENSION(:),        INTENT(in) :: arr
    ! Result    
    INTEGER(KIND=ink),DIMENSION(SIZE(arr)),TARGET     :: index1
    ! Local
    INTEGER(KIND=ink)                   :: a
    INTEGER(KIND=ink),PARAMETER         :: nn=7_ink,nstack=500_ink
    INTEGER(KIND=ink)                   :: n,k,i,j,indext,jstack,l,r,sw,swap
    INTEGER(KIND=ink),POINTER           :: p1,p2,p3
    INTEGER(KIND=ink),DIMENSION(nstack) :: istack

    n=SIZE(arr)
    index1=utils_kn_arth(1_ink,1_ink,n)
    jstack=0_ink
    l=1_ink
    r=n
    p1=>NULL()
    p2=>NULL()
    p3=>NULL()
    DO
      IF (r-l.LT.nn) THEN
        DO j=l+1,r
          indext=index1(j)
          a=arr(indext)
          DO i=j-1,l,-1
            IF (arr(index1(i)).le.a) EXIT
            index1(i+1)=index1(i)
          ENDDO
          index1(i+1)=indext
        ENDDO
        IF (jstack.eq.0_ink) RETURN
        r=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2_ink
      ELSE
        k=(l+r)/2_ink
        swap=index1(k)
        index1(k)=index1(l+1)
        index1(l+1)=swap
        p1=>index1(l)
        p2=>index1(r)
        p3=>index1(l+1)
        IF (arr(p2).LT.arr(p1)) THEN
          sw=p1
          p1=p2
          p2=sw
        ENDIF
        IF (arr(p2).LT.arr(p3)) THEN
          sw=p3
          p3=p2
          p2=sw
        ENDIF
        IF (arr(p3).LT.arr(p1)) THEN
          sw=p1
          p1=p3
          p3=sw
        ENDIF
        i=l+1_ink
        j=r
        indext=index1(l+1)
        a=arr(indext)
        DO
          DO
            i=i+1_ink
            IF (arr(index1(i)).ge.a) EXIT
          ENDDO
          DO
            j=j-1_ink
            IF (arr(index1(j)).le.a) EXIT
          ENDDO
          IF (j.LT.i) EXIT
          swap=index1(i)
          index1(i)=index1(j)
          index1(j)=swap
        ENDDO
        index1(l+1)=index1(j)
        index1(j)=indext
        jstack=jstack+2_ink
        IF (jstack.gt.nstack) THEN
          index1(1)=-HUGE(1_ink)
          RETURN
        ENDIF
        IF (r-i+1.ge.j-1) THEN
          istack(jstack)=r
          istack(jstack-1)=i
          r=j-1_ink
        ELSE
          istack(jstack)=j-1_ink
          istack(jstack-1)=l
          l=i
        ENDIF
      ENDIF
    ENDDO

  END FUNCTION utils_kn_indexi

  SUBROUTINE utils_kn_sort1ir2(iarray,icol)

    ! Argument list
    INTEGER(KIND=ink),DIMENSION(:,:),INTENT(INOUT) :: iarray
    INTEGER(KIND=ink),DIMENSION(:),  INTENT(IN)    :: icol
    ! Local
    INTEGER(KIND=ink),DIMENSION(SIZE(iarray,1))    :: itemp
    INTEGER(KIND=ink) :: l,ir,i,j,k,n,m
    LOGICAL(KIND=lok) :: z

    n=SIZE(iarray,2)
    m=SIZE(icol)
    IF (n.LT.2_ink) RETURN
    l=n/2+1_ink
    ir=n
    DO
      IF (l.GT.1_ink) THEN
        l=l-1_ink
        itemp=iarray(:,l)
      ELSE
        itemp=iarray(:,ir)
        iarray(:,ir)=iarray(:,1)
        ir=ir-1_ink
        IF (ir.EQ.1_ink) THEN
          iarray(:,1)=itemp
          RETURN
        ENDIF
      ENDIF
      i=l
      j=2*l
      DO
        IF (j.GT.ir) EXIT
        IF (j.LT.ir) THEN
          z=.FALSE._lok
          DO k=1,m
            IF (icol(k).GT.0_ink) THEN
              IF (iarray(icol(k),j).EQ.iarray(icol(k),j+1)) CYCLE
              IF (iarray(icol(k),j).GT.iarray(icol(k),j+1)) EXIT
            ELSE
              IF (ABS(iarray(-icol(k),j)).EQ.ABS(iarray(-icol(k),j+1))) CYCLE
              IF (ABS(iarray(-icol(k),j)).GT.ABS(iarray(-icol(k),j+1))) EXIT
            ENDIF
            z=.TRUE._lok
            EXIT
          ENDDO
          IF (z) j=j+1_ink
        ENDIF
        z=.FALSE._lok
        DO k=1,m
          IF (icol(k).GT.0_ink) THEN
            IF (itemp(icol(k)).EQ.iarray(icol(k),j)) CYCLE
            IF (itemp(icol(k)).GT.iarray(icol(k),j)) EXIT
          ELSE
            IF (ABS(itemp(-icol(k))).EQ.ABS(iarray(-icol(k),j))) CYCLE
            IF (ABS(itemp(-icol(k))).GT.ABS(iarray(-icol(k),j))) EXIT
          ENDIF
          z=.TRUE._lok
          EXIT
        ENDDO
        IF (z) THEN
          iarray(:,i)=iarray(:,j)
          i=j
          j=2*j
        ELSE
          j=ir+1_ink
        ENDIF
      ENDDO
      iarray(:,i)=itemp
    ENDDO

  END SUBROUTINE utils_kn_sort1ir2

  SUBROUTINE utils_kn_sort1ir2perm(iarray,icol,indx)

    ! Argument list
    INTEGER(KIND=ink),DIMENSION(:,:),INTENT(INOUT) :: iarray
    INTEGER(KIND=ink),DIMENSION(:),  INTENT(IN)    :: icol
    INTEGER(KIND=ink),DIMENSION(:),  INTENT(INOUT) :: indx
    ! Local
    INTEGER(KIND=ink),DIMENSION(SIZE(iarray,1))    :: itemp
    INTEGER(KIND=ink) :: l,ir,i,j,k,it,n,m
    LOGICAL(KIND=lok) :: z

    n=SIZE(iarray,2)
    m=SIZE(icol)
    IF (n.LT.2_ink) RETURN
    l=n/2+1_ink
    ir=n
    DO
      IF (l.GT.1_ink) THEN
        l=l-1_ink
        itemp=iarray(:,l)
        it=indx(l)
      ELSE
        itemp=iarray(:,ir)
        it=indx(ir)
        iarray(:,ir)=iarray(:,1)
        indx(ir)=indx(1)
        ir=ir-1_ink
        IF (ir.EQ.1_ink) THEN
          iarray(:,1)=itemp
          indx(1)=it
          RETURN
        ENDIF
      ENDIF
      i=l
      j=2*l
      DO
        IF (j.GT.ir) EXIT
        IF (j.LT.ir) THEN
          z=.FALSE._lok
          DO k=1,m
            IF (icol(k).GT.0_ink) THEN
              IF (iarray(icol(k),j).EQ.iarray(icol(k),j+1)) CYCLE
              IF (iarray(icol(k),j).GT.iarray(icol(k),j+1)) EXIT
            ELSE
              IF (ABS(iarray(-icol(k),j)).EQ.ABS(iarray(-icol(k),j+1))) CYCLE
              IF (ABS(iarray(-icol(k),j)).GT.ABS(iarray(-icol(k),j+1))) EXIT
            ENDIF
            z=.TRUE._lok
            EXIT
          ENDDO
          IF (z) j=j+1_ink
        ENDIF
        z=.FALSE._lok
        DO k=1,m
          IF (icol(k).GT.0_ink) THEN
            IF (itemp(icol(k)).EQ.iarray(icol(k),j)) CYCLE
            IF (itemp(icol(k)).GT.iarray(icol(k),j)) EXIT
          ELSE
            IF (ABS(itemp(-icol(k))).EQ.ABS(iarray(-icol(k),j))) CYCLE
            IF (ABS(itemp(-icol(k))).GT.ABS(iarray(-icol(k),j))) EXIT
          ENDIF
          z=.TRUE._lok
          EXIT
        ENDDO
        IF (z) THEN
          iarray(:,i)=iarray(:,j)
          indx(i)=indx(j)
          i=j
          j=2*j
        ELSE
          j=ir+1_ink
        ENDIF
      ENDDO
      iarray(:,i)=itemp
      indx(i)=it
    ENDDO

  END SUBROUTINE utils_kn_sort1ir2perm

  SUBROUTINE utils_kn_sort1rr2(rarray,icol)

    ! Argument list
    REAL(KIND=rlk),   DIMENSION(:,:),INTENT(INOUT) :: rarray
    INTEGER(KIND=ink),DIMENSION(:),  INTENT(IN)    :: icol
    ! Local
    REAL(KIND=rlk),   DIMENSION(SIZE(rarray,1))    :: itemp
    INTEGER(KIND=ink) :: l,ir,i,j,k,n,m
    LOGICAL(KIND=lok) :: z

    n=SIZE(rarray,2)
    m=SIZE(icol)
    IF (n.LT.2_ink) RETURN
    l=n/2+1_ink
    ir=n
    DO
      IF (l.GT.1_ink) THEN
        l=l-1_ink
        itemp=rarray(:,l)
      ELSE
        itemp=rarray(:,ir)
        rarray(:,ir)=rarray(:,1)
        ir=ir-1_ink
        IF (ir.EQ.1_ink) THEN
          rarray(:,1)=itemp
          RETURN
        ENDIF
      ENDIF
      i=l
      j=2*l
      DO
        IF (j.GT.ir) EXIT
        IF (j.LT.ir) THEN
          z=.FALSE._lok
          DO k=1,m
            IF (icol(k).GT.0_ink) THEN
              IF (rarray(icol(k),j).EQ.rarray(icol(k),j+1)) CYCLE
              IF (rarray(icol(k),j).GT.rarray(icol(k),j+1)) EXIT
            ELSE
              IF (ABS(rarray(-icol(k),j)).EQ.ABS(rarray(-icol(k),j+1))) CYCLE
              IF (ABS(rarray(-icol(k),j)).GT.ABS(rarray(-icol(k),j+1))) EXIT
            ENDIF
            z=.TRUE._lok
            EXIT
          ENDDO
          IF (z) j=j+1_ink
        ENDIF
        z=.FALSE._lok
        DO k=1,m
          IF (icol(k).GT.0_ink) THEN
            IF (itemp(icol(k)).EQ.rarray(icol(k),j)) CYCLE
            IF (itemp(icol(k)).GT.rarray(icol(k),j)) EXIT
          ELSE
            IF (ABS(itemp(-icol(k))).EQ.ABS(rarray(-icol(k),j))) CYCLE
            IF (ABS(itemp(-icol(k))).GT.ABS(rarray(-icol(k),j))) EXIT
          ENDIF
          z=.TRUE._lok
          EXIT
        ENDDO
        IF (z) THEN
          rarray(:,i)=rarray(:,j)
          i=j
          j=2*j
        ELSE
          j=ir+1_ink
        ENDIF
      ENDDO
      rarray(:,i)=itemp
    ENDDO

  END SUBROUTINE utils_kn_sort1rr2

  SUBROUTINE utils_kn_sort0ir1(iarray)

    ! Argument list
    INTEGER(KIND=ink),DIMENSION(:),INTENT(INOUT) :: iarray
    ! Local
    INTEGER(KIND=ink) :: l,ir,i,j,itemp,n
    
    n=SIZE(iarray)
    IF (n.LT.2_ink) RETURN
    l=n/2+1_ink
    ir=n
10  CONTINUE
    IF (l.GT.1_ink) THEN
      l=l-1_ink
      itemp=iarray(l)
    ELSE
      itemp=iarray(ir)
      iarray(ir)=iarray(1)
      ir=ir-1_ink
      IF (ir.EQ.1_ink) THEN
        iarray(1)=itemp
        RETURN
      ENDIF
    ENDIF
    i=l
    j=2*l
20  CONTINUE
    IF (j.LE.ir) THEN
      IF (j.LT.ir) THEN
        IF (iarray(j).LT.iarray(j+1)) j=j+1_ink
      ENDIF
      IF (itemp.LT.iarray(j)) THEN
        iarray(i)=iarray(j)
        i=j
        j=2*j
      ELSE
        j=ir+1_ink
      ENDIF
      GOTO 20
    END IF
    iarray(i)=itemp
    GOTO 10

  END SUBROUTINE utils_kn_sort0ir1

  SUBROUTINE utils_kn_sort0ir1rr2(iarray,rr2)

    ! Argument list
    INTEGER(KIND=ink),DIMENSION(:),  INTENT(INOUT) :: iarray
    REAL(KIND=rlk),   DIMENSION(:,:),INTENT(INOUT) :: rr2
    ! Local
    INTEGER(KIND=ink) :: l,ir,i,j,itemp,n
    REAL(KIND=rlk),   DIMENSION(SIZE(rr2,1)) :: rtmp

    n=SIZE(iarray)
    IF (n.LT.2_ink) RETURN
    l=n/2+1_ink
    ir=n
10  CONTINUE
    IF (l.GT.1_ink) THEN
      l=l-1_ink
      itemp=iarray(l)
      rtmp=rr2(:,l)
    ELSE
      itemp=iarray(ir)
      rtmp=rr2(:,ir)
      iarray(ir)=iarray(1)
      rr2(:,ir)=rr2(:,1)
      ir=ir-1_ink
      IF (ir.EQ.1_ink) THEN
        iarray(1)=itemp
        rr2(:,1)=rtmp
        RETURN
      ENDIF
    ENDIF
    i=l
    j=2*l
20  CONTINUE
    IF (j.LE.ir) THEN
      IF (j.LT.ir) THEN
        IF (iarray(j).LT.iarray(j+1)) j=j+1_ink
      ENDIF
      IF (itemp.LT.iarray(j)) THEN
        iarray(i)=iarray(j)
        rr2(:,i)=rr2(:,j)
        i=j
        j=2*j
      ELSE
        j=ir+1_ink
      ENDIF
      GOTO 20
    ENDIF
    iarray(i)=itemp
    rr2(:,i)=rtmp
    GOTO 10

  END SUBROUTINE utils_kn_sort0ir1rr2

  SUBROUTINE utils_kn_sort0ir2(iarray)

    ! Argument list
    INTEGER(KIND=ink),DIMENSION(:,:),INTENT(INOUT) :: iarray
    ! Local
    INTEGER(KIND=ink),DIMENSION(SIZE(iarray,1))    :: itemp
    INTEGER(KIND=ink) :: l,ir,i,j,n
    
    n=SIZE(iarray,2)
    IF (n.LT.2) RETURN
    l=n/2+1_ink
    ir=n
10  CONTINUE
    IF (l.GT.1_ink) THEN
      l=l-1_ink
      itemp=iarray(:,l)
    ELSE
      itemp=iarray(:,ir)
      iarray(:,ir)=iarray(:,1)
      ir=ir-1_ink
      IF (ir.EQ.1_ink) THEN
        iarray(:,1)=itemp
        RETURN
      ENDIF
    ENDIF
    i=l
    j=2*l
20  CONTINUE
    IF (j.LE.ir) THEN
      IF (j.LT.ir) THEN
        IF (iarray(1,j).LT.iarray(1,j+1)) j=j+1_ink
      ENDIF
      IF (itemp(1).LT.iarray(1,j)) THEN
        iarray(:,i)=iarray(:,j)
        i=j
        j=2*j
      ELSE
        j=ir+1_ink
      ENDIF
      GOTO 20
    ENDIF
    iarray(:,i)=itemp
    GOTO 10

  END SUBROUTINE utils_kn_sort0ir2

  SUBROUTINE utils_kn_iusort(ia,nn)

    ! Argument list
    INTEGER(KIND=ink),DIMENSION(:),INTENT(INOUT) :: ia
    INTEGER(KIND=ink),             INTENT(OUT)   :: nn
    ! Local
    INTEGER(KIND=ink) :: n,l,ir,i,j,ira,ip,ic  

    n=SIZE(ia) 

    IF (n.GT.1_ink) THEN
      l=n/2+1_ink
      ir=n
      mloop:DO
        IF (l.GT.1_ink) THEN
          l=l-1_ink
          ira=ia(l)
        ELSE
          ira=ia(ir)
          ia(ir)=ia(1)
          ir=ir-1_ink
          IF (ir.EQ.1_ink) THEN
            ia(1)=ira
            EXIT mloop
          ENDIF
        ENDIF
        i=l
        j=2*l
        DO
          IF (j.GT.ir) EXIT
          IF (j.LT.ir) THEN
            IF (ia(j).LT.ia(j+1)) j=j+1_ink
          ENDIF
          IF (ira.LT.ia(j)) THEN
            ia(i)=ia(j)
            i=j
            j=2*j
          ELSE
            j=ir+1_ink
          ENDIF
        ENDDO
        ia(i)=ira
      ENDDO mloop
    ENDIF

    nn=0_ink
    ip=0_ink
    DO i=1,n
      ic=ia(i)
      ia(i)=0_ink
      IF ((i.EQ.1_ink).OR.(ic.NE.ip)) THEN
        nn=nn+1_ink
        ip=ic
        ia(nn)=ip
      ENDIF
    ENDDO

  END SUBROUTINE utils_kn_iusort

END MODULE utils_kn_sort_mod
