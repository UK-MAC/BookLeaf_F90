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
MODULE geometry_kn_length_mod

  USE dataAPI_kinds_mod, ONLY: ink,rlk
  USE dataAPI_params_mod,ONLY: NCORN

  IMPLICIT NONE

  PUBLIC  :: geometry_kn_dlm,geometry_kn_dln
  PRIVATE :: geometry_kn_denom,geometry_kn_distpp,geometry_kn_distpl

CONTAINS

  PURE FUNCTION geometry_kn_dlm(elx,ely) RESULT(res)

    ! Argument list
    REAL(KIND=rlk),DIMENSION(NCORN),INTENT(IN) :: elx,ely
    ! Result
    REAL(KIND=rlk),DIMENSION(NCORN)            :: res
    ! Local
    REAL(KIND=rlk)                             :: x1,x2,y1,y2

    x1=elx(1)+elx(2)
    x2=elx(3)+elx(4)
    y1=ely(1)+ely(2)
    y2=ely(3)+ely(4)
    x1=0.5_rlk*(x1-x2)
    y1=0.5_rlk*(y1-y2)
    res(1)=x1*x1+y1*y1
    x1=elx(3)+elx(2)
    x2=elx(1)+elx(4)
    y1=ely(3)+ely(2)
    y2=ely(1)+ely(4)
    x1=0.5_rlk*(x1-x2)
    y1=0.5_rlk*(y1-y2)
    res(2)=x1*x1+y1*y1
    res(3)=res(1)
    res(4)=res(2)

  END FUNCTION geometry_kn_dlm

  PURE FUNCTION geometry_kn_dln(elx,ely,zcut) RESULT(res)

    ! Argument list
    REAL(KIND=rlk),DIMENSION(NCORN),INTENT(IN) :: elx,ely
    REAL(KIND=rlk),                 INTENT(IN) :: zcut
    ! Result
    REAL(KIND=rlk),DIMENSION(NCORN)            :: res
    ! Local
    REAL(KIND=rlk)                             :: w1

    w1=geometry_kn_denom(elx(3),ely(3),elx(4),ely(4))
    IF (w1.LT.zcut) THEN
      res(1)=geometry_kn_distpp(elx(1),ely(1),elx(2),ely(2),elx(3),ely(3))
    ELSE
      res(1)=geometry_kn_distpl(elx(1),ely(1),elx(2),ely(2),elx(3),ely(3),     &
&                                   elx(4),ely(4))/w1
    ENDIF
    w1=geometry_kn_denom(elx(4),ely(4),elx(1),ely(1))
    IF (w1.LT.zcut) THEN
      res(2)=geometry_kn_distpp(elx(2),ely(2),elx(3),ely(3),elx(4),ely(4))
    ELSE
      res(2)=geometry_kn_distpl(elx(2),ely(2),elx(3),ely(3),elx(4),ely(4),     &
&                                   elx(1),ely(1))/w1
    ENDIF
    w1=geometry_kn_denom(elx(1),ely(1),elx(2),ely(2))
    IF (w1.LT.zcut) THEN
      res(3)=geometry_kn_distpp(elx(3),ely(3),elx(4),ely(4),elx(1),ely(1))
    ELSE
      res(3)=geometry_kn_distpl(elx(3),ely(3),elx(4),ely(4),elx(1),ely(1),     &
&                                   elx(2),ely(2))/w1
    ENDIF
    w1=geometry_kn_denom(elx(2),ely(2),elx(3),ely(3))
    IF (w1.LT.zcut) THEN
      res(4)=geometry_kn_distpp(elx(4),ely(4),elx(1),ely(1),elx(2),ely(2))
    ELSE
      res(4)=geometry_kn_distpl(elx(4),ely(4),elx(1),ely(1),elx(2),ely(2),     &
&                                   elx(3),ely(3))/w1
    ENDIF

  END FUNCTION geometry_kn_dln

  PURE FUNCTION geometry_kn_denom(x1,y1,x2,y2) RESULT(res)

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: x1,y1,x2,y2
    ! Result
    REAL(KIND=rlk)            :: res
    ! Local
    REAL(KIND=rlk)            :: w1,w2

    w1=y1-y2
    w2=x1-x2
    res=w1*w1+w2*w2

  END FUNCTION geometry_kn_denom

  PURE FUNCTION geometry_kn_distpp(x3,y3,x4,y4,x1,y1) RESULT(res)

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: x3,y3,x4,y4,x1,y1
    ! Result
    REAL(KIND=rlk)            :: res
    ! Local
    REAL(KIND=rlk)            :: w1,w2

    w1=0.5_rlk*(x3+x4)-x1
    w2=0.5_rlk*(y3+y4)-y1
    res=w1*w1+w2*w2

  END FUNCTION geometry_kn_distpp  

  PURE FUNCTION geometry_kn_distpl(x3,y3,x4,y4,x1,y1,x2,y2) RESULT(res)

    ! Argument list
    REAL(KIND=rlk),INTENT(IN) :: x3,y3,x4,y4,x1,y1,x2,y2
    ! Result
    REAL(KIND=rlk)            :: res

    res=0.5_rlk*(y1-y2)*(x3+x4)+0.5_rlk*(y3+y4)*(x2-x1)+y2*x1-y1*x2
    res=res*res

  END FUNCTION geometry_kn_distpl

END MODULE geometry_kn_length_mod
