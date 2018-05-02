/*
  Crown Copyright 2014 AWE.
 
  This file is part of Bookleaf.
 
  Bookleaf is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License, or (at your option)
  any later version.
 
  Bookleaf is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
  details.
 
  You should have received a copy of the GNU General Public License along with
  Bookleaf. If not, see http://www.gnu.org/licenses/.
*/
 
#ifndef _UTILS_HEADER
#define _UTILS_HEADER

#ifdef __cplusplus
extern "C" {
#endif

#define UTILS_SUCCESS (0)
#define UTILS_FAIL   (-1)

#define UTILS_FAIL_NOACCESS  (-10)
#define UTILS_FAIL_EXIST     (-11)
#define UTILS_FAIL_ISDIR     (-12)
#define UTILS_FAIL_NOTEXIST  (-13)
#define UTILS_FAIL_NOTDIR    (-14)
#define UTILS_FAIL_NOTEMPTY  (-15)
#define UTILS_FAIL_STRING    (-20)
#define UTILS_FAIL_NOMATCH   (-21)
#define UTILS_FAIL_F90API    (-50)

extern int UTILS_mkdir(const char *path);

extern int UTILS_ln(const char *path, const char *link_path);

#ifdef __cplusplus
}
#endif

#endif
