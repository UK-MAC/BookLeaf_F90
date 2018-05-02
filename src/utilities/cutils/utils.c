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


#include <errno.h>                  /*  System-level error handling and messages     */
#include <stdio.h>                  /*  Standard I/O                                 */
#include <stdarg.h>                 /*  Variable arguments used in verbose routines  */
#include <limits.h>                 /*  Useful system limits, e.g. PATH_MAX          */
#include <stdlib.h>                 /*  malloc,free,getenv,etc.                      */
#include <string.h>                 /*  String handling and conversion               */

#include <sys/types.h>              /*  size_t, uid_t, gid_t, pid_t, etc .           */
#include <sys/stat.h>               /*  struct stat and stat() function              */
#include <sys/utsname.h>            /*  uname() to get OS info                       */

#include <unistd.h>                 /*  UNIX stuff and types getuid()                */
#include <grp.h>                    /*  UNIX group access                            */
#include <pwd.h>                    /*  For passwd struct, getpwuid(), etc.          */
#include <utime.h>                  /*  utime() update time of a file                */
#include <time.h>                   /*  ctime() time format conversion               */

#include <glob.h>                   /*  Globbing to resolve paths with wildcards     */
#include <libgen.h>                 /*  basename() for path splitting                */

#include "utils.h"                 /*  Function prototypes and defined constants    */

#ifndef TRUE
#define TRUE (1==1)
#endif
#ifndef FALSE
#define FALSE (1==0)
#endif

/* Default stdout buffer size = 64 kilobytes */

#ifdef STDOUT_BUFFER
#undef STDOUT_BUFFER
#endif
#define STDOUT_BUFFER 64

static int  last_errno = 0;
static char last_errstr[128];
static char last_desc[128];

static int silent  = TRUE;    /* fail silently by default         */
static int verbose = FALSE;   /* verbosity for debugging & errors */

/*
 * Data structure used to hold list of filenames
 */
struct list_tp {
  char *name;
};

/*
 * Cache of glob results and status
 */
static glob_t glob_res;
static int    glob_status = 0;

/* Internal function prototypes: */
static void UTILS_error(const char *id, const char *desc);
static int UTILS_fail();
static int UTILS_isdir(const char *path, const char *caller);
static int ut_forall(const char *srcpath, const char *tgtpath, const int tgttype, const char *caller, int (*func)(const char *, const char *));
static int ut_listfree(struct list_tp **list);
static int ut_listget(const char *pattern, const char *caller, size_t *listlen, struct list_tp **list);

static int ut_mkdir(const char *path, const mode_t mode)
{
  
  char pathcpy[PATH_MAX];
  char *lastslash;
  
  
  (void) strcpy(pathcpy, path);
  
  lastslash = strrchr(pathcpy, (int)'/');
  
  if (lastslash != NULL) {
    
    int isdir;
    
    /* If the dirname doesn't exist, call recursively to create it */
    
    *lastslash = '\0';
    
    isdir = UTILS_isdir(pathcpy, "UTILS_mkdir");
    if (isdir < 0) {
      char errstr[128];
      (void) sprintf(errstr, "UTILS_mkdir error on UTILS_isdir() [201]");
      (void) UTILS_error(errstr, path);
      return (UTILS_fail());
    }
    if (!isdir) {
      if (ut_mkdir(pathcpy, mode) != UTILS_SUCCESS) {
	return (UTILS_fail());
      }
    }
    
  }
  
  errno = 0;
  if (mkdir(path, mode) < 0) {
    (void) UTILS_error("UTILS_mkdir error [1]", NULL);
    return (UTILS_fail());
  }
  
/*  (void) UTILS_verbose("UTILS_mkdir", "mkdir %s", path); */
  
  return (UTILS_SUCCESS);
}

int UTILS_mkdir(const char *path)
{
  mode_t mode = S_IRWXU | S_IRWXG | S_ISGID;
  
  /* works like mkdir -p */
  
  return (ut_mkdir(path, mode));
}

static int ut_ln(const char *path, const char *link_path)
{
  
  char rpath[PATH_MAX];
  
  errno = 0;
  if (realpath(path, rpath) == NULL) {
    (void) UTILS_error("UTILS_ln error [1]", NULL);
    return (UTILS_fail());
  }    
  
  errno = 0;
  if (symlink(rpath, link_path) < 0) {
    (void) UTILS_error("UTILS_ln error [2]", NULL);
    return (UTILS_fail());
  }
  
/*  (void) UTILS_verbose("UTILS_ln", "ln -s %s %s", path, link_path); */
  
  return (UTILS_SUCCESS);
}

int UTILS_ln(const char *path, const char *link_path)
{
  
  return (ut_forall(path, link_path, 1, "UTILS_ln", ut_ln));
}

static char *UTILS_copyfstr(const char *fstr, int len)
{
  char *t;

  t = NULL;
  t = (char *)malloc(sizeof(char) * (size_t)(len + 1));

  if (t != NULL) {
    (void) memcpy((void *)t, (const void *)fstr, (size_t)len);
    t[len]='\0';    /* append null terminator at end of actual string */
  }

  return (t);
}

static void UTILS_error(const char *id, const char *desc)
{
  /* Hold onto last error details for future use
     - may be required in later call to UTILS_error() */
 
  last_errno = errno;
  (void) strncpy(last_errstr, id,   strlen(last_errstr));
  (void) strncpy(last_desc,   desc, strlen(last_desc));
 
 
  /* If silent mode is off, or verbose mode is on, stream out the error message */
 
  if (!silent || verbose) {
    errno = last_errno;    /* make sure perror is called for the right errno         */
                           /* strncpy() above may have obliterated the value we want */
   
    /* If errno is zero, the error is just a warning */
   
    if (errno == 0) {
      (void) fprintf(stderr, "%s\n", id);
    } else {
      (void) perror(id);   /* writes 'UNIX' error message to stderr */
    }
   
    if (desc != NULL) {
      (void) fprintf(stderr, "%s\n", desc);
    }
  }
 
  return;
}

static int UTILS_fail()
{
  /* An error has probably occured, so return an appropriate error code       */
  /* - If errno is useful, use that, otherwise just return the standard fail  */
 
  switch (last_errno) {
  case EACCES:
    return (UTILS_FAIL_NOACCESS);
  case EEXIST:
    return (UTILS_FAIL_EXIST);
  case EISDIR:
    return (UTILS_FAIL_ISDIR);
  case ENOENT:
    return (UTILS_FAIL_NOTEXIST);
  case ENOTDIR:
    return (UTILS_FAIL_NOTDIR);
  case ENOTEMPTY:
    return (UTILS_FAIL_NOTEMPTY);
  default:                 /* No errno of use */
    return (UTILS_FAIL);
  }
}

static int UTILS_isdir(const char *path, const char *caller)
{
  /* returns true if a given path is identified as a directory */

  struct stat statbuf;
 
 
  if (NULL == path) {
    return (0);
  }
 
  errno = 0;
  if (stat(path, &statbuf) <  0) {
   
    /* Directory may not exist, but this is not a problem, so ignore that case */
   
    if (errno == ENOENT) {
      statbuf.st_mode = 0;           /* file does not exist so set mode to 0 */
    }
    else {
      return (-1);                   /* path is erronous */
    }
  }
 
  if (S_ISDIR(statbuf.st_mode)) {
    return (1);                     /* path is a directory */
  }
  else {
    return (0);                     /* path is not a directory */
  }
}

static int ut_forall(const char *srcpath, const char *tgtpath, const int tgttype,
         const char *caller, int (*func)(const char *, const char *))

{
  /* - calls a given function for all files matching patterns */
  char *srcname;
  const char *tgtptr;
  char tgtdir[PATH_MAX];
  char errstr[128];
 
  int ierr;
  int rerr;
  int isdir;
 
  struct list_tp *slist = NULL;
  size_t scount;
  size_t tcount;
  size_t fi;
 
 
 
  /* Check the target path arg, as it influences behaviour
   *
   * If more than one match found for srcpath, then it must have contained a wildcard
   * and tgtpath may need to be a directory, depending on tgttype arg
   *
   *   tgttype == 0  - tgtpath is dummy and not used  (e.g. ls)
   *
   *   tgttype == 1  - tgtpath can be a directory or a single file, but must be a
   *                   directory if srcpath resolves to multiple files
   *                   (e.g. cp, mv,...)
   *
   *   tgttype == 2  - tgtpath cannot be a directory and multiple source files
   *                   can perform on a single target file (e.g. cat)
   *
   *   tgttype == 3  - tgtpath is not a path (e.g. chmod)
   *  
   */
 
  isdir  = FALSE;
  tgtptr = tgtpath;
 
  switch (tgttype) {
   
  case 0:
    tgtptr = NULL;
    break;
   
  case 1:
  case 2:
   
    /* Check that the target path only resolves to a single name or none at all
     * - must be 1 or 0 as must be a single existing file/dir or a non-existing file
     */
   
    ierr = ut_listget(tgtpath, caller, &tcount, NULL);
    if (ierr != 0) {
      (void) UTILS_error(caller, "failed on ut_listget() [801]");
      return (1);
    }
    ierr = ut_listfree(NULL);
    if (ierr != 0) {
      (void) UTILS_error(caller, "failed on ut_listfree() [802]");
      return (1);
    }
    if (tcount > 1) {
      sprintf(errstr, "%s error [501]: %s", caller, tgtpath);
      errno = EINVAL;
      (void) UTILS_error(errstr, "target argument invalid - resolves to too many files");
      return (1);
    }
   
    /* Is the target path an existing directory? */
    isdir = UTILS_isdir(tgtpath, caller);
    if (isdir < 0) {
      (void) sprintf(errstr, "%s error on UTILS_isdir() [803]: %s", caller, tgtpath);
      (void) UTILS_error(errstr, NULL);
      return (1);
    }
   
    /* Error if directory not allowed as target */
    if ((tgttype == 2) && isdir) {
      sprintf(errstr, "%s error [804]: %s", caller, tgtpath);
      errno = EISDIR;
      (void) UTILS_error(errstr, "target is a directory");
      return (1);
    }
   
    /* tgtpre = tgtpath - already assigned */
    break;
   
  case 3:
    /* no action required - isdir and tgtptr already assigned */
    break;
   
  default:
    break;
  }
 
 
  /* Get the files matching the srcpath */
  /*   slist returned here will automatically be in alphanumeric (linux) order */
  ierr = ut_listget(srcpath, caller, &scount, &slist);
  if (ierr != 0) {
    (void) UTILS_error(caller, "failed on ut_listget() [805]");
    return (1);
  }
 
  /* Check some file/dir names were matched and return & error if not */
  if (0 == scount) {
    ierr = ut_listfree(&slist);
    sprintf(errstr, "%s error [806]: %s", caller, srcpath);
    errno = ENOENT;
    (void) UTILS_error(errstr, "no matching files/directories");
    return (1);
  }
 
 
  if (isdir) {
    (void) strcpy(tgtdir, tgtpath);
    (void) strcat(tgtdir, "/");
  }
  else {

    if ((scount > 1) && (tgttype == 1)) {
      ierr = ut_listfree(&slist);
      sprintf(errstr, "%s error [806]: %s", caller, tgtpath);
      errno = ENOTDIR;
      (void) UTILS_error(errstr, "target is not a directory");
      return (1);
    }
  }
 
  /* Return code at end of routine                                           */
  /* - if any failure occurs, only the first failure's code will be returned */
 
  rerr = UTILS_SUCCESS;
 
  for (fi=0; fi<scount; ++fi) {
   
    srcname = slist[fi].name;
   
    /* If target is a directory, then derive target filename from directory and
     * source filename */
   
    if (isdir) {
     
      char filename[PATH_MAX];
      char tgtname[PATH_MAX];
     
      (void) strcpy(filename, srcname);   /*  Copy srcpath as the buffer will be  */
                                          /*  overwritten in the basename() call  */
      (void) strcpy(tgtname, tgtdir);
      (void) strcat(tgtname, basename(filename));
     
      tgtptr = tgtname;
    }
   
    /* Call the function for this path */
    ierr = func(srcname, tgtptr);
    if (ierr != UTILS_SUCCESS) {
      if (rerr == UTILS_SUCCESS) rerr = ierr;
    }
  }
 
  ierr = ut_listfree(&slist);
  if (ierr != 0) {
    (void) UTILS_error(caller, "failed on ut_listfree() [802]");
    return (UTILS_FAIL);
  }
 
  return (rerr);
}

/*
 * Dummy error routine - must return zero (see glob() man pages)
 */
static int ut_globerr(const char *epath, int eerrno)
{
  return (0);
}

/**************************************************************************************
 *
 * ut_listfree()
 *
 * - Frees resources used for a list of filenames and the cached glob
 *
 * arguments:
 *
 *   list (inout) number of items in returned list
 *
 */
static int ut_listfree(struct list_tp **list)
{
  if (list != NULL) {
    if (NULL ==  *list) {   /* Possible for *list to be NULL if listdir() failed */
      return (0);
    }
    (void) free(*list);
    *list = NULL;
  }
 
  if (glob_status > 0) {
    errno = 0;
    (void) globfree(&glob_res);
    --glob_status;
  }
 
  return (0);
}

/**************************************************************************************
 *
 * ut_listget()
 *
 * - blah
 *
 * arguments:
 *
 *   pattern  (in)  name of environment variable to read
 *   caller   (in)  calling routine name, used if error found
 *   listlen  (out) number of items in returned list
 *
 */
static int ut_listget(const char     *pattern,
          const char     *caller,
          size_t         *listlen,
          struct list_tp **list)
{
  int    ierr;     /* Error returned from called functions */
  size_t ii;       /* Loop */

  struct list_tp *list_ptr;
 
 
  if (NULL == pattern) {
    *listlen = 0;
    if (list != NULL) {
      *list    = NULL;
    }
    return (0);
  }
 
  errno = 0;
  ierr  = glob(pattern, 0, &ut_globerr, &glob_res);
  ++glob_status;
 
  switch (ierr) {
   
  case 0:
    /* Matches found - allocate and fill a list of matching filenames */
   
    *listlen = glob_res.gl_pathc;
   
    if (list != NULL) {
      *list = NULL;
      *list = (struct list_tp *)malloc(sizeof(struct list_tp) * glob_res.gl_pathc);
     
      if (NULL == *list) {
  (void) UTILS_error(caller, "failed on malloc() for filename list");
  (void) globfree(&glob_res);
  --glob_status;
  return (1);
      }
     
      list_ptr = *list;
     
      for (ii=0; ii<glob_res.gl_pathc; ++ii) {
  list_ptr->name = glob_res.gl_pathv[ii];  /* copying pointer - not string */
  ++list_ptr;
      }
    }
   
    break;
   
  case GLOB_NOMATCH:
    /* No glob matches found - this is fine */
    *listlen = 0;
    if (list != NULL) {
      *list = NULL;
    }
    break;
   
  case GLOB_NOSPACE:
    /* Memory allocation failure */
   
  case GLOB_ABORTED:
    /* if lgloberr return != 0 */
    break;
   
  default:
    /* Other unknown error */
    break;
  }
 
  /* Don't free up the glob - it will corrupt the filenames being pointed to in the
     list */
 
  return (0);
}
