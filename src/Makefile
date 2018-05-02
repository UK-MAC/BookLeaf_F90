#
# !Crown Copyright 2014 AWE.
# !
# ! This file is part of Bookleaf.
# !
# ! Bookleaf is free software: you can redistribute it and/or modify it under
# ! the terms of the GNU General Public License as published by the
# ! Free Software Foundation, either version 3 of the License, or (at your option)
# ! any later version.
# !
# ! Bookleaf is distributed in the hope that it will be useful, but
# ! WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# ! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# ! details.
# !
# ! You should have received a copy of the GNU General Public License along with
# ! Bookleaf. If not, see http://www.gnu.org/licenses/.
#
# machine input
MKFILEM = makefile.GENERIC
MKFILEC = makefile.gcc
MKFILEV = $(SRCDIR)/makefiles/makefile.VER
# paths
SRCDIR  = .
MODDIR  = $(SRCDIR)/mods
PCKDIR  = $(SRCDIR)/packages
UTLDIR  = $(SRCDIR)/utilities
DATDIR  = $(SRCDIR)/dataAPI
INFDIR  = $(SRCDIR)/infrastructure
TIMDIR  = $(SRCDIR)/timers

# build info
NO_MPI  =
NO_OMP  =
PHI     =
SILO    =
TIO     =
METIS   =
LSETUP  = 1
DEP     =
# Version
VERS = 2.0.0
EXE  = bookleaf
ifeq ($(MOD),)
  EXENAME  = $(EXE)
else
  EXENAME  = $(EXE)_$(MOD)
endif

# Determine level
ifeq ($(LEV),)
  LEV=OPT
endif
FIND_LEV=$(findstring $(LEV),MEMDEBUG DEBUG TV MEMTV OPTDEBUG RELEASE OPT)
ifeq ($(FIND_LEV),)
  $(error Unknown build level)
endif

# Default
help:
	# Usage
	# Pre_pro:
	#  MKFILEM     - machine specific makefile,  DEFAULT makefile.GENERIC
	#  MKFILEC     - compiler specific makefile, DEFAULT makefile.gcc
	#  MKFILEV     - override default list of library versions
	#  SRCDIR      - path to source directory,   DEFAULT .
	#  LEV         - build level, options: MEMDEBUG,DEBUG,TV,MEMTV,OPTDEBUG,RELEASE,OPT
	#                DEFAULT OPT
	#  MOD         - include problem specific initialisation modification, DEFAULT none
	#  NO_MPI      - Do not compile MPI routines
	#  NO_OMP      - Do not use OpenMP threads
	#  PHI         - Build executable for Intel Xeon Phi
	#  SILO        - Compile Silo routines
	#  TIO         - Compile TyphonIO routines
	#  METIS       - Use Metis decomposition with LSETUP (if not NO_MPI)
	#  LSETUP      - Use local mesh generation and decomposition (if not NO_MPI)
	#
	# Commands:
	#  help        - print this message
	#  bookleaf    - build executable
	#  clean       - remove objects, modules and executable
	#  cleandata   - remove IO files

# Include machine specific options
ifneq (,$(findstring $(EXE),$(MAKECMDGOALS)))
  include $(SRCDIR)/makefiles/$(MKFILEM)
  include $(SRCDIR)/makefiles/$(MKFILEC)
  include $(MKFILEV)
endif

# Include object list
ifneq (,$(findstring $(MAKECMDGOALS),$(EXE)))
  include $(INFDIR)/makefile.INFRASTRUCTURE
  include $(DATDIR)/makefile.DATAAPI
  include $(PCKDIR)/makefile.PACKAGES
  include $(UTLDIR)/makefile.UTILITIES
  include $(TIMDIR)/makefile.TIMERS
endif

# set pre-processor flags, adjust object list and dependency list
ifneq ($(SILO),)
  ifeq ($(MKFILEC), makefile.xl)
    PP_SILO=-WF,-DSILO
  else
    PP_SILO=-DSILO
  endif
endif
ifneq ($(TIO),)
  ifeq ($(MKFILEC), makefile.xl)
    PP_TIO=-WF,-DTIO
  else
    PP_TIO=-DTIO
  endif
endif
ifneq ($(MOD),)
  ifeq ($(MKFILEC), makefile.xl)
    PP_MOD=-WF,-DMODY
  else
    PP_MOD=-DMODY
  endif
  OBJ+=$(MOD).o
endif
ifneq ($(NO_MPI),)
  ifeq ($(MKFILEC), makefile.xl)
    PP_MPI=-WF,-DNOMPI
  else
    PP_MPI=-DNOMPI
  endif
endif
ifneq ($(NO_OMP),)
  ifeq ($(MKFILEC), makefile.xl)
    PP_OMP=-WF,-DNOOMP
  else
    PP_OMP=-DNOOMP
  endif
endif
ifeq ($(LSETUP),1)
  ifeq ($(MKFILEC), makefile.xl)
    PP_LSETUP=-WF,-DLSETUP
  else
    PP_LSETUP=-DLSETUP
  endif
endif
ifneq ($(METIS),)
  ifeq ($(NO_MPI),)
    ifeq ($(MKFILEC), makefile.xl)
      PP_METIS=-WF,-DMETIS
    else
      PP_METIS=-DMETIS
    endif
  endif
endif

# Include dependencies
ifneq (,$(findstring $(EXE),$(MAKECMDGOALS)))
  sinclude ./makefile.dep
endif

# Source paths
VPATH=$(MODDIR):$(DATDIR):$(PCKPATH):$(UTLPATH):$(INFPATH):$(TIMDIR)

# Collect all pre-processor options
PREPRO = $(PP_MOD)    \
	 $(PP_MPI)    \
	 $(PP_SILO)   \
	 $(PP_TIO)    \
	 $(PP_METIS)  \
	 $(PP_LSETUP) \
	 $(PP_OMP)

CPREPRO = 

# Includes
INC =	$(SILOINC)   \
	$(TIO_INC)   \
	$(METISINC)  \
	$(HDF5INC) 

# Libraries
LIB = 	$(SILOLIB)   \
	$(TIO_LIB)   \
	$(HDF5LIB)   \
	$(METISLIB)  \
	$(ZLIBLIB)

# Rules
.SUFFIXES:
.SUFFIXES: .o .f90 .F90 .c
.f90.o:
	$(FC) $(FFLAGS) $(INC) $<
.F90.o:
	$(FC) $(FFLAGS) $(PREPRO) $(INC) $<
.c.o:
	$(CC) $(CFLAGS) $(CPREPRO) $<

# Dependencies
$(OBJ): | makefile.dep
DEP+=makefile.dep

# Commands
$(EXE): $(DEP) $(OBJ)
	$(FC) $(OBJ) $(LIB) $(LFLAGS) $(EXENAME)

clean:
	- rm -f *.o
	- rm -f *.mod
	- rm -f $(EXE)*
	- rm -f makefile.dep

cleandata:
	- rm -rf *dump*

makefile.dep:
	$(UTLPUDIR)/depend.py -d $(SRCDIR) $(OBJ)
