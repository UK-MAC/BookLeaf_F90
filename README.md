![BookLeaf logo](doc/img/logo.png "BookLeaf logo")

# BookLeaf

BookLeaf is a 2D unstructured hydrodynamics mini-app.

## Installation

Bookleaf can either be built in the src directory or in a user specified
directory.  If a user specified directory is used then a copy of the
`src/Makefile` must be placed in there. Additionally the make command line must
include:

```
SRCDIR=path/to/src
```

Bookleaf has a number of example makefiles for different compilers and
architectures in src/makefiles. By default it will use the makefile.GENERIC and
makefile.intel files. This behaviour can be changed by setting new values on the
command line:

```
MKFILEM=<new makefile> - replaces makefile.GENERIC
MKFILEC=<new makefile> - replaces makefile.intel
```

Bookleaf will automatically partition the mesh according to the number of cores
that the problem is run on. The default paritioner is rcb, however Metis 5.1.0
can be used if bookleaf has it linked in. To buld a Metis version set METIS=1.

By default Bookleaf builds with MPI, however a truly serial version can be built
by adding:

`NO_MPI=1`

## Running the code

BookLeaf can run with no command line arguments. By default it expects to find a
file called "control" in the directory it is running in. This can be changed 
by running:

`bookleaf FILE=<[path_to_file/]newfile>`

This file is a copy of the files found in the inputs directory, depending on 
which problem you wish to run.

## Release history

* 02/05/2018, version 2.0
