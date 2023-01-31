# GEOFON SeedStuff SEED writing tools

These routines heavily modified by G. Helffrich/U. Bristol since obtained
in 1995 from W. Hanka of GEOFON.  They form a useful set of utilities to handle
mseed data and package it into SEED volumes given time windows.  Main
programs are:

* check_seed - Verifies that data is continuous and that data counts in mseed
   blockettes agrees with the number of samples in the blockettes.

* make_qseed - Reblocks mseed data into 4096 byte blocks and names output
   files in a way consistent with copy_seed use.  Will also change
   site information in blockettes (station name, network code, channel code).

* copy_seed - Extracts a seed volume from a pool of mseed blockettes and a
   network description.  This is the main program of the lot.

* make_dlsv - Make a dataless seed volume to describe network parameters.

* cfg - A directory of prototype network configuration files.  These files
   describe a network's characteristics and controls data retrieval from
   the data archive (kept separately).  The directory contains configuration
   files from previous networks that can serve as starting points for new
   network descriptions, and contains useful instrument response information
   (FIR coefficient files) for some typical instruments used in temporary
   deployments.

* contrib/utils - A directory of utility programs for querying raw mseed files
   and creating file names acceptable for use with the GEOFON SEED writing
   programs.

## Documentation

Sketchy.  Run each program with no command line parameters to get a fairly
helpful summary of program options.

## History

The basic differences between the GFZ versions and the present ones are in
handling
1) Y2K problems (yes, there were some);
2) endian problems (yes, there were rather more of those, too);
3) making the programs work with mseed data of any endianness (not part of the
   original design, which assumed all big-endian data);
4) cleaning up strange programming practices (e.g.
   using functions rather than subroutines, even though no useful function
   result was returned ... eh?);
5) the configuration file format for response descriptions was extended to
   allow for a range of FIR coefficients rather than a list (e.g. 1-5 for FIR
   coeffs. 1 to 5 rather than 1/2/3/4/5); and
6) creating an automatic configuration procedure.

Finally, these programs were changed to compile and run properly with `gfortran`.
This is a bigger headache that you think due to the way that `g77` and
`gfortran` handle fixed-format input:  `g77` is lenient, while `gfortran` is strict.
(Long lines that extend beyond column 71 but that are valid Fortran are accepted
by `g77` but truncated by `gfortran`.)  Usually this leads to syntax errors, but
occasionally it leads to a subtly malfunctioning program.  There may
still be (rare) bugs of this sort.

Other `gfortran` compatibility problems that were fixed are:

1) the `g77` runtime library allowed the same file to be attached to multiple
i/o units; `gfortran`'s does not.

2) `g77` runtime returns -1 for EOF on read, used by these programs to tell the
difference between an i/o error and EOF.  The only reliably-defined codes are
0 = OK and nonzero = not ok.  So, interpret all nonzero values as indicating
EOF.

## Compiling
This version of the package has an automatic configuration script.  For
basic configuration, use:

`./configure ; make`

If on a FreeBSD system, use gmake rather than make due to different make file
semantics:

`./configure ; gmake`

You can get a summary of configure options by issuing

`./configure --help`

The options `--prefix=` and `BINDIR=` control where the programs are installed when
'make install' is used.

If you don't have a Fortran compiler on your machine but you do have a C
compiler, you will find the `f2c` package in contrib to convert to C.  There
is no automatic configuration for this procedure; you are on your own to develop
a command that uses f2c to compile Fortran using C to object code files.
`fort77` is such a program that is available from online source repositories.

Some gfortran compiler builds default to 32 bit output; others default to 64.
If the defaults differ between the C and Fortran compilers, explicitly force
the C compiler to the Fortran default.  E.g., if gfortran is 32 bit by
default, add the `-m32` option to your CFLAGS, for example by

`./configure CFLAGS=-m32`

## Compile history
```
SeedStuff - Original version by Winfried Hanka, GFZ Potsdam for GEOFON network.

Past versions of SeedStuff were compiled and tested with the following sytems.

Solaris 5.5    - gcc version 2.7.2
               - f77

SunOS 4.1.3    - gcc version 2.5.8
               - f77

Linux 2.0      - gcc version 2.7.2
               - f2c

HP-UX A.09.05  - cc
               - f77 

The present version of SeedStuff was compiled and tested with the following
systems.

Linux 2.2      - gcc version 2.7.2
               - g77 version 2.95.2

Linux 2.2      - gcc egcs-2.91.66
               - g77 egcs-2.91.66

FreeBSD 6.3    - gcc version 3.4.6
 (sparc)       - f77 version 3.4.6 and gfortran

Darwin 6,7     - gcc
               - g77

Darwin 8       - gcc version 4.0.1 (Apple Inc. build 5367)
               - g77 version 3.4.0 and gfortran

Darwin 10      - gcc version 4.2.1 (Apple Inc. build 5664)
	       - g77 version 3.4.3 and gfortran

Darwin 18      - cc Apple LLVM 10.0.1
               - gfortran 11.2.0
```
