dnl Configure input for geofon SEED programs - G. Helffrich/U. Bristol
dnl   Oct. 7 2010
AC_INIT(mkconf/Make.ARCH)
AC_CONFIG_AUX_DIR(mkconf)
AC_PROG_CC(cc gcc)
AC_PROG_F77(gfortran g77 f77 ifort)
AC_PROG_RANLIB
AC_PROG_INSTALL
AC_F77_LIBRARY_LDFLAGS
AC_CANONICAL_HOST
dnl Have to undo damage done by F77_LIBRARY_LDFLAGS on MacOS systems
KLUDGE= KEEP=
for L in $ac_cv_flibs $ac_cv_f77_libs ; do
   case $L in
   -l*) ;;
   -L*) KEEP=1 ;;
   *) ;;
   esac
   test -z "${KEEP}" || KLUDGE="${KLUDGE} $L"
done
sym=neither
test -z "$ac_cv_flibs" || sym=ac_cv_flibs
test -z "$ac_cv_f77_libs"  || sym=ac_cv_f77_libs
: ${sym}="${KLUDGE}" ; FLIBS="${KLUDGE}"
dnl Set default CFLAGS, FFLAGS to what we want
dnl KEEP=
dnl for L in $CFLAGS ; do
dnl    case $L in
dnl    -g* | -O*) ;;
dnl    *) KEEP="${KEEP} $L"
dnl    ;;
dnl    esac
dnl done
dnl CFLAGS=`echo "${KEEP}"`
KEEP=
for L in $FFLAGS ; do
   case $L in
   -g* | -O*) ;;
   *) KEEP="${KEEP} $L"
   ;;
   esac
done
FFLAGS=`echo "${KEEP}"`

AC_ARG_VAR(BINDIR,[destination directory])
AC_SUBST(DEFAULT)
AC_SUBST(BINDIR)
AC_SUBST(CFLAGS)
AC_SUBST(ADD_FFLAGS)
AC_SUBST(ADD_CFLAGS)
AC_SUBST(OS)
dnl AC_SUBST(HAVE_X)
dnl AC_SUBST(HAVE_COCOA)
dnl AC_SUBST(SACDIR)
dnl AC_SUBST(GL4)
dnl AC_SUBST(LIBG2C)
dnl AC_SUBST(LIBFRT)
dnl AC_SUBST(LIBCRT)
dnl AC_ARG_VAR(LIBDIR,[tpttsub.o library routine location])
dnl AC_ARG_VAR(SACAUX,[SAC auxiliary files (defaults to LIBDIR/aux)])
dnl SACDIR=$PWD
DEFAULT=$ac_default_prefix
BINDIR=${BINDIR:-${prefix}}
dnl LIBDIR=${LIBDIR:-${libdir}}
dnl SACAUX=${SACAUX:-${LIBDIR}/aux}

dnl -DLINUX -- unpick.  Seems to do cond. compilation on lseek presence
case $host in
   *-apple-darwin*) # Darwin system
     ADD_CFLAGS="${ADD_CFLAGS} -DREFTEK -DLINUX" OS=Darwin
      ;;
   *-*-hpux*) # HP/UX system
     ADD_CFLAGS="${ADD_CFLAGS}" OS=HP-UX
     ;;
   *-*-freebsd*) # FreeBSD system
     ADD_CFLAGS="${ADD_CFLAGS} -DLINUX -DREFTEK" OS=FreeBSD
     ;;
   *-*-linux*) # Linux generic system
     ADD_CFLAGS="${ADD_CFLAGS} -DLINUX -DREFTEK" OS=Linux
     ;;
   *-*-solaris*) # Solaris generic system
     ADD_CFLAGS="${ADD_CFLAGS}" OS=Solaris
     ;;
   *-*-sunos*) # Sunos generic system
     ADD_CFLAGS="${ADD_CFLAGS}" OS=SunOS
     ;;
esac

dnl C header checks
AC_CHECK_HEADER(floatingpoint.h,
   [ADD_CFLAGS="${ADD_CFLAGS} -DHAVE_FLOATINGPOINT"])
AC_CHECK_HEADER(sys/queue.h,
   [ADD_CFLAGS="${ADD_CFLAGS} -DHAVE_QUEUE"])
AC_HEADER_STDC

dnl C library routine checks
dnl AC_CHECK_LIB(SystemStubs,err$LDBLStub)
dnl AC_CHECK_FUNC(snprintf,,[ADD_CFLAGS="${ADD_CFLAGS} -DDONTHAVE_SNPRINTF"])
dnl AC_CHECK_FUNC(reallocf,,[ADD_CFLAGS="${ADD_CFLAGS} -DDONTHAVE_REALLOCF"])

dnl for L in ${LIBS}; do
dnl    case $L in
dnl    -lSystemStubs) # Needed to get restFP/saveFP on gcc-4.x systems
dnl       V=`$CC -v 2>&1| awk '/gcc version/{print $3}'`
dnl       P=`uname -p | awk '{print substr($0,1,1)}'`
dnl       FN=`echo /usr/lib/gcc/${P}*-apple-darwin*/$V/libgcc.a`
dnl       test -f ${FN} && LIBS="${LIBS} ${FN}"
dnl       ;;
dnl    *) ;;
dnl    esac
dnl done

dnl Semi-exotic types
dnl AC_CHECK_TYPES(long long,,[ADD_CFLAGS="${ADD_CFLAGS} -DDONTHAVE_LONG_LONG"])

dnl Additional flags based on system type
dnl if (uname -s > /dev/null) ; then
dnl    case `uname -s` in
dnl    FreeBSD) ADD_CFLAGS="${ADD_CFLAGS} -DBSD4d2" ;;
dnl    Darwin) ADD_CFLAGS="${ADD_CFLAGS} -DBSD4d2" HAVE_COCOA=mac
dnl    #  test `uname -r` = 6.8 && LDFLAGS="${LDFLAGS} -L/sw/lib"
dnl    #  test -d /sw/lib && LDFLAGS="${LDFLAGS} -L/sw/lib"
dnl       case `uname -r` in
dnl       7*) LIBS="${LIBS} -lcc_dynamic"
dnl         ;;
dnl #     8*) LIBCRT="-lcrt1.o"
dnl #       ;;
dnl       esac
dnl       ;;
dnl    *) ;;
dnl    esac
dnl    case `uname -s` in
dnl    FreeBSD|Linux) ADD_CFLAGS="${ADD_CFLAGS} -DTRY_PROC" ;;
dnl    *) ;;
dnl    esac
dnl    case `uname -r -s` in
dnl    "SunOS 4.1."*) FOPT=-O ADD_CFLAGS="${ADD_CFLAGS} -DDUMMY_XRTNS" ;;
dnl    *) FOPT=-O2 ;;
dnl    esac
dnl fi

dnl Library routines
dnl   Start by add local libraries to library search path
dnl LDFLAGS="${LDFLAGS} -L/usr/local/lib"
dnl AC_PATH_XTRA
dnl test "${have_x}" = yes -a "${with_x}" != no && HAVE_X=x11 X_PRE_LIBS="${X_PRE_LIBS} -lXaw -lXmu -lXext -lXt -lX11" || HAVE_X=
dnl if test "${enable_readline}" != no ; then
dnl AC_MSG_CHECKING(whether editline or readline library present)

dnl AC_SEARCH_LIBS(el_init,edit,
dnl       [ADD_CFLAGS="${ADD_CFLAGS} -DHAVE_EDITLINE" LIBS="-ltermcap ${LIBS}" HAVE_READLINE=" "])
dnl    if test "${HAVE_READLINE-unset}" = unset ; then
dnl AC_SEARCH_LIBS(readline,
dnl 	 readline readline.1 readline.2 readline.3 readline.4,
dnl          [ADD_CFLAGS="${ADD_CFLAGS} -DHAVE_READLINE" HAVE_READLINE=" "])
dnl       if test "${HAVE_READLINE-unset}" = unset ; then
dnl          $as_unset ac_cv_search_readline || ac_cv_search_readline=:
dnl AC_SEARCH_LIBS(readline,
dnl 	    readline readline.1 readline.2 readline.3 readline.4,
dnl             [ADD_CFLAGS="${ADD_CFLAGS} -DHAVE_READLINE" LIBS="-ltermcap ${LIBS}" HAVE_READLINE=" "],[],
dnl          -ltermcap)
dnl       fi
dnl    fi
dnl fi
dnl AC_MSG_CHECKING(whether pixrect library present)
dnl AC_SEARCH_LIBS(mem_create,pixrect,
dnl    [ADD_CFLAGS="${ADD_CFLAGS} -DPREVIEW" LIBS="${LIBS} -lpixrect"])
dnl AC_MSG_CHECKING(whether objective C libraries needed)
dnl if test ${HAVE_COCOA} = mac ; then
dnl    case `uname -r` in
dnl    6.*) GL4="/usr/lib/libcc_dynamic.a -Wl,-framework,AppKit" ;;
dnl    8.*) GL4="-Wl,-framework,AppKit" ;;
dnl    *) ;;
dnl    esac
dnl else
dnl    GL4=
dnl fi

LIBS="${LDFLAGS} ${LIBS}"

dnl Check -m32 option for compiler
AC_LANG_PUSH([Fortran 77])
TMP_SV_FFLAGS="${FFLAGS}" FFLAGS="${FFLAGS} -m32"
AC_TRY_COMPILE([],[
      integer array(2)
      array(1)=1
      array(2)=2
],[FM32="-m32"],[FM32=])
FFLAGS="${TMP_SV_FFLAGS}"
AC_LANG_POP([Fortran 77])

AC_LANG_PUSH([C])
TMP_SV_CFLAGS="${CFLAGS}" CFLAGS="${CFLAGS} -m32"
AC_TRY_COMPILE([],[
      int i=1;
],[CM32="-m32"],[CM32=])
CFLAGS="${TMP_SV_CFLAGS}"
AC_LANG_POP([C])

dnl Fortran/C interoperability
AC_MSG_CHECKING(Fortran symbol naming)
TMP_SV_CFLAGS="${CFLAGS}" CFLAGS="${CFLAGS} ${CM32}"
TMP_SV_FFLAGS="${FFLAGS}" FFLAGS="${FFLAGS} ${FM32}"
AC_F77_FUNC(initsac,SYMBOL)
CFLAGS="${TMP_SV_CFLAGS}" FFLAGS="${TMP_SV_FFLAGS}"
case $ac_cv_f77_mangling in
   *"no extra underscore") 
      AC_MSG_RESULT(they end with _ - fine)
      ;;
   *"extra underscore") ADD_CFLAGS="${ADD_CFLAGS} -DFSYMSFX=2"
      AC_MSG_RESULT(they end with __ - fine)
      ;;
   *"no underscore") ADD_CFLAGS="${ADD_CFLAGS} -DFSYMSFX=0"
      AC_MSG_RESULT(they are unchanged - fine)
      ;;
   *) AC_MSG_RESULT(no known method - good luck!)
      ;;
esac
dnl Locate -lg2c and -lfrtbegin on g77/gfortran systems
dnl for L in ${FLIBS} ; do
dnl    case $L in
dnl    -L*) LI=`echo $L | awk '{print substr($0,3)}'`
dnl       test -f $LI/libg2c.a && LIBG2C="${LIBG2C} $LI/libg2c.a"
dnl       test -f $LI/libfrtbegin.a && LIBFRT=$LI/libfrtbegin.a
dnl       test -f $LI/libgfortranbegin.a && \
dnl          LIBG2C="${LIBG2C} $LI/libgfortranbegin.a"
dnl       test -f $LI/libgfortran.a && LIBFRT=$LI/libgfortran.a
dnl       ;;
dnl    *) ;;
dnl    esac
dnl done
dnl test "${LIBG2C-unset}" = unset && LIBG2C=
dnl test "${LIBFRT-unset}" = unset && LIBFRT=
dnl test "${LIBCRT-unset}" = unset && LIBCRT=
AC_LANG_PUSH(Fortran 77)
SV_FFLAGS="${FFLAGS}" F77BC= F77OPTS=
dnl Check for Fortran bounds-check option
for F in fbounds-check C unavailable-option ; do
   FFLAGS="-c -$F"
AC_TRY_COMPILE([],[
      integer array(2)
      array(1)=1
      array(2)=2
],[break])
done
test "$F" = "unavailable-option" || F77BC="-$F"
dnl Check for Fortran no-globals option
F77OPT=
for F in fno-globals unavailable-option ; do
   FFLAGS="-c -$F"
AC_TRY_COMPILE([],[
      integer array(2)
      array(1)=1
      array(2)=2
],[break])
done
test "$F" = "unavailable-option" || F77OPT="-$F"
F77OPTS="${F77OPTS} ${F77OPT}"
dnl Check Fortran for brief debug info availability
FFLAGS="-g -feliminate-unused-debug-symbols"
AC_TRY_COMPILE([],[
      integer array(2)
      array(1)=1
      array(2)=2
],[FDEBUG=" -feliminate-unused-debug-symbols"],[FDEBUG=])
rm -f conftest.$ac_objext conftest.$ac_ext
dnl Check Fortran for uninitialized symbol check availability
FFLAGS="-O -Wuninitialized"
AC_TRY_COMPILE([],[
      integer array(2)
      array(1)=1
      array(2)=2
],[FDEBUG=" -O -Wuninitialized"],[FDEBUG=])
rm -f conftest.$ac_objext conftest.$ac_ext
dnl Check for ishft Fortran intrinsic
FFLAGS=
AC_LINK_IFELSE([AC_LANG_SOURCE([[
      i=ishft(1,1)
      end
]])], [], [ADD_CFLAGS="${ADD_CFLAGS} -DNEED_ISHFT"])
dnl Check for need for Fortran equivalence/common table size options
dnl cat >conftest.$ac_ext <<_ACEOF
dnl       program main
dnl       end
dnl _ACEOF
dnl F77SIZE="-Nq300 -Nn2000"
dnl KEEP=`eval "$F77 -c ${F77SIZE} conftest.$ac_ext 2>&1  1>/dev/null"`
dnl if test $? -eq 0 -a -s conftest.$ac_ext ; then
dnl    for L in $KEEP ; do
dnl       case $L in
dnl         unrecognized) F77SIZE= ;;
dnl         *) ;;
dnl       esac
dnl    done
dnl else
dnl    F77SIZE=
dnl fi
dnl rm -f conftest.$ac_objext conftest.$ac_ext
AC_LANG_POP(Fortran 77)
FFLAGS="${SV_FFLAGS} ${F77OPTS}"

dnl Check C for brief debug info availability
AC_LANG_PUSH(C)
SV_CFLAGS="${CFLAGS}" SV_LDFLAGS="${LDFLAGS}"
CFLAGS="-g -feliminate-unused-debug-symbols"
AC_TRY_COMPILE([],[
      int main(){return 0;}
],[CDEBUG=" -feliminate-unused-debug-symbols"],[CDEBUG=])
CFLAGS="${SV_CFLAGS}"
dnl Check C loader libraries
dnl CFLAGS="${SV_CFLAGS}" LDFLAGS="-nostdlib -lcrt1.o -lSystem"
dnl AC_LINK_IFELSE([
dnl       int main(){return 0;}
dnl ],[LIBCRT="${LIBCRT} -lcrt1.o"],[])
AC_LANG_POP(C)
CFLAGS="${SV_CFLAGS}" LDFLAGS="${SV_LDFLAGS}"

dnl test ${enable_debug-no} = yes && \
dnl    ADD_FFLAGS="${ADD_FFLAGS} -g${FDEBUG}" ADD_CFLAGS="${ADD_CFLAGS} -g${CDEBUG}"
dnl test ${enable_opt-yes} = yes && \
dnl    ADD_FFLAGS="${ADD_FFLAGS} ${FOPT}" ADD_CFLAGS="${ADD_CFLAGS} -O2"
dnl test ${enable_range_check-no} = yes && ADD_FFLAGS="${ADD_FFLAGS} ${F77BC}"
test ${enable_debug-no} = yes && \
    ADD_FFLAGS="${ADD_FFLAGS} -g${FDEBUG}"
test ${enable_opt-yes} = yes && \
    ADD_FFLAGS="${ADD_FFLAGS} ${FOPT}"
test ${enable_range_check-no} = yes && ADD_FFLAGS="${ADD_FFLAGS} ${F77BC}"
test $ac_cv_header_stdc = yes && \
    ADD_CFLAGS="${ADD_CFLAGS} -DSTDC_HEADERS"

dnl Optional compilation defines
AC_ARG_ENABLE(debug,  
   [
  --enable-debug          compiles programs with debugging enabled]
)
AC_ARG_ENABLE(range_check,  
   [  --enable-range-check    checks that array references are within bounds]
)
AC_ARG_ENABLE(opt,  
   [  --disable-opt           compiles programs with optimizing disabled]
)

test "${BINDIR}" = NONE && BINDIR=$bindir
dnl test -z "${LIBDIR}" && LIBDIR=$libdir

AC_OUTPUT(Makefile)
AC_OUTPUT(mkconf/Make.any.FLAGS)
AC_OUTPUT(contrib/utils/Makefile)
echo "Configured with Fortran compiler" $F77
echo "Configured with Fortran compiler flags" $FFLAGS $ADD_FFLAGS
echo "Configured with C compiler" $CC
echo "Configured with C compiler flags" $CFLAGS $ADD_CFLAGS
