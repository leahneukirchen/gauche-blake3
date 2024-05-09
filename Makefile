# General info
SHELL       = /bin/sh
prefix      = /usr
exec_prefix = ${prefix}
bindir      = ${exec_prefix}/bin
libdir      = ${exec_prefix}/lib
datadir     = ${datarootdir}
datarootdir = ${prefix}/share
srcdir      = .
VPATH       = $(srcdir)

# These may be overridden by make invocators
DESTDIR        =
GOSH           = /usr/bin/gosh
GAUCHE_CONFIG  = /usr/bin/gauche-config
GAUCHE_PACKAGE = /usr/bin/gauche-package
INSTALL        = /usr/bin/gauche-install
PRECOMP        = $(GOSH) tools/precomp

# Other parameters
SOEXT  = so
OBJEXT = o
EXEEXT = 
LOCAL_PATHS = 

# Module-specific stuff
PACKAGE   = blake3

ARCHFILES = blake3.$(SOEXT)
SCMFILES  = $(srcdir)/blake3.scm
HEADERS   =

CFLAGS = -fPIC -O2 -DPACKAGE_URL= -DPACKAGE_BUGREPORT= -DPACKAGE_TARNAME=\"blake3\" -DPACKAGE_STRING= -DPACKAGE_NAME=\"blake3\" -DPACKAGE_VERSION=\"0.1\"
BLAKE3_OBJ = c/blake3.o c/blake3_dispatch.o c/blake3_portable.o
BLAKE3_OBJ += c/blake3_sse2.o c/blake3_sse41.o c/blake3_avx2.o c/blake3_avx512.o

TARGET    = $(ARCHFILES) $(BLAKE3_OBJ)
GENERATED = 
CONFIG_GENERATED = Makefile config.cache config.log config.status \
		   configure.lineno $(PACKAGE).gpd

GAUCHE_PKGINCDIR  = $(DESTDIR)${libdir}/gauche-0.98/site/include
GAUCHE_PKGLIBDIR  = $(DESTDIR)${datadir}/gauche-0.98/site/lib
GAUCHE_PKGARCHDIR = $(DESTDIR)${libdir}/gauche-0.98/site/x86_64-unknown-linux-gnu

all : $(TARGET)

blake3.$(SOEXT): $(srcdir)/blake3.scm $(BLAKE3_OBJ)
	$(PRECOMP) --ext-main $(srcdir)/blake3.scm
	$(GAUCHE_PACKAGE) compile \
	  --local=$(LOCAL_PATHS) --cflags="-Ic" --libs "$(BLAKE3_OBJ)" --verbose blake3 blake3.c

c/blake3_sse2.o : CFLAGS+=-msse2
c/blake3_sse41.o : CFLAGS+=-msse4.1
c/blake3_avx2.o : CFLAGS+=-mavx2
c/blake3_avx512.o : CFLAGS+=-mavx512f -mavx512vl
c/blake3_neon.o : 

check : all
	$(GOSH) -I. $(srcdir)/test.scm > test.log

install : all
	$(INSTALL) -m 444 -T $(GAUCHE_PKGINCDIR) $(HEADERS)
	$(INSTALL) -m 444 -T $(GAUCHE_PKGLIBDIR) $(SCMFILES)
	$(INSTALL) -m 555 -T $(GAUCHE_PKGARCHDIR) $(ARCHFILES)
	$(INSTALL) -m 444 -T $(GAUCHE_PKGLIBDIR)/.packages $(PACKAGE).gpd

uninstall :
	$(INSTALL) -U $(GAUCHE_PKGINCDIR) $(HEADERS)
	$(INSTALL) -U $(GAUCHE_PKGLIBDIR) $(SCMFILES)
	$(INSTALL) -U $(GAUCHE_PKGARCHDIR) $(ARCHFILES)
	$(INSTALL) -U $(GAUCHE_PKGLIBDIR)/.packages $(PACKAGE).gpd

clean :
	$(GAUCHE_PACKAGE) compile --clean blake3 blake3.c
	rm -f core $(TARGET) $(GENERATED) test.log

distclean : clean
	rm -f $(CONFIG_GENERATED)

maintainer-clean : clean
	rm -f $(CONFIG_GENERATED) VERSION
