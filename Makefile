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

CFLAGS = -fPIC -O2 -DBLAKE3_NO_SSE2 -DBLAKE3_NO_SSE41 -DBLAKE3_NO_AVX2 -DBLAKE3_NO_AVX512
BLAKE3_OBJ = c/blake3.o c/blake3_dispatch.o c/blake3_portable.o

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

check : all
	@rm -f test.log
	$(GOSH) -I. -I$(srcdir) $(srcdir)/test.scm > test.log

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
	rm -rf core $(TARGET) $(GENERATED) *~ test*.log so_locations

distclean : clean
	rm -rf $(CONFIG_GENERATED)

maintainer-clean : clean
	rm -rf $(CONFIG_GENERATED) VERSION
