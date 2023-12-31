# SPDX-FileCopyrightText: 2023 Ricardo Wurmus <rekado@elephly.net>
#
# SPDX-License-Identifier: CC0-1.0

include guile.am

BUILT_SOURCES = $(top_srcdir)/.version
$(top_srcdir)/.version:
	echo $(VERSION) > $@-t && mv $@-t $@
dist-hook:
	echo $(VERSION) > $(distdir)/.tarball-version

moddir = $(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =							\
  swineherd/config.scm				\
  swineherd/utils.scm				\
  swineherd/api.scm					\
  swineherd/netlink.scm				\
  swineherd.scm

dist_pkgdata_DATA = \
  etc/shepherd-config.scm

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED) \
  -e 's,[@]VERSION[@],$(VERSION),g' \
  -e 's,[@]HERD[@],$(HERD),g' \
  -e 's,[@]BTRFS[@],$(BTRFS),g' \
  -e 's,[@]GUILE[@],$(GUILE),g' \
  -e 's,[@]prefix[@],$(prefix),g' \
  -e 's,[@]localstatedir[@],$(localstatedir),g'

swineherd/config.scm: swineherd/config.scm.in Makefile
	$(AM_V_at)rm -f $@ $@-t
	$(AM_V_at)$(MKDIR_P) "$(@D)"
	$(AM_V_GEN)$(do_subst) < "$(srcdir)/$@.in" > "$@-t"
	mv -f "$@-t" "$@"

EXTRA_DIST += \
  swineherd/config.scm.in \
  LICENSES/GFDL-1.3-no-invariants-or-later.texi doc/version.texi \
  $(wildcard LICENSES/*.txt)

info_TEXINFOS = doc/swineherd.texi

TEST_EXTENSIONS = .scm

SCM_TESTS =

TESTS = $(SCM_TESTS)

EXTRA_DIST += $(TESTS)

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)" GUILE_AUTO_COMPILE=0 ENVIRONMENT=test

SCM_LOG_DRIVER =                                          \
  $(top_builddir)/pre-inst-env                            \
  $(GUILE) --no-auto-compile -e main                      \
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

EXTRA_DIST +=					\
  pre-inst-env.in				\
  build-aux/test-driver.scm
