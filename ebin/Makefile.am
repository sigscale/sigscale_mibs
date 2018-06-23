## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = sigscale_mibs.app
EXTRA_DIST = sigscale_mibs.app

beam_DATA = sigscale_mib.beam \
		sigscale_snmp_lib.beam

%.beam:$(top_srcdir)/src/%.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' < $< > $@

CLEANFILES = $(beam_DATA) $(app_DATA)

