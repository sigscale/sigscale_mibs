## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

emib_DATA =  SIGSCALE-SMI.bin \
			SIGSCALE-TC.bin \
			SIGSCALE-MODULES-MIB.bin \
			SIGSCALE-PRODUCTS-MIB.bin

%.bin:$(top_srcdir)/mibs/%.mib
	$(ERLC) -I $(top_srcdir)/include $<

CLEANFILES = $(emib_DATA)

