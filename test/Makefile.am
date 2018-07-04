## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

testmods = sigscale_mibs_oid_SUITE \
			sigscale_mibs_snmp_SUITE

check_DATA = .dialyzer_ok .common_test_ok

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer --src $(top_srcdir)/src

%.beam:$(top_srcdir)/test/%.erl
	$(ERLC) $(ERLCFLAGS) $<

.common_test_ok:	$(addsuffix .beam, $(testmods))
	@if [ ! -d log ]; then mkdir log; fi
	@if [ ! -f test.spec ]; then cp $(top_srcdir)/test/test.spec .; fi
	for i in $(addsuffix _data,$(testmods)); do \
		if [ -d $(top_srcdir)/test/$$i ]; then \
			if [ ! -d $$i ]; then \
				mkdir $$i; \
			fi; \
			for j in `ls $(top_srcdir)/test/$$i`; do \
				if [ $(top_srcdir)/test/$$i/$$j -nt $$i/$$j ]; then \
					cp $(top_srcdir)/test/$$i/$$j $$i/$$j; \
				fi; \
			done; \
		fi; \
	done
	NODENAME=`id -un`
	ct_run -no_auto_compile -pa $(top_builddir)/ebin -logdir log -spec test.spec -dir . -erl_args -noshell -sname $(LOGNAME)-ct

clean-local:
	-rm -r *.beam log/*

