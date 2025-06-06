# make all: compile to bytecode
# make opt: compile to native code
# make install: install bytecode and/or native code
#
# See Makefile.config for configurable variables.
# Runtime configurations might also be necessary in the site-lib/*/META
# files.
#----------------------------------------------------------------------

TOP=../..
include $(TOP)/Makefile.config

NAME = findlib

# Need compiler-libs since ocaml-4.00
OCAMLC = ocamlc -I +compiler-libs
OCAMLOPT = ocamlopt -I +compiler-libs $(OCAMLOPT_G)
OCAMLOPT_SHARED = $(OCAMLOPT)
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
#CAMLP4O =  camlp4 pa_o.cmo pa_op.cmo pr_o.cmo --
#CAMLP4O =  camlp4 pa_o.cmo pa_op.cmo pr_dump.cmo --


OBJECTS        = fl_compat.cmo fl_split.cmo findlib_config.cmo \
		 fl_metatoken.cmo fl_meta.cmo fl_metascanner.cmo fl_topo.cmo \
		 fl_package_base.cmo findlib.cmo fl_args.cmo fl_lint.cmo
TOBJECTS       = topfind.cmo

XOBJECTS       = $(OBJECTS:.cmo=.cmx)
TXOBJECTS      = $(TOBJECTS:.cmo=.cmx)

OCAMLFIND_OBJECTS = ocaml_args.cmo frontend.cmo
OCAMLFIND_XOBJECTS = $(OCAMLFIND_OBJECTS:.cmo=.cmx)

# OCAMLFIND_ARCHIVES: set in Makefile.config
OCAMLFIND_XARCHIVES = $(OCAMLFIND_ARCHIVES:.cma=.cmxa)

NUMTOP_OBJECTS = num_top_printers.cmo num_top.cmo

DYNLOAD_OBJECTS  = fl_dynload.cmo
DYNLOAD_XOBJECTS = $(DYNLOAD_OBJECTS:.cmo=.cmx)


all: ocamlfind$(EXEC_SUFFIX) findlib.cma findlib_top.cma topfind $(NUMTOP) \
     findlib_dynload.cma

opt: ocamlfind_opt$(EXEC_SUFFIX) findlib.cmxa findlib_top.cmxa topfind \
     findlib_dynload.cmxa

num-top: num_top.cma

ocamlfind$(EXEC_SUFFIX): findlib.cma $(OCAMLFIND_OBJECTS)
	$(OCAMLC) $(CUSTOM) -o ocamlfind$(EXEC_SUFFIX) -g $(OCAMLFIND_ARCHIVES) \
	          $(OCAMLC_FLAGS) $(OCAMLFIND_OCAMLFLAGS) $(OCAMLFIND_OBJECTS)

ocamlfind_opt$(EXEC_SUFFIX): findlib.cmxa $(OCAMLFIND_XOBJECTS)
	$(OCAMLOPT) -o ocamlfind_opt$(EXEC_SUFFIX) $(OCAMLFIND_XARCHIVES) \
		  $(OCAMLOPT_FLAGS) $(OCAMLFIND_OCAMLFLAGS) $(OCAMLFIND_XOBJECTS)

test_parser$(EXEC_SUFFIX): fl_metascanner.cmx test_parser.cmx fl_metatoken.cmx fl_meta.cmx
	$(OCAMLOPT) -o test_parser$(EXEC_SUFFIX) fl_meta.cmx fl_metatoken.cmx fl_metascanner.cmx test_parser.cmx

findlib.cma: $(OBJECTS)
	$(OCAMLC) -a -o findlib.cma $(OBJECTS)

findlib_top.cma: $(TOBJECTS)
	$(OCAMLC) -a -o findlib_top.cma $(TOBJECTS)

findlib.cmxa: $(XOBJECTS)
	$(OCAMLOPT) -a -o findlib.cmxa $(XOBJECTS)
	if [ $(HAVE_NATDYNLINK) -gt 0 ]; then \
	    $(OCAMLOPT_SHARED) -shared -o findlib.cmxs $(XOBJECTS); \
	fi

findlib_top.cmxa: $(TXOBJECTS)
	$(OCAMLOPT) -a -o findlib_top.cmxa $(TXOBJECTS)
	if [ $(HAVE_NATDYNLINK) -gt 0 ]; then \
	    $(OCAMLOPT_SHARED) -shared -o findlib_top.cmxs $(TXOBJECTS); \
	fi

findlib_dynload.cma: $(DYNLOAD_OBJECTS)
	$(OCAMLC) -a -o findlib_dynload.cma $(DYNLOAD_OBJECTS)

findlib_dynload.cmxa: $(DYNLOAD_XOBJECTS)
	$(OCAMLOPT) -a -o findlib_dynload.cmxa $(DYNLOAD_XOBJECTS)
	if [ $(HAVE_NATDYNLINK) -gt 0 ]; then \
	    $(OCAMLOPT_SHARED) -shared -o findlib_dynload.cmxs $(DYNLOAD_XOBJECTS); \
	fi

findlib_config.ml: findlib_config.mlp $(TOP)/Makefile.config
	USE_CYGPATH="$(USE_CYGPATH)"; \
	export USE_CYGPATH; \
	cat findlib_config.mlp | \
	        $(SH) $(TOP)/tools/patch '@CONFIGFILE@' '$(OCAMLFIND_CONF)' | \
	        $(SH) $(TOP)/tools/patch '@STDLIB@' '$(OCAML_CORE_STDLIB)' | \
	        $(SH) $(TOP)/tools/patch '@RELATIVE_PATHS@' '$(RELATIVE_PATHS)' | \
		sed -e 's;@AUTOLINK@;$(OCAML_AUTOLINK);g' \
		    -e 's;@SYSTEM@;$(SYSTEM);g' \
		     >findlib_config.ml

topfind.ml: topfind.ml.in
	if [ "$(ENABLE_TOPFIND_PPXOPT)" = "true" ]; then                 \
		cp topfind.ml.in topfind.ml;                             \
	else                                                             \
		sed -e '/PPXOPT_BEGIN/,/PPXOPT_END/ d' topfind.ml.in     \
			> topfind.ml ;                                   \
	fi

topfind.compat.in: topfind.in
	if [ "$(OCAML_REMOVE_DIRECTORY)" = "1" ]; then                        \
		cp $< $@;                                                     \
	else                                                                  \
		sed -e '/REMOVE_DIRECTORY_BEGIN/,/REMOVE_DIRECTORY_END/ d' $< \
			> $@ ;                                                \
	fi

topfind: topfind.compat.in
	USE_CYGPATH="$(USE_CYGPATH)"; \
	export USE_CYGPATH; \
	cat topfind.compat.in | \
	        $(SH) $(TOP)/tools/patch '@SITELIB@' '$(OCAML_SITELIB)' | \
	        $(SH) $(TOP)/tools/patch '@RELATIVE_PATHS@' '$(RELATIVE_PATHS)' \
	    	    >topfind

num_top.cma: $(NUMTOP_OBJECTS)
	$(OCAMLC) -a -o num_top.cma $(NUMTOP_OBJECTS)

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.a *.lib *.o *.obj *.cmxa \
	  fl_meta.ml findlib_config.ml topfind.ml topfind.compat.in topfind \
	  ocamlfind$(EXEC_SUFFIX) ocamlfind_opt$(EXEC_SUFFIX)

install: all
	$(INSTALLDIR) "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/$(NAME)"
	$(INSTALLDIR) "$(DESTDIR)$(prefix)$(OCAMLFIND_BIN)"
	test $(INSTALL_TOPFIND) -eq 0 || $(INSTALLDIR) "$(DESTDIR)$(prefix)$(OCAML_CORE_STDLIB)"
	test $(INSTALL_TOPFIND) -eq 0 || $(CP) topfind "$(DESTDIR)$(prefix)$(OCAML_CORE_STDLIB)/"
	files=`$(SH) $(TOP)/tools/collect_files $(TOP)/Makefile.config \
	findlib.cmi findlib.mli findlib.cma findlib.cmxa findlib$(LIB_SUFFIX) findlib.cmxs \
	findlib_config.cmi findlib_config.ml topfind.cmi topfind.mli \
	fl_args.cmi fl_lint.cmi fl_meta.cmi fl_split.cmi fl_topo.cmi ocaml_args.cmi \
	fl_package_base.mli fl_package_base.cmi fl_metascanner.mli fl_metascanner.cmi \
	fl_metatoken.cmi findlib_top.cma findlib_top.cmxa findlib_top$(LIB_SUFFIX) findlib_top.cmxs \
	findlib_dynload.cma findlib_dynload.cmxa findlib_dynload$(LIB_SUFFIX) findlib_dynload.cmxs fl_dynload.mli fl_dynload.cmi \
	META` && \
	$(CP) $$files "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/$(NAME)/"
	f="ocamlfind$(EXEC_SUFFIX)"; { test -f ocamlfind_opt$(EXEC_SUFFIX) && f="ocamlfind_opt$(EXEC_SUFFIX)"; }; \
	$(INSTALLFILE) $$f "$(DESTDIR)$(prefix)$(OCAMLFIND_BIN)/ocamlfind$(EXEC_SUFFIX)"
	# the following "if" block is only needed for 4.00beta2
	if [ $(OCAML_REMOVE_DIRECTORY) -eq 0 -a -f "$(OCAML_CORE_STDLIB)/compiler-libs/topdirs.cmi" ]; then \
	    cd "$(OCAML_CORE_STDLIB)/compiler-libs/"; \
	    $(CP) topdirs.cmi toploop.cmi "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/$(NAME)/"; \
	fi

install-num-top:
	$(INSTALLDIR) "$(DESTDIR)$(prefix)$(OCAML_SITELIB)/num-top"
	$(CP) num_top.cma num_top.cmi num_top_printers.cmi \
		"$(DESTDIR)$(prefix)$(OCAML_SITELIB)/num-top/"

# Note: uninstall-num-top is part of the removal of the META files.

uninstall:
	rm -f $(DESTDIR)$(prefix)$(OCAML_CORE_STDLIB)/findlib
	rm -rf $(DESTDIR)$(prefix)$(OCAML_SITELIB)/$(NAME)
	rm -f $(DESTDIR)$(prefix)$(OCAMLFIND_BIN)/ocamlfind$(EXEC_SUFFIX)


depend: *.ml *.mli fl_meta.ml fl_metascanner.ml findlib_config.ml topfind.ml
	$(OCAMLDEP) *.ml *.mli >depend

# Some 'make' implementations require that .SUFFIXES must occur before
# the first suffix rule. (E.g. AIX)
.SUFFIXES: .mll .cmo .cmi .cmx .ml .mli
# .src

.ml.cmx:
	$(OCAMLOPT) $(OPAQUE) $(OCAMLOPT_FLAGS) $(OCAMLFIND_OCAMLFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(OPAQUE)  $(OCAMLC_FLAGS) $(OCAMLFIND_OCAMLFLAGS) -g -c $<

.mli.cmi:
	$(OCAMLC) $(OPAQUE)  $(OCAMLC_FLAGS) $(OCAMLFIND_OCAMLFLAGS) -c $<

#.src.ml:
#	$(CAMLP4O) -impl $< -o $@

# Solaris make does not like the suffix rule .mll.ml,
# so I replaced it by its single application:
fl_meta.ml: fl_meta.mll
	$(OCAMLLEX) fl_meta.mll

# Don't remove fl_metascanner.ml:
.PRECIOUS: fl_metascanner.ml

include depend

