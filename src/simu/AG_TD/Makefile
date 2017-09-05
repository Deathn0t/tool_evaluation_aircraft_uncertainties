CSLC = ocamlc
CSLOPT = ocamlopt
CSLDEP = ocamldep
INCLUDES = -I ../
CSLFLAGS = $(INCLUDES) -g
CSLOPTFLAGS = $(INCLUDES)

AGNORM_OBJS = ../xy.cmo ../funcnico.cmo ../acft.cmo misc.cmo gvars.cmo local.cmo simplex.cmo types.cmo scale.cmo share.cmo initpop.cmo crossmut.cmo reproduce.cmo main.cmo
AGOPT_OBJS = $(AGNORM_OBJS:.cmo=.cmx)

all : .depend agopt

agopt : $(AGOPT_OBJS) 
	$(CSLOPT) $(CSLOPTFLAGS) -o agopt unix.cmxa str.cmxa bigarray.cmxa $(AGOPT_OBJS)

agnorm :  $(AGNORM_OBJS) 
	$(CSLC) -custom $(CSLFLAGS) -o agnorm str.cma $(AGNORM_OBJS) 

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo :
	$(CSLC) $(CSLFLAGS) -c $<
.mli.cmi :
	$(CSLC) $(CSLFLAGS) -c $<
.ml.cmx :
	$(CSLOPT) $(CSLOPTFLAGS) -c $<

clean:
	\rm -f *.cmo *.cmi *.cmx *.o agnorm agopt *~

.depend: $(AGNORM_OBJS:.cmo=.ml)
	$(CSLDEP) $(INCLUDES) *.mli *.ml >.depend

include .depend
