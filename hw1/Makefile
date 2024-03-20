MAIN=interp
ZIP=interp.zip

OBJS = ast.cmo lexer.cmo parser.cmo eval.cmo

%.cmo : %.ml
	ocamlc -g -c $<

%.cmi : %.mli
	ocamlc -g -c $<


$(MAIN): clean $(OBJS) main.cmo
	ocamlc -g -o $(MAIN) $(OBJS) main.cmo
	cp $(OBJS) main.cmo interpreter/

lexer.ml : lexer.mll
	ocamllex -q $<

lexer.cmo : parser.cmi lexer.ml
	ocamlc -g -c lexer.ml

parser.ml : parser.mly
	ocamlyacc -q $<

parser.mli : parser.mly
	ocamlyacc -q $<

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(MAIN)
	rm -f interpreter/*.cmo interpreter/$(MAIN)
	rm -f $(ZIP)

release: $(MAIN)
	zip -9r $(ZIP) interpreter

graderhelper: $(OBJS) graderhelper.cmo
	ocamlc -g -o graderhelper $(OBJS) graderhelper.cmo
