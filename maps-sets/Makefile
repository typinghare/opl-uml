PROGRAM = test.native

clean:
	rm -f $(PROGRAM) && rm -rf _build

$(PROGRAM): clean
	ocamlbuild -use-ocamlfind $@

build: $(PROGRAM)
	./$(PROGRAM)