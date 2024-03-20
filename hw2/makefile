FILE = "hw2.ml"

PACKAGES = camlp-streams

make:
	ocamlfind ocamlc -package $(PACKAGES) -linkpkg $(FILE)

clean:
	rm -f hw2.cmi hw2.cmo *.out


# #require "camlp-streams";; 
# #use "hw2.ml";;

# let p = LambdaParser.parse;;
# let pp = LambdaParser.pp;;
