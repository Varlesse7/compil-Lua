LEX_ML = ocamllex
YACC_ML = /usr/bin/ocamlyacc
OCAMLC = ocamlc

print_ast:
	$(OCAMLC) -c miniml/ast.ml
	$(YACC_ML) -b miniml/parser miniml/parser.mly
	$(OCAMLC) -I miniml -c miniml/parser.mli
	$(LEX_ML) -o miniml/lexer.ml miniml/lexer.mll
	$(OCAMLC) -I miniml -c miniml/parser.ml
	$(OCAMLC) -I miniml -c miniml/lexer.ml
	$(OCAMLC) -I miniml -c miniml/compiler.ml
	$(OCAMLC) -I miniml -c miniml/cam_interpreter.ml
	$(OCAMLC) -I miniml -c miniml/flatten.ml
	$(OCAMLC) -I miniml -c miniml/miniml_test.ml

	$(OCAMLC) -o miniml_test miniml/compiler.cmo miniml/ast.cmo miniml/lexer.cmo miniml/parser.cmo miniml/cam_interpreter.cmo miniml/flatten.cmo miniml/miniml_test.cmo
clean:
	rm -f miniml/*.cmo miniml/*.cmi miniml/miniml_test
	rm -f miniml/lexer.ml miniml/parser.mli miniml/parser.ml miniml/parser.output
