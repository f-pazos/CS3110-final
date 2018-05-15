test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte
ui:
	ocamlbuild -use-ocamlfind guimain.byte && ./guimain.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean

zip:
	zip finalsrc.zip *.ml*

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte
