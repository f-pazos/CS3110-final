test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte
ui:
	ocamlbuild -use-ocamlfind guimain.byte && ./guimain.byte

clean:
	ocamlbuild -clean

zip:
	zip finalsrc.zip *.ml* tribe_names.txt

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte
