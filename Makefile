
asteroids: asteroids.ml
	ocamlc -thread graphics.cma unix.cma asteroids.ml -o asteroids

report.pdf: report.tex
	pdflatex $<

.PHONY: clean mrproper

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.cmi
	rm -f *.cmo

mrproper: clean
	rm -f asteroids
	rm -f report.pdf
