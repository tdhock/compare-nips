HOCKING-compare.pdf: HOCKING-compare.tex refs.bib figure-hard-margin.tex figure-simulation.tex
	rm -f *.aux *.bbl
	pdflatex HOCKING-compare
	bibtex HOCKING-compare
	pdflatex HOCKING-compare
	pdflatex HOCKING-compare
figure-norm-data.tex: figure-norm-data.R tikz.R
	R --no-save < $<
figure-hard-margin.tex: figure-hard-margin.R tikz.R
	R --no-save < $<
figure-simulation.tex: figure-simulation.R tikz.R simulation.RData
	R --no-save < $<
simulation.RData: simulation.R svmlight.R
	R --no-save < $<
