qsep.tex: qsep.Rnw
	R-3.3 --vanilla -e "knitr::knit('qsep.Rnw')"

qsep.pdf: qsep.tex
	pdflatex qsep.tex
	bibtex qsep
	pdflatex qsep.tex
	pdflatex qsep.tex


clean:
	rm -f .Rhistory qsep-blx.bib qsep.aux qsep.bbl qsep.blg qsep.log qsep.pdf qsep.run.xml qsep.tex
	rm -rf figure/
