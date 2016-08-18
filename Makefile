qsep.tex: qsep.Rnw
	R-3.3 --vanilla -e "knitr::knit('qsep.Rnw')"
	perl -pi -e 's/\\begin{longtable}{lrrl}/\\begin{longtable}{lrrp{11cm}}/' qsep.tex

qsep.pdf: qsep.tex
	pdflatex qsep.tex
	bibtex qsep
	pdflatex qsep.tex
	pdflatex qsep.tex

qsep.R: qsep.Rnw
	R-3.3 --vanilla -e "knitr::purl('qsep.Rnw')"

clean:
	rm -f .Rhistory qsep-blx.bib qsep.aux qsep.bbl qsep.blg qsep.log qsep.run.xml qsep.tex
	rm -f *~
	rm -rf figure/
