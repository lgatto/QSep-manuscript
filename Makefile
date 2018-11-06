all:
	make qsep.pdf
	make qsep.R
	make reviewers.pdf

qsep.tex: qsep.Rnw
	R --vanilla -e "knitr::knit('qsep.Rnw')"
	perl -pi -e 's/\\begin{longtable}{lrrrrl}/\\begin{longtable}{lrrrrp{8cm}}/' qsep.tex
	perl -pi -e 's/\$$\\backslash\$$/\\/' qsep.tex
	perl -pi -e 's/\\{/{/' qsep.tex
	perl -pi -e 's/\\}/}/' qsep.tex

qsep.pdf: qsep.tex
	pdflatex qsep.tex
	bibtex qsep
	pdflatex qsep.tex
	pdflatex qsep.tex

qsep.R: qsep.Rnw
	R --vanilla -e "knitr::purl('qsep.Rnw')"

reviewers.pdf: reviewers.Rmd
	R --vanilla -e "rmarkdown::render('reviewers.Rmd', output_format = rmarkdown::pdf_document())"

.PHONY: clean all

qsep.R: qsep.Rnw
	R --vanilla -e "knitr::purl('qsep.Rnw')"

clean:
	rm -f .Rhistory qsep-blx.bib qsep.aux qsep.bbl qsep.blg qsep.log qsep.run.xml qsep.tex qsep.out qsep.tx
	rm -f *~
	rm -rf figure/
	rm -rf cache/
