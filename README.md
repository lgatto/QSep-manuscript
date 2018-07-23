# Assessing sub-cellular resolution in  spatial proteomics experiments

To reproduce this document, you'll need `R` version 3.3.1 or
later. Install all packages with

```r
source("http://www.bioconductor.org/biocLite.R")
bioLite(c("pRoloc", "pRolocdata", "knitr"))
```

Clone the git repository

```
git clone git@github.com:lgatto/QSep-manuscript.git
```

If you have `make`, then just type 

```
make qsep.pdf
```

Otherwise, in `R`

```r
bioLite("rmarkdown")
rmarkdown::render("qsep.Rnw", output_format = pdf_document)
```

The latter will produce a document with slighly different formatting,
but the text, figures and references will be identical.
