# Assessing sub-cellular resolution in  spatial proteomics experiments

To reproduce this document, you'll need `R` version 3.3.1 or
later. Install all packages with

```r
source("http://www.bioconductor.org/biocLite.R")
bioLite(c("pRoloc", "pRolocdata", "knitr"))
```

Version requirements are `pRoloc` >= 1.13.9 and `pRolodata` >= 1.11.2.

```r
if (packageVersion("pRoloc") < '1.13.9')
    biocLite("lgatto/pRoloc")
if (packageVersion("pRolocdata") < '1.11.2')
    biocLite("lgatto/pRolocdata")
```

Clone the git repository

```
git clone git@github.com:ComputationalProteomicsUnit/QSep-manuscript.git
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
