# Assessing sub-cellular resolution in  spatial proteomics experiments

## Abstract

The sub-cellular localisation of a protein is paramount in defining
its function, and a protein's mis-localisation is known to lead to
adverse effect. As a result, numerous experimental techniques and
datasets have been published, with the aim to decipher localisation of
proteins at various scales and resolutions, including high profile
mass spectrometry-based efforts. Here, we present a tool, termed
[QSep](https://lgatto.github.io/pRoloc/reference/QSep-class.html), and
a meta-analysis assessing and comparing the sub-cellular resolution of
28 such mass spectrometry-based spatial proteomics experiments.

## Reproducible document

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

If you have `make`, then typing will re-generate the pdf document

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
