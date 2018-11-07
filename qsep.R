## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, cache = FALSE)

## ----env, warnings = FALSE-----------------------------------------------
suppressWarnings(suppressPackageStartupMessages(library("MSnbase")))
suppressWarnings(suppressPackageStartupMessages(library("pRoloc")))
suppressPackageStartupMessages(library("pRolocdata"))
suppressPackageStartupMessages(library("xtable"))
suppressPackageStartupMessages(library("hexbin"))
stopifnot(packageVersion("pRolocdata") >= "1.17.2")
stopifnot(packageVersion("pRoloc") >= "1.13.15")
suppressPackageStartupMessages(library("ggrepel"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("lattice"))
suppressPackageStartupMessages(library("patchwork"))


## ----myfuns--------------------------------------------------------------
## a wrapper around sapply to avoid having to 'get' the actual data at
## every iteration
pdapply <- function(X, FUN, ..., use.names = TRUE) {
    ans <- sapply(X, function(x) {
        x <- get(x)
        FUN <- match.fun(FUN)
        args <- list(x)
        if (!is.null(pairlist(...)))
            args <- c(args, pairlist(...))
        do.call(FUN, args)
    })
    if (!use.names)
        names(ans) <- NULL
    ans
}

## ----pdata---------------------------------------------------------------

.experiments <- c("E14TG2aS1", "HEK293T2011", "andreyev2010rest", "dunkley2006",
                  "foster2006", "groen2014cmb", "hall2009", "hyperLOPIT2015",
                  "hyperLOPIT2015ms2", "itzhak2016stcSILAC", "nikolovski2012imp",
                  "nikolovski2014", "rodriguez2012r1", "tan2009r1", "trotter2010",
                  "beltran2016HCMV120", "beltran2016HCMV72", "beltran2016MOCK24",
                  "beltran2016MOCK96", "beltran2016HCMV24", "beltran2016HCMV96",
                  "beltran2016MOCK48", "beltran2016HCMV48", "beltran2016MOCK120",
                  "beltran2016MOCK72", "hirst2018", "itzhak2017",
                  "hyperLOPITU2OS2017", "hyperLOPITU2OS2017b")
pdata <- pRolocdata()$results
pdata <- pdata[pdata[, "Item"] %in% .experiments, ]
pdataitems <- pdata[, "Item"]
data(list = pdataitems)

## Let's shorted some marker labels for better visualisation - see issue
hyperLOPIT2015 <- fDataToUnknown(hyperLOPIT2015, fcol = "markers",
                                 from = "Endoplasmic reticulum/Golgi apparatus",
                                 to = "ER/GA")
hyperLOPIT2015ms2 <- fDataToUnknown(hyperLOPIT2015ms2, fcol = "markers",
                                    from = "Endoplasmic reticulum/Golgi apparatus",
                                    to = "ER/GA")

## Subset hirst2018, to retain CTRL experiment.
## All 3 have very similar QSep values; CTRL is top.
hirst2018 <- split(hirst2018, "sample")[["CTRL"]]


## make table
pdtab <- data.frame(pdata[, 3:4], stringsAsFactors = FALSE)
pdtab$nprot <- pdapply(pdataitems, nrow)
pdtab$frac <- pdapply(pdataitems, ncol)
pdtab$nclust <- pdapply(pdataitems, function(x)
    length(unique(fData(markerMSnSet(x))$markers)))
pdtab$pcvar <- sapply(pdataitems, function(x) {
    pcvar <- plot2D(impute(get(x), "zero"), plot = FALSE,
                    dims = c(1, ifelse(x == "itzhak2016stcSILAC", 3, 2)))
    pcvar <- sub("%\\)", "", sub("^.*\\(", "", colnames(pcvar)))
    sum(as.numeric(pcvar))
})
pdtab <- pdtab[, c(1, 3:6, 2)]
names(pdtab) <- c("Data", "Proteins", "Fractions",
                  "Clusters", "PC var (%)", "Title")

## This is added manually and depends on the order of pdtab$Data
cits <- c("\\citep{Breckels:2016}",
          "\\citep{Breckels:2013}",
          "\\citep{Andreyev:2010}",
          "\\citep{JeanBeltran:2016} HCMV infection, 120 hpi (hours post-infection)",
          "\\citep{JeanBeltran:2016} HCMV infection, 24 hpi",
          "\\citep{JeanBeltran:2016} HCMV infection, 48 hpi",
          "\\citep{JeanBeltran:2016} HCMV infection, 72 hpi",
          "\\citep{JeanBeltran:2016} HCMV infection, 96 hpi",
          "\\citep{JeanBeltran:2016} MOCK, 120 hpi",
          "\\citep{JeanBeltran:2016} MOCK, 24 hpi",
          "\\citep{JeanBeltran:2016} MOCK, 48 hpi",
          "\\citep{JeanBeltran:2016} MOCK, 72 hpi",
          "\\citep{JeanBeltran:2016} MOCK, 96 hpi",
          "\\citep{Dunkley:2006}",
          "\\citep{Foster:2006}",
          "\\citep{Groen:2014}",
          "\\citep{Hall:2009}",
          "\\citep{Hirst:2018}",
          "\\citep{Christoforou:2016}",
          "\\citep{Christoforou:2016}",
          "\\citep{Thul:2017} (all fractions)",
          "\\citep{Thul:2017} (cleaned)",
          "\\citep{Itzhak:2016}",
          "\\citep{Itzhak:2017}",
          "\\citep{Nikolovski:2012}",
          "\\citep{Nikolovski:2014}",
          "\\citep{RodriguezPineiro:2012}",
          "\\citep{Tan:2009}",
          "\\citep{Trotter:2010}")
npubs <- length(unique(sub("}.+$", "}", cits)))
stopifnot(identical(length(cits), nrow(pdtab)))
pdtab$Title <- paste(pdtab$Title, cits)

year <- c(2011, ## E14 from Breckels 2016
          2011, ## HEK293T2011, from Breckels 2013
          2010, ## Andreyev
          2016, ## Beltran 2016
          2016, ## Beltran 2016
          2016, ## Beltran 2016
          2016, ## Beltran 2016
          2016, ## Beltran 2016
          2016, ## Beltran 2016
          2016, ## Beltran 2016
          2016, ## Beltran 2016
          2016, ## Beltran 2016
          2016, ## Beltran 2016
          2006, ## Dunkley, re-analysed in Breckels 2013
          2006, ## Foster
          2014, ## Groen
          2009, ## Hall
          2018, ## Hirst 2018
          2016, ## hyperLOPIT
          2016, ## hyperLOPIT
          2017, ## Thul 2017
          2017, ## Thul 2017
          2016, ## Itzhak 2016
          2017, ## Itzhak 2017
          2012, ## Nikolovski 2012
          2014, ## Nikolovski 2014
          2012, ## Rodriguez Pineiro
          2009, ## Tan, re-analised in Breckels 2013
          2009) ## Trotter (dunkley/sadowski), with Breckels 2013 clusters
stopifnot(identical(length(year), nrow(pdtab)))
pdtab$year <- year

mstech <- c("iTRAQ", ## E14 from Breckels 2016
            "iTRAQ", ## HEK293T2011, from Breckels 2013
            "iTRAQ", ## Andreyev
            "TMT", ## Beltran 2016
            "TMT", ## Beltran 2016
            "TMT", ## Beltran 2016
            "TMT", ## Beltran 2016
            "TMT", ## Beltran 2016
            "TMT", ## Beltran 2016
            "TMT", ## Beltran 2016
            "TMT", ## Beltran 2016
            "TMT", ## Beltran 2016
            "TMT", ## Beltran 2016
            "iTRAQ", ## Dunkley, re-analysed in Breckels 2013
            "LF", ## Foster
            "iTRAQ", ## Groen
            "iTRAQ", ## Hall
            "SILAC", ## Hirst 2018
            "TMT", ## hyperLOPIT
            "TMT", ## hyperLOPIT
            "TMT", ## Thul 2017
            "TMT", ## Thul 2017
            "SILAC", ## Itzhak 2016
            "LF", ## Itzhak 2017
            "iTRAQ", ## Nikolovski 2012
            "LF", ## Nikolovski 2014
            "LF", ## Rodriguez Pineiro
            "iTRAQ", ## Tan, re-analised in Breckels 2013
            "iTRAQ") ## Trotter (dunkley/sadowski), with Breckels 2013 clusters
## mstech <- sub("iTRAQ", "isobaric", mstech)
## mstech <- sub("TMT", "isobaric", mstech)
stopifnot(identical(length(mstech), nrow(pdtab)))
pdtab$MS <- mstech

sp <- c("Mouse", ## E14 from Breckels 2016
        "Human", ## HEK293T2011, from Breckels 2013
        "Mouse", ## Andreyev
        "Human", ## Beltran 2016
        "Human", ## Beltran 2016
        "Human", ## Beltran 2016
        "Human", ## Beltran 2016
        "Human", ## Beltran 2016
        "Human", ## Beltran 2016
        "Human", ## Beltran 2016
        "Human", ## Beltran 2016
        "Human", ## Beltran 2016
        "Human", ## Beltran 2016
        "Arabidopsis", ## Dunkley, re-analysed in Breckels 2013
        "Mouse", ## Foster
        "Arabidopsis", ## Groen
        "Chicken", ## Hall
        "Human", ## Hirst 2018
        "Mouse", ## hyperLOPIT
        "Mouse", ## hyperLOPIT
        "Human", ## Thul 2017
        "Human", ## Thul 2017
        "Human", ## Itzhak 2016
        "Mouse", ## Itzhak 2017
        "Arabidopsis", ## Nikolovski 2012
        "Arabidopsis", ## Nikolovski 2014
        "Human", ## Rodriguez Pineiro
        "Fly", ## Tan, re-analised in Breckels 2013
        "Arabidopsis") ## Trotter (dunkley/sadowski), with Breckels 2013 clusters
stopifnot(identical(length(sp), nrow(pdtab)))
pdtab$Species <- sp


o <- order(pdtab[, "Clusters"], decreasing = TRUE)
pdataitems <- pdataitems[o]
pdtab <- pdtab[o, ]

## ----pdtab, results = 'asis'---------------------------------------------
toprint <- !names(pdtab) %in% c("year", "MS", "Species")
print(xtable(pdtab[, toprint],
             label = "tab:pdtab",
             caption = "Summary of the datasets used in this study. The percentage of variance along the principal components (PC) is related to the PCA plots on figure~\\ref{fig:pca}. All datasets are available in the \\Biocexptpkg{pRolocdata} package."),
      include.rownames = FALSE,
      booktabs = TRUE,
      floating = FALSE,
      tabular.environment = "longtable")

## ----foster2006preproc---------------------------------------------------
mmu <- pRolocmarkers("mmus")
fData(foster2006)$markers <- NULL
foster2006 <- addMarkers(foster2006, mmu, fcol = "UniProt",
                         verbose = FALSE)

## ----minMarkers5---------------------------------------------------------

tmp <- sapply(pdataitems,
       function(x) {
           obj <- get(x)
           obj <- minMarkers(obj, 7)
           fData(obj)$markers <- fData(obj)$markers7
           fData(obj)$markers5 <- NULL
           assign(x, obj, envir = globalenv())
       })


## ----density, fig.show = 'hide'------------------------------------------
plot2D(hyperLOPIT2015, fcol = NULL, col = "black")
plot2D(hyperLOPIT2015, fcol = NULL, col = "#00000020", pch = 19)
plot2D(hyperLOPIT2015, method = "hexbin")

## ----hexbin, fig.show = 'hide'-------------------------------------------
suppressMessages(hl <- commonFeatureNames(hyperLOPIT2015, hyperLOPIT2015ms2))
plot2D(hl[[1]], method = "hexbin")
plot2D(hl[[2]], method = "hexbin")
plot2D(hall2009, method = "hexbin")

## ----pcahl, out.width = '75%'--------------------------------------------
plot2D(hyperLOPIT2015)
addLegend(hyperLOPIT2015, where = "bottomleft", cex = .6)

## ----plotDist, fig.width = 12, fig.height = 6----------------------------
par(mfrow = c(1, 2))
m <- markerMSnSet(hyperLOPIT2015)
plot2D(m)
par(las = 2, cex.axis = .7)
plotDist(m[, order(m$TMT.Reagent)], fcol = "markers",
         fractions = "TMT.Reagent")
addLegend(m, where = "top", ncol = 2, cex = .8)

## ----qsepplot, echo=FALSE------------------------------------------------
## To address a reviewers suggestion to use the same scale across
## different QSep heatmaps, this function, which is based on the
## levelPlot,QSep method, allows to pass a range of values to be used
## on the colour scale.
.qsepLevelPlot <- function(object, norm = TRUE, ..., rr) {
    pal <- colorRampPalette(c("blue", "white", "red"))
    myPanel <- function(x, y, z, ...) {
        panel.levelplot(x, y, z, ...)
        panel.text(x, y, ifelse(is.na(z), "", round(z, 2)))
    }
    n <- 50
    lattice::levelplot(t(qsep(object, norm = norm)),
                       col.regions = pal(n),
                       panel = myPanel,
                       xlab = "Reference cluster", ylab = "",
                       scales = list(x = list(cex = .8, rot = 45),
                                     y = list(cex = .8)),
                       at = seq(min(rr), max(rr), length.out = n),
                       ...)
}

## ----qsep0---------------------------------------------------------------
qh <- QSep(hyperLOPIT2015)
names(qh) <- sub("/", "\n", names(qh))
qi <- QSep(itzhak2016stcSILAC)
qhmed <- median(summary(qh, verbose = FALSE))
qimed <- median(summary(qi, verbose = FALSE))

## ----qsep0lv, fig.width = 9, fig.height = 9, fig.show = 'hide'-----------
rr <- range(c(qsep(qi), qsep(qh)))
print(levelPlot(qh, norm = FALSE))
print(.qsepLevelPlot(qh, norm = TRUE, rr = rr))
print(.qsepLevelPlot(qi, rr = rr))

## ----qsep0bx, fig.show = 'hide'------------------------------------------
par(mar = c(4, 10, 2, 2), cex.axis = .8)
plot(qh, norm = FALSE, xlab = "QSep score")
plot(qh, norm = TRUE, xlab = "QSep score")

## ----qsepcmp, fig.show = 'hide'------------------------------------------
par(mar = c(2.2, 10, 0, 1), cex.axis = .8)
plot(qh, ylim = c(0.9, 17))
abline(v = qhmed, lty = "dashed")
legend("topright", legend = "hyperLOPIT2015", bty = "n")
plot(qi, ylim = c(0.9, 17))
abline(v = qimed, lty = "dashed")
legend("topright", legend = "itzhak2016stcSILAC", bty = "n")

## ----nummarkers----------------------------------------------------------
nmh <- length(getMarkerClasses(hyperLOPIT2015ms2))
nmi <- length(getMarkerClasses(itzhak2016stcSILAC))
nrh <- nrow(hyperLOPIT2015ms2)
nri <- nrow(itzhak2016stcSILAC)

## ----pcacmp, fig.width = 8, fig.height = 8, fig.asp = 1/4----------------
par(mar = c(4, 4, 0, 0), mfrow = c(1, 4))
plot2D(impute(itzhak2016stcSILAC, "zero"), fcol = NULL, col = "#00000020")
plot2D(impute(itzhak2016stcSILAC, "zero"))
plot2D(hyperLOPIT2015, fcol = NULL, col = "#00000020")
plot2D(hyperLOPIT2015)

## ----qsep----------------------------------------------------------------
res <- pdapply(pdataitems,
       function(x) {
           stopifnot("markers" %in% fvarLabels(x))
           summary(QSep(x), verbose = FALSE)
})

o <- order(mds <- sapply(res, median))
pdtab$mds <- mds

## ----figqsep, out.width = '65%'------------------------------------------
par(las = 1, mar = c(3, 8, 1, 1))
boxplot(res[o], horizontal = TRUE, cex.axis = .7, ylim = c(0, 16))
abline(v = 1, lty = "dotted")

## ----nitz----------------------------------------------------------------
nitz <- pdtab[pdtab$Data == "itzhak2016stcSILAC", "Clusters"]

## ----fighexpca, fig.show = 'hide'----------------------------------------
tmp <- sapply(pdataitems[rev(o)], function(x)
    plot2D(impute(get(x), method = "zero"), method = "hexbin",
           dims = c(1, ifelse(x == "itzhak2016stcSILAC", 3, 2)),
           main = x))

## ----figpca, fig.show = 'hide'-------------------------------------------
par(mar = c(5, 4, 2, 1))
tmp <- sapply(pdataitems[rev(o)], function(x)
    plot2D(impute(get(x), method = "zero"), main = x,
           cex = 1,
           dims = c(1, ifelse(x == "itzhak2016stcSILAC", 3, 2))))

## ----allqseps, fig.show = 'hide'-----------------------------------------
par(mar = c(4, 10, 2, 2), cex.axis = .9, cex.main = 2)
tmp <- sapply(pdataitems[rev(o)],
              function(x) plot(QSep(get(x)), main = x))

## ----allhmaps, fig.show = 'hide'-----------------------------------------
for (x in pdataitems[rev(o)])
    print(levelPlot(QSep(get(x)), main = x))

## ----simcoefs------------------------------------------------------------
load("hlsim.rda")
load("e14sim.rda")
hllm <- lm(median ~ as.numeric(n), data = hlsim$dd)
hlslope <- hllm$coefficients[[2]]
e14lm <- lm(median ~ as.numeric(n), data = e14sim$dd)
e14slope <- e14lm$coefficients[[2]]

## ----mrkswtchstats-------------------------------------------------------
load("mrkswtch.rda")
mrkswtch <- lapply(mrkswtch, log)
## From figure
## +-------p3-------+
## | +---p1----+    | +---p2---+
## | |         |    | |        |
## dhl.mhl dhl.mit dit.mhl dit.mit
##           |                |
##           +-------p4-------+
p1 <- t.test(mrkswtch[["dhl.mhl"]], mrkswtch[["dhl.mit"]])$p.value
p2 <- t.test(mrkswtch[["dit.mhl"]], mrkswtch[["dit.mit"]])$p.value
## p3 <- t.test(mrkswtch[["dhl.mhl"]], mrkswtch[["dit.mhl"]])$p.value
## p4 <- t.test(mrkswtch[["dhl.mit"]], mrkswtch[["dit.mit"]])$p.value

## ----restime, out.width = '60%'------------------------------------------
ggplot(data = pdtab, aes(x = year, y = mds)) +
  geom_point() +
  geom_text_repel(aes(label = Data)) +
  xlab("Year of publication") +
  ylab("Quantitative assessment (median)")

## ----ref2, fig.width = 13, fig.height = 13-------------------------------
p1 <- ggplot(data = pdtab, aes(x = year, y = mds)) +
    geom_point(aes(colour = MS, size = I(3), alpha = I(0.5))) +
    geom_text_repel(aes(label = Data)) +
    xlab("Year of publication") +
    ylab("Quantitative assessment (median)") +
    theme(legend.position = "top")
p2 <- ggplot(data = pdtab, aes(x = year, y = mds)) +
    geom_point(aes(colour = Species, size = I(3), alpha = I(0.5))) +
    geom_text_repel(aes(label = Data)) +
    xlab("Year of publication") +
    ylab("") +
    theme(legend.position = "top")
p3 <- ggplot(data = pdtab, aes(x = MS, y = mds)) +
    geom_boxplot(aes(fill = MS)) +
    ylab("Quantitative assessment (median)") +
    theme(legend.position = "none")
p4 <- ggplot(data = pdtab, aes(x = Species, y = mds)) +
    geom_boxplot(aes(fill = Species)) +
    ylab("") +
    theme(legend.position = "none")
p1 + p2 + p3 + p4 + plot_layout(ncol = 2)

## ----si, results = 'asis'------------------------------------------------
toLatex(sessionInfo(), locale = FALSE)

