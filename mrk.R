library("pRoloc")
library("pRolocdata")
data(itzhak2016stcSILAC)
data(hyperLOPIT2015)
fData(hyperLOPIT2015)$TAGM <- NULL

library("readr")
library("dplyr")

## Mapping from human to mouse

## read UniProt mapping
map <- readr::read_tsv("./data/uniprot-yourlist%3AM2016080814483A1C7ED25EE8374758DF3FD545FD22E8D6F.tab.gz")[, -2]
names(map)[1] <- "GN"
fvarLabels(itzhak2016stcSILAC)[1] <- "GN"
map <- map[, 1:3]

## update feature data
map <- map[!duplicated(map[, 1]), ]
fd2 <- dplyr::left_join(fData(itzhak2016stcSILAC), map)
rownames(fd2) <- fd2[, 2]
fData(itzhak2016stcSILAC) <- fd2

## create new marker vector
m2 <- markerMSnSet(itzhak2016stcSILAC)
m2 <- m2[!is.na(fData(m2)[, "Entry"]), ]
mm2 <- getMarkers(m2, verbose = FALSE)
names(mm2) <- fData(m2)$Entry

hyperLOPIT2015 <- addMarkers(hyperLOPIT2015, mm2,
                             mcol = "markers2", verbose = FALSE)

## Mapping from mouse to human

## read UniProt mapping
map <- readr::read_tsv("./data/uniprot-yourlist%3AM20160808C2335653E4FA1B8AECF5153189FA788F1CC137N.tab.gz")
names(map)[1] <- "ID"
map <- map[!duplicated(map[, 1]), ]

## update feature data
fd2 <- fData(hyperLOPIT2015)
fd2$fn <- featureNames(hyperLOPIT2015)
fd2$ID <- sub("MOUSE", "HUMAN", fData(hyperLOPIT2015)[, 1])
fd2 <- dplyr::left_join(fd2, map)

rownames(fd2) <- fd2$fn
fData(hyperLOPIT2015) <- fd2

## create marker vector
hlm <- markerMSnSet(hyperLOPIT2015)
hlm <- hlm[!is.na(fData(hlm)[, "Entry"]), ]
hlmm <- getMarkers(hlm, verbose = FALSE)
names(hlmm) <- fData(hlm)$Entry

itzhak2016stcSILAC <- addMarkers(itzhak2016stcSILAC, hlmm,
                                 mcol = "markers2", verbose = FALSE)

## rename makers to save space
hyperLOPIT2015 <- fDataToUnknown(hyperLOPIT2015,
                                 from = "Endoplasmic reticulum/Golgi apparatus",
                                 to = "ER/Golgi")
itzhak2016stcSILAC <- fDataToUnknown(itzhak2016stcSILAC, fcol = "markers2",
                                     from = "Endoplasmic reticulum/Golgi apparatus",
                                     to = "ER/Golgi")


pdf("mrkswtch-pca.pdf", width = 10, height = 10)
.cex <- 0.9
par(mfrow = c(2, 2), oma = c(0, 0, 0, 0), mar = c(2, 2, 1, 1))
plot2D(hyperLOPIT2015, fcol = "markers")
addLegend(hyperLOPIT2015, cex = .cex)
plot2D(hyperLOPIT2015, fcol = "markers2")
addLegend(hyperLOPIT2015, fcol = "markers2", cex = .cex)
plot2D(impute(itzhak2016stcSILAC, "zero"), fcol = "markers2")
addLegend(itzhak2016stcSILAC, fcol = "markers2", cex = .cex)
plot2D(impute(itzhak2016stcSILAC, "zero"), fcol = "markers")
addLegend(itzhak2016stcSILAC, cex = .cex)
dev.off()

mrkswtch <- list(dhl.mhl = summary(QSep(hyperLOPIT2015, fcol = "markers"), verbose = FALSE),
                 dhl.mit = summary(QSep(hyperLOPIT2015, fcol = "markers2"), verbose = FALSE),
                 dit.mhl = summary(QSep(itzhak2016stcSILAC, fcol = "markers2"), verbose = FALSE),
                 dit.mit = summary(QSep(itzhak2016stcSILAC, fcol = "markers"), verbose = FALSE))


save(mrkswtch, file = "mrkswtch.rda")

mrkdf <- rbind(data_frame(QSep = mrkswtch[["dhl.mhl"]], data = "Data: hyperLOPIT2015", markers = "hyperLOPIT2015"),
               data_frame(QSep = mrkswtch[["dhl.mit"]], data = "Data: hyperLOPIT2015", markers = "itzhak2016stcSILAC"),
               data_frame(QSep = mrkswtch[["dit.mhl"]], data = "Data: itzhak2016stcSILAC", markers = "hyperLOPIT2015"),
               data_frame(QSep = mrkswtch[["dit.mit"]], data = "Data: itzhak2016stcSILAC", markers = "itzhak2016stcSILAC"))

pdf("mrkswtch-qsep.pdf")
ggplot(aes(markers, QSep), data = mrkdf) +
     geom_boxplot() +
    facet_wrap(~ data) +
    xlab("Markers") +
    ylab("QSep scores") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          strip.text.x =
              element_text(size = 14))
dev.off()
