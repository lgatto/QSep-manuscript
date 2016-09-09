library("pRoloc")
library("pRolocdata")
data(itzhak2016stcSILAC)
data(hyperLOPIT2015)

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

hyperLOPIT2015 <- addMarkers(hyperLOPIT2015, mm2, mcol = "markers2")

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
hlmm <- getMarkers(hlm)
names(hlmm) <- fData(hlm)$Entry

itzhak2016stcSILAC <- addMarkers(itzhak2016stcSILAC, hlmm, mcol = "markers2")



pdf("mrkswtch-pca.pdf")
par(mfrow = c(2, 2))
plot2D(hyperLOPIT2015, fcol = "markers", main = "hyperLOPIT2015 markers")
addLegend(hyperLOPIT2015, cex = .6)
plot2D(hyperLOPIT2015, fcol = "markers2", main = "itzhak2016stcSILAC markers")
plot2D(impute(itzhak2016stcSILAC, "zero"), fcol = "markers2", main = "hyperLOPIT2015 markers")
plot2D(impute(itzhak2016stcSILAC, "zero"), fcol = "markers", main = "itzhak2016stcSILAC markers")
addLegend(itzhak2016stcSILAC, cex = .6)
dev.off()

mrkswtch <- list(hl1 = summary(QSep(hyperLOPIT2015, fcol = "markers"), verbose = FALSE),
                 hl2 = summary(QSep(hyperLOPIT2015, fcol = "markers2"), verbose = FALSE),
                 it1 = summary(QSep(itzhak2016stcSILAC, fcol = "markers"), verbose = FALSE),
                 it2 = summary(QSep(itzhak2016stcSILAC, fcol = "markers2"), verbose = FALSE))

## boxplot(mrkswtch)

mrkdf <- rbind(data_frame(QSep = mrkswtch[[1]], data = "d:hyperLOPIT2015", markers = "m:hyperLOPIT2015"),
               data_frame(QSep = mrkswtch[[2]], data = "d:hyperLOPIT2015", markers = "m:itzhak2016stcSILAC"),
               data_frame(QSep = mrkswtch[[3]], data = "d:itzhak2016stcSILAC", markers = "m:itzhak2016stcSILAC"),
               data_frame(QSep = mrkswtch[[4]], data = "d:itzhak2016stcSILAC", markers = "m:hyperLOPIT2015"))

pdf("mrkswtch-qsep.pdf")
ggplot(aes(markers, QSep), data = mrkdf) + geom_boxplot() + facet_wrap(~ data) + xlab("")
dev.off()
