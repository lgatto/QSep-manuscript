library("ggplot2")
library("gridExtra")
library("pRoloc")
library("pRolocdata")

runsim <- function(object) {
    xx <- markerMSnSet(object)
    umm <- unique(getMarkers(xx, verbose = FALSE))
    ## all classes
    res0 <- summary(QSep(xx), verbose = FALSE)
    ## one class removed
    res1 <- lapply(umm, function(x) {
        obj <- fDataToUnknown(xx, from = x)
        summary(QSep(obj), verbose = FALSE)
    })
    ## two classes removed
    k <- combn(length(umm), 2)
    res2 <- lapply(data.frame(k),
                   function(i) {
                       obj <- fDataToUnknown(xx, from = umm[i[1]])
                       obj <- fDataToUnknown(obj, from = umm[i[2]])
                       summary(QSep(obj), verbose = FALSE)
                   })
    ## three classes removed
    k <- combn(length(umm), 3)
    res3 <- lapply(data.frame(k),
                   function(i) {
                       obj <- fDataToUnknown(xx, from = umm[i[1]])
                       obj <- fDataToUnknown(obj, from = umm[i[2]])
                       obj <- fDataToUnknown(obj, from = umm[i[3]])
                       summary(QSep(obj), verbose = FALSE)
                   })
    ## combine results
    sim <- c(list(res0), res1, res2, res3)
    n <- c(0,
           rep(1, length(res1)),
           rep(2, length(res2)),
           rep(3, length(res3)))
    mds <- sapply(sim, median)
    dd <- data.frame(median = mds[-1], n = factor(n[-1]))
    return(list(dd = dd, sim = sim, n = n, mds = mds))
}


if (!file.exists("hlsim.rda")) {
    data(hyperLOPIT2015)
    hlsim <- runsim(hyperLOPIT2015)
    save(hlsim, file = "hlsim.rda")
} else {
    load("hlsim.rda")
}

if (!file.exists("hlsim.rda")) {
    data(E14TG2aS1)
    e14sim <- runsim(E14TG2aS1)
    save(e14sim, file = "e14sim.rda")
} else {
    load("e14sim.rda")
}


p1 <- ggplot(data = hlsim$dd, aes(y = median, x = n)) +
    geom_boxplot() + geom_jitter() +
    geom_hline(yintercept = hlsim$mds[1]) +
    ggtitle("hyperLOPIT2015")

p2 <- ggplot(data = e14sim$dd, aes(y = median, x = n)) +
    geom_boxplot() + geom_jitter() +
    geom_hline(yintercept = e14sim$mds[1]) +
    ggtitle("E14TG2aS1")


## pdf("simn.pdf", width = 12, height = 6)
## grid.arrange(p1, p2, nrow = 1)
## dev.off()


d1 <- hlsim$dd
d2 <- e14sim$dd
dd <- rbind(d1, d2)
ymds <- data.frame(data = c("hyperLOPIT2015", "E14TG2aS1"),
                   mds = c(hlsim$mds[1], e14sim$mds[1]))

pdf("simn.pdf", width = 12, height = 6)
ggplot(data = dd, aes(y = median, x = n)) + geom_boxplot() +
    geom_jitter() + facet_grid(. ~ data) +
    geom_hline(aes(yintercept = mds), ymds, col = "red") +
    xlab("Number of removed classes") +
    ylab("Median QSep score") +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text.x = element_text(size = 14))
dev.off()
