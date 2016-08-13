library("pRoloc")
library("pRolocdata")

i <- c(2, 6, 10, 14, 15, 20, 22, 28, 36, 37, 38)
pdata <- pRolocdata()
pdata <- pdata$results[i, "Item"]

data(list = pdata)

mmu <- pRolocmarkers("mmus")
fData(foster2006)$markers <- NULL
foster2006 <- addMarkers(foster2006, mmu, fcol = "UniProt")

res <- sapply(pdata,
       function(x) {
           xx <- get(x)
           stopifnot("markers" %in% fvarLabels(xx))
           summary(QSep(xx), verbose = FALSE)
})

o <- order(sapply(res, median))
## boxplot(res[o], horizontal = TRUE, cex.axis = .7)



opar <- par(las = 1, mar = c(3, 8, 1, 1))
boxplot(res[o], horizontal = TRUE, cex.axis = .7, ylim = c(0, 16))
abline(v = 1, lty = "dotted")
par(opar)


x11()
par(mfrow = c(4, 3), mar = c(2, 2, 2, 2))
tmp <- sapply(pdata[rev(o)], function(x)
    plot2D(impute(get(x), method = "zero"), main = x,
           cex.axis = .6, tck = -0.025,
           dims = c(1, ifelse(x == "itzhak2016stcSILAC", 3, 2))))
