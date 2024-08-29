library(oce)
library(oceglider)
g1 <- read.glider.seaexplorer.delayed(".")
rtspo <- 1.5 * 3600
g2 <- read.glider.seaexplorer.delayed(".", removeTimeSincePowerOn = rtspo)
g2[["time"]][1] - g1[["time"]][1]

if (!interactive()) {
    png("02_plot_%d.png",
        unit = "in", width = 7, height = 5,
        res = 200, pointsize = 11
    )
}

for (g in list(g1, g2)) {
    par(mfrow = c(2, 2))
    for (field in c("SA", "CT")) {
        var <- g[[field]]
        oce.plot.ts(g[["time"]], var, ylab = field, type = "p", cex = 0.5)
        cm <- colormap(var,
            zlim = quantile(var, c(0.01, 0.9), na.rm = TRUE),
            col = oceColorsTurbo
        )
        par(mar = c(3, 3, 1, 1))
        drawPalette(colormap = cm)
        plot(g,
            which = 1, type = "p", pch = 20, cex = 0.5,
            col = cm$zcol,
            mar = c(3, 3, 1, 4),
            drawTimeRange = FALSE
        )
        mtext(field, adj = 1, cex = par("cex"))
    }
}

if (!interactive()) {
    dev.off()
}
