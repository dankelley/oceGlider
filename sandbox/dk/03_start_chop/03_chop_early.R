library(oce)
library(oceglider)
if (!exists("g1")) {
    g1 <- read.glider.seaexplorer.delayed(".")
    rtspo <- 1.5 * 3600
    g2 <- read.glider.seaexplorer.delayed(".", removeTimeSincePowerOn = rtspo)
}
sigma0 <- g1[["sigma0"]]
SA <- g1[["salinity"]]
CT <- g1[["temperautre"]]
time <- g1[["time"]]

sigma0Cutoff <- quantile(sigma0, 0.02, na.rm = TRUE)
SACutoff <- quantile(SA, 0.02, na.rm = TRUE)
bad <- sigma0 < sigma0Cutoff | SA < SACutoff

if (!interactive()) {
    png("03_chop_early.png",
        unit = "in", width = 7, height = 3.5,
        res = 200, pointsize = 11
    )
}
par(mfrow = c(1, 3))
plot(g1[["salinity"]], g1[["temperature"]])
points(g1[["salinity"]][bad], g1[["temperature"]][bad], pch = 20, cex = 0.5, col = 2)
drawIsopycnals(eos = "unesco")

# Middle
plot(g2[["salinity"]], g2[["temperature"]])
drawIsopycnals(eos = "unesco")
mtext(
    sprintf(
        "removeTimeSincePowerOn=%d\nskip %.2fh",
        rtspo,
        as.numeric(difftime(g2[["time"]][1], g1[["time"]][1], "hour"))
    ),
    line = 1, cex = par("cex")
)

# Right: new method
firstGood <- 1 + tail(which(bad), 1)
firstGoodTime <- time[firstGood]
skipHours <- as.numeric(difftime(g1[["time"]][firstGood], g1[["time"]][1], "hour"))
g3 <- subset(g1, time > firstGoodTime)
plot(g3[["salinity"]], g3[["temperature"]])
drawIsopycnals(eos = "unesco")
mtext(sprintf("from quantiles\nskip %.2fh", skipHours), line = 1, cex = par("cex"))

if (!interactive()) {
    dev.off()
}
