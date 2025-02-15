# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceglider)

test_that("subset seaexplorer by 'ascending' and 'descending'", {
    directory <- system.file("extdata/sea_explorer/delayed_raw", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.raw(directory, pattern = "pld1.raw", yo = 3, progressBar = FALSE))
    expect_silent(ga <- subset(g, "ascending"))
    expect_silent(gd <- subset(g, "descending"))
})

test_that("subset seaexplorer by pressure", {
    directory <- system.file("extdata/sea_explorer/delayed_raw", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.raw(directory, pattern = "pld1.raw", yo = 3, progressBar = FALSE))
    deep <- g[["pressure"]] > 20
    deep[is.na(deep)] <- FALSE
    expect_silent(gdeep <- subset(g, pressure > 20))
    expect_equal(gdeep[["payload1"]], g[["payload1"]][deep, ])
})
