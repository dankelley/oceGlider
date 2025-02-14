# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceglider)

test_that("degreeMinute() works as expected", {
    x <- 1234.56
    expect_equal(12 + 34.56 / 60, degreeMinute(x))
    x <- -x
    expect_equal(-(12 + 34.56 / 60), degreeMinute(x))
})

test_that("subset seaexplorer by 'ascending' and descending", {
    directory <- system.file("extdata/sea_explorer/realtime_raw", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory = directory, progressBar = FALSE))
    expect_silent(ga <- subset(g, "ascending"))
    expect_silent(gd <- subset(g, "descending"))
})

test_that("subset seaexplorer by pressure", {
    directory <- system.file("extdata/sea_explorer/realtime_raw", package = "oceglider")
    expect_silent(d <- read.glider.seaexplorer.realtime(directory = directory, progressBar = FALSE))
    deep <- d[["pressure"]] > 20
    deep[is.na(deep)] <- FALSE
    expect_silent(gdeep <- subset(d, pressure > 20))
    expect_equal(gdeep[["payload1"]], d[["payload1"]][deep, ])
})
