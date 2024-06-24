# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceglider)
# library(testthat)

test_that("[[ with glider names and original names", {
    directory <- system.file("extdata/seaexplorer/sub", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory = directory, yo = 3, progressBar = FALSE))
    # items in @data$payload1
    expect_equal(g[["temperature"]], g[["GPCTD_TEMPERATURE"]])
    expect_equal(g[["conductivity"]], g[["GPCTD_CONDUCTIVITY"]])
    # items in @data$glider
    expect_equal(g[["pitch"]], g[["Pitch"]])
    expect_equal(g[["altitude"]], g[["Altitude"]])
    # NOTE: I was going to check navState, but I see that in both the glider and
    # payload1 streams AND THEY DIFFER SLIGHTLY. I do not understand the data
    # format well enough to know why they differ, and am leaving this note for
    # future references.
})
