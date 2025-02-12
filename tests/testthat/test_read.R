# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceglider)
# library(testthat)

test_that("read.glider.seaexplorer.realtime raw", {
    directory <- system.file("extdata/sea_explorer/realtime_raw", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory = directory, progressBar = FALSE))
    expect_equal(c("glider", "payload1"), names(g[["data"]]))
    # dimensionality and names in glider stream
    expect_equal(dim(g[["glider"]]), c(462, 22))
    gliderNamesExpected <- sort(c(
        "alarm", "altitude", "angCmd", "angPos",
        "ballastCmd", "ballastPos", "declination", "depth",
        "desiredH", "heading", "latitude", "linCmd", "linPos", "longitude",
        "navState", "pitch", "pressureInternal", "roll", "temperature", "time",
        "voltage", "yoNumberNav"
    ))
    expect_equal(sort(names(g[["glider"]])), gliderNamesExpected)
    # dimensionality and names in payload1 stream (and payload nickname)
    # This is partly a check against changes to the built-in file.
    expect_equal(dim(g[["payload1"]]), c(339, 19))
    expect_equal(dim(g[["payload"]]), c(339, 19))
    payloadNamesExpected <- c(
        "backscatter", "backscatterCount", "cdom", "cdomCount",
        "chlorophyll", "chlorophyllCount", "conductivity", "latitude",
        "longitude", "navState", "oxygen", "oxygenFrequency",
        "oxygenTemperature", "pressure", "pressureNav", "salinity",
        "temperature", "time", "yoNumber"
    )
    expect_equal(sort(names(g[["payload1"]])), payloadNamesExpected)
    expect_equal(sort(names(g[["payload"]])), payloadNamesExpected)
})

test_that("read.glider.seaexplorer.delayed raw", {
    directory <- system.file("extdata/sea_explorer/delayed_raw", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.realtime(
        directory = directory,
        pattern = "pld1.raw",
        progressBar = FALSE
    ))
    payloadNamesExpected <- sort(c(
        "backscatter", "backscatterCount", "cdom",
        "cdomCount", "chlorophyll", "chlorophyllCount",
        "conductivity", "latitude", "longitude",
        "navState", "oxygen", "oxygenFrequency",
        "oxygenTemperature", "pressure", "pressureNav",
        "salinity", "temperature", "time",
        "yoNumber"
    ))
    expect_equal(sort(names(g@data)), payloadNamesExpected)
})
