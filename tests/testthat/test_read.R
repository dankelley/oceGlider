# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceglider)
# library(testthat)

test_that("read.glider.seaexplorer.realtime", {
    directory <- system.file("extdata/seaexplorer/sub", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory = directory, yo = 2, progressBar = FALSE))
    expect_output(summary(g), "Input file:")
    expect_equal(c("glider", "payload1"), names(g[["data"]]))
    # dimensionality and names in glider stream
    expect_equal(dim(g[["glider"]]), c(95, 23))
    gliderNamesExpected <- sort(c(
        "alarm", "altitude", "angCmd", "angPos",
        "ballastCmd", "ballastPos", "DeadReckoning", "declination", "depth",
        "desiredH", "heading", "latitude", "linCmd", "linPos", "longitude",
        "navState", "pitch", "pressureInternal", "roll", "temperature", "time",
        "voltage", "yoNumberNav"
    ))
    expect_equal(sort(names(g[["glider"]])), gliderNamesExpected)
    # dimensionality and names in payload1 stream (and payload nickname)
    # This is partly a check against changes to the built-in file.
    expect_equal(dim(g[["payload1"]]), c(1763, 22))
    expect_equal(dim(g[["payload"]]), c(1763, 22))
    payloadNamesExpected <- c(
        "backscatter", "backscatterCount", "cdom",
        "cdomCount", "chlorophyll", "chlorophyllCount", "conductivity",
        "conductivity2", "conductivityTemperature", "latitude", "longitude",
        "navState", "oxygenFrequency", "pressure", "pressure2", "pressureNav",
        "salinity", "salinity2", "temperature", "temperature2", "time",
        "yoNumber"
    )
    expect_equal(sort(names(g[["payload1"]])), payloadNamesExpected)
    expect_equal(sort(names(g[["payload"]])), payloadNamesExpected)
})

test_that("read.glider.seaexplorer.delayed", {
    directory <- system.file("extdata/seaexplorer/raw", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.delayed(directory = directory, progressBar = FALSE))
    # Note that this data structure does not have a "glider" component.
    # The purpose in testing for this is to ensure that if that component
    # gets added, a developer will notice the change and invent new tests
    # for that component.
    expect_equal(names(g@data), "payload1")
    sort(names(g[["payload1"]]))
    payloadNamesExpected <- sort(c(
        "backscatter", "backscatterCount", "cdom",
        "cdomCount", "chlorophyll", "chlorophyllCount", "conductivity",
        "conductivity2", "conductivityTemperature", "latitude", "longitude",
        "navState", "oxygenFrequency", "pressure", "pressure2", "pressureNav",
        "salinity", "temperature", "temperature2", "time", "yoNumber"
    ))
    expect_equal(sort(names(g@data$payload1)), payloadNamesExpected)
    expect_equal(sort(names(g[["payload1"]])), payloadNamesExpected)
    expect_equal(sort(names(g[["payload"]])), payloadNamesExpected)
})
