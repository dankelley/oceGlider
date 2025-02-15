# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceglider)
# library(testthat)

test_that("read.glider.seaexplorer.realtime raw", {
    directory <- system.file("extdata/sea_explorer/realtime_raw", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.raw(directory = directory, pattern = "pld1.sub", progressBar = FALSE))
    # dimensionality and names in glider stream
    expect_equal(dim(g[["data"]]), c(339, 19))
    gliderNamesExpected <- c(
        "backscatter", "backscatterCount", "cdom", "cdomCount", "chlorophyll",
        "chlorophyllCount", "conductivity", "latitude", "longitude",
        "navState", "oxygen", "oxygenFrequency", "oxygenTemperature",
        "pressure", "pressureNav", "salinity", "temperature", "time",
        "yoNumber"
    )
    expect_equal(sort(names(g[["data"]])), gliderNamesExpected)
    # dimensionality and names in payload1 stream (and payload nickname)
    # This is partly a check against changes to the built-in file.
    expect_equal(dim(g[["data"]]), c(339, 19))
    payloadNamesExpected <- c(
        "backscatter", "backscatterCount", "cdom", "cdomCount",
        "chlorophyll", "chlorophyllCount", "conductivity", "latitude",
        "longitude", "navState", "oxygen", "oxygenFrequency",
        "oxygenTemperature", "pressure", "pressureNav", "salinity",
        "temperature", "time", "yoNumber"
    )
    expect_equal(sort(names(g[["data"]])), payloadNamesExpected)
})

test_that("read.glider.seaexplorer.realtime", {
    directory <- system.file("extdata/sea_explorer/delayed_raw", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.raw(
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
