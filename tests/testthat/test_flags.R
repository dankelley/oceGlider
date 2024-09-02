# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceglider)
# library(testthat)

test_that("read.glider.seaexplorer.realtime flag names", {
    directory <- system.file("extdata/sea_explorer/delayed_mode", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.delayed(directory = directory, yo = 3, progressBar = FALSE))
    expect_equal(
        g@metadata$flagScheme,
        list(
            name = "IOOS",
            mapping = list(good = 1, not_evaluated = 2, questionable = 3, bad = 4, missing = 9),
            default = c(3, 4, 9)
        )
    )
})

test_that("read.glider.seaexplorer.delayed flag names", {
    directory <- system.file("extdata/sea_explorer/delayed_mode", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.delayed(directory = directory, progressBar = FALSE))
    expect_equal(
        g@metadata$flagScheme,
        list(
            name = "IOOS",
            mapping = list(good = 1, not_evaluated = 2, questionable = 3, bad = 4, missing = 9),
            default = c(3, 4, 9)
        )
    )
})

test_that("read.glider.seaexplorer.realtime flag setting and handling", {
    # This is based on the example given by ?"handleFlags,glider-method"
    directory <- system.file("extdata/sea_explorer/realtime_mode", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory))
    # NOTE: this test was more hard-wired before issue40, e.g. it
    # demanded that the number of data read be 2784, but that number
    # became 3435 when read.glider.seaexplorer.delayed() was changed to
    # address issue40.
    n <- length(g[["salinity"]])
    expect_equal(n, sum(g[["salinityFlag"]] == 2))
    lowSalinity <- which(g[["salinity"]] < 31)
    g2 <- setGliderFlags(g, "salinity", g[["salinity"]] < 31, 3)
    expect_true(all(g2[["salinityFlag"]][lowSalinity] == 3))
    g3 <- handleGliderFlags(g2)# , c(3, 4, 9)) # use default action, which is "NA"
    expect_true(all(g3[["salinityFlag"]][lowSalinity] == 3))
    expect_true(all(is.na(g3[["salinity"]][lowSalinity])))
})

test_that("read.glider.seaexplorer.delayed flag setting and handling", {
    # This is based on the example given by ?"handleFlags,glider-method"
    directory <- system.file("extdata/sea_explorer/delayed_mode", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.delayed(directory, progressBar = FALSE))
    # NOTE: this test was more hard-wired before issue40, e.g. it
    # demanded that the number of data read be 2784, but that number
    # became 3435 when read.glider.seaexplorer.delayed() was changed to
    # address issue40.
    n <- length(g[["salinity"]])
    expect_equal(n, sum(g[["salinityFlag"]] == 2))
    lowSalinity <- which(g[["salinity"]] < 31)
    g2 <- setGliderFlags(g, "salinity", g[["salinity"]] < 31, 3)
    expect_true(all(g2[["salinityFlag"]][lowSalinity] == 3))
    g3 <- handleGliderFlags(g2, c(3, 4, 9)) # use default action, which is "NA"
    expect_true(all(g3[["salinityFlag"]][lowSalinity] == 3))
    expect_true(all(is.na(g3[["salinity"]][lowSalinity])))
})
