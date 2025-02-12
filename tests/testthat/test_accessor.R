# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceglider)

test_that("[[ with glider names and original names", {
    directory <- system.file("extdata/sea_explorer/delayed_raw",
        package = "oceglider"
    )
    expect_silent(g <- read.glider.seaexplorer.raw(
        directory = directory,
        pattern = "pld1.raw", yo = 3, progressBar = FALSE
    ))
    expect_equal(g[["temperature"]], g[["GPCTD_TEMPERATURE"]])
    expect_equal(g[["conductivity"]], g[["GPCTD_CONDUCTIVITY"]])
})
