# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oceglider)
# library(testthat)

# Oxygen calibration file
#   SEA021_CTD_DO_calib_files_SN0184_2022
# in directory
#   ~/data/glider/sea021/M71/SEA021_CTD_DO_calib_files_SN0184_2022
oxycalib <- list(
    serialNumber = "43-3336",
    model = "SBE43F",
    calibrationDate = "20220201",
    calibrationCoefficients = data.frame(
        Soc = 3.0667e-4,
        Foffset = -878.86,
        Tau20 = 1.26,
        A = -4.6593e-3,
        B = 1.7954e-4,
        C = -2.2483e-6,
        Enom = 0.036
    )
)

test_that("oxygen computed against previously-computed values", {
    directory <- system.file("extdata/seaexplorer/sub", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory = directory, yo = 3, progressBar = FALSE))
    g@metadata$oxycalib <- oxycalib
    # consistency check (will fail if dataset is changed or code changes)
    options(digits = 15)
    g[["oxygen"]][1:3]
    expect_equal(g[["oxygen"]][1:3], c(363.265784249780, 363.301998679272, 363.257645877092))
    # test values from the calibration sheet
})

test_that("oxygen computed against calibration sheet", {
    # These few lines from the calibration sheet (I'm too lazy to type over
    # a dozen lines) should be enough to catch problems
    dataText <- "
oxygen temperature salinity pressure oxygenFrequency
1.11  2 0 0 1256.172
3.90 12 0 0 2627.269
6.90 26 0 0 5005.613
"
    calibrationData <- read.table(text = dataText, header = TRUE)
    directory <- system.file("extdata/seaexplorer/sub", package = "oceglider")
    expect_silent(g <- read.glider.seaexplorer.realtime(directory = directory, yo = 3, progressBar = FALSE))
    g@metadata$oxycalib <- oxycalib
    g@data$payload1 <- calibrationData[, 2:5]
    # convert from umol/kg to ml/l
    O2 <- g[["oxygen"]] / 44.6591
    expect_equal(O2, calibrationData$oxygen, tolerance = 1e-2)
})
