# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
#<20240903 specific to old test files> library(oceglider)
#<20240903 specific to old test files> # library(testthat)
#<20240903 specific to old test files> 
#<20240903 specific to old test files> # Oxygen calibration file
#<20240903 specific to old test files> #   SEA021_CTD_DO_calib_files_SN0184_2022
#<20240903 specific to old test files> # in directory
#<20240903 specific to old test files> #   ~/data/glider/sea021/M71/SEA021_CTD_DO_calib_files_SN0184_2022
#<20240903 specific to old test files> oxycalib <- list(
#<20240903 specific to old test files>     serialNumber = "43-3336",
#<20240903 specific to old test files>     model = "SBE43F",
#<20240903 specific to old test files>     calibrationDate = "20220201",
#<20240903 specific to old test files>     calibrationCoefficients = data.frame(
#<20240903 specific to old test files>         Soc = 3.0667e-4,
#<20240903 specific to old test files>         Foffset = -878.86,
#<20240903 specific to old test files>         Tau20 = 1.26,
#<20240903 specific to old test files>         A = -4.6593e-3,
#<20240903 specific to old test files>         B = 1.7954e-4,
#<20240903 specific to old test files>         C = -2.2483e-6,
#<20240903 specific to old test files>         Enom = 0.036
#<20240903 specific to old test files>     )
#<20240903 specific to old test files> )
#<20240903 specific to old test files> 
#<20240903 specific to old test files> test_that("oxygen computed against previously-computed values", {
#<20240903 specific to old test files>     directory <- system.file("extdata/seaexplorer/sub", package = "oceglider")
#<20240903 specific to old test files>     expect_silent(g <- read.glider.seaexplorer.realtime(directory = directory, yo = 3, progressBar = FALSE))
#<20240903 specific to old test files>     g@metadata$oxycalib <- oxycalib
#<20240903 specific to old test files>     # consistency check (will fail if dataset is changed or code changes)
#<20240903 specific to old test files>     # options(digits = 15)
#<20240903 specific to old test files>     # g[["oxygen"]][1:3]
#<20240903 specific to old test files>     expect_equal(g[["oxygen"]][1:3], c(354.508135471242, 354.543025470803, 354.499770611843))
#<20240903 specific to old test files>     # test values from the calibration sheet
#<20240903 specific to old test files> })
#<20240903 specific to old test files> 
#<20240903 specific to old test files> test_that("oxygen computed against calibration sheet", {
#<20240903 specific to old test files>     # These few lines from the calibration sheet (I'm too lazy to type over
#<20240903 specific to old test files>     # a dozen lines) should be enough to catch problems
#<20240903 specific to old test files>     dataText <- "
#<20240903 specific to old test files> oxygen temperature salinity pressure oxygenFrequency
#<20240903 specific to old test files> 1.11  2 0 0 1256.172
#<20240903 specific to old test files> 3.90 12 0 0 2627.269
#<20240903 specific to old test files> 6.90 26 0 0 5005.613
#<20240903 specific to old test files> "
#<20240903 specific to old test files>     calibrationData <- read.table(text = dataText, header = TRUE)
#<20240903 specific to old test files>     directory <- system.file("extdata/seaexplorer/sub", package = "oceglider")
#<20240903 specific to old test files>     expect_silent(g <- read.glider.seaexplorer.realtime(directory = directory, yo = 3, progressBar = FALSE))
#<20240903 specific to old test files>     g@metadata$oxycalib <- oxycalib
#<20240903 specific to old test files>     g@data$payload1 <- calibrationData[, 2:5]
#<20240903 specific to old test files>     # convert from umol/kg to ml/l
#<20240903 specific to old test files>     O2 <- g[["oxygen"]] / 44.6591
#<20240903 specific to old test files>     expect_equal(O2, calibrationData$oxygen, tolerance = 1e-2)
#<20240903 specific to old test files> })
