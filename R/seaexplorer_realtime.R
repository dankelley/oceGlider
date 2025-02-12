# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read real-time SeaExplorer glider data
#'
#' Reads real-time CSV files produced by a SeaExplorer glider, as
#' detected by the presence of `".sub."` in their names.
#' Such real-time data are decimated before transmission, and thus do not
#' represent the full data collected by the glider sensors.
#' (Use [read.glider.seaexplorer.raw)] instead of
#' this, to read delayed-mode data, as downloaded from the glider
#' after recovery.)
#'
#' @section Flag Scheme:
#' A flag scheme is set up according to the IOOS classification system (see
#' Table 2 of reference 1), as follows.
#'
#' \tabular{llll}{
#' \strong{Name}         \tab \strong{Value} \tab \strong{IOOS Name}            \tab \strong{Description}\cr
#' `pass`           \tab 1              \tab Pass                          \tab Data has passed quality control (QC) tests\cr
#' `not_evaluated`  \tab 2              \tab Not Evaluated                 \tab Data has not been QC tested\cr
#' `suspect`        \tab 3              \tab Suspect or of High Interest   \tab Data is considered to be of suspect or high interest\cr
#' `fail`           \tab 4              \tab Fail                          \tab Data is considered to have failed on one or more QC tests\cr
#' `missing`        \tab 9              \tab Missing Data                  \tab Data are missing; using a placeholder\cr
#' }
#'
#' @references
#' 1. IOOS. “Manual for Real-Time Oceanographic Data Quality Control Flags,” May 2017.
#' https://cdn.ioos.noaa.gov/media/2017/12/QARTOD-Data-Flags-Manual_Final_version1.1.pdf.
#'
#' @param directory The directory in which the realtime SeaExplorer files are located.
#'
#' @param yo A numeric value (or vector) specifying the yo numbers to
#'     read. If this is not provided, [read.glider.seaexplorer.raw()]
#'     will read all yo numbers for which files are present in `dir`.
#'
#' @param level Ignored by [read.glider.seaexplorer.realtime] and
#'     only included for similarity with
#'     [read.glider.seaexplorer.raw()].
#'
#' @param progressBar a logical value that controls whether to indicate the
#' progress made in reading and interpreting the data.  This can be useful,
#' since the work can be slow. The default is to show progress in
#' interactive sessions, but not in scripts.
#'
#' @param missingValue A value that indicates missing data; all
#'     values that match this are set to `NA`.
#'
#' @template debug
#'
#' @template seaexplorer_names
#'
#' @examples
#' library(oceglider)
#' directory <- system.file("extdata/sea_explorer/realtime_raw", package = "oceglider")
#' g <- read.glider.seaexplorer.realtime(directory, progressBar = FALSE)
#' plot(g, which = "navState")
#' plot(g, which = "S")
#' plot(g, which = "T")
## ctd <- as.ctd(g[['salinity']], g[['temperature']], g[['pressure']],
##               longitude=g[['longitude']], latitude=g[['latitude']])
## plot(ctd)
## # Isolate the upcast, inferred with oce::ctdTrim().
## plot(ctdTrim(ctd, "upcast"))
## # Isolate the upcast, using g[["NAV_RESOURCE"]]==117;
## # note that the downcast has code 100.
## plot(subset(ctd, g[["NAV_RESOURCE"]]==117))
#'
#' @family functions for seaexplorer gliders
#' @family functions to read glider data
#'
#' @importFrom utils read.delim
#' @importFrom methods new
#' @importFrom oce swSCTp processingLogAppend
#'
#' @author Dan Kelley and Clark Richards
#'
#' @md
#'
#' @export
read.glider.seaexplorer.realtime <- function(directory, yo, level = 1,
                                             progressBar = interactive(), missingValue = 9999, debug) {
    if (missing(debug)) {
        debug <- getOption("gliderDebug", default = 0)
    }
    if (missing(directory)) {
        stop("must provide 'directory', in which glider files reside")
    }
    showProgressBar <- identical(progressBar, TRUE)
    gliderDebug(debug, "read.glider.seaexplorer.realtime(\"", directory, "\", ...) START\n", sep = "", unindent = 1)
    yoGiven <- !missing(yo)
    glifiles <- dir(directory, pattern = "*gli*", full.names = TRUE)
    pld1files <- dir(directory, pattern = "*.pld1.*", full.names = TRUE)
    if (length(glifiles) != length(pld1files)) {
        warning(
            "There is an unequal number of *gli* files (", length(glifiles),
            ") and *pld1* files (", length(pld1files), "), but they ought to be",
            " paired. This may indicate a problem in the data directory.",
            " Try calling this function with debug=2 to see filenames."
        )
    }

    gliderDebug(debug, "Originally, gli files: ", oce::vectorShow(glifiles))
    gliderDebug(debug, "Originally, pld1 files: ", oce::vectorShow(pld1files))

    # If 'yo' was not given, we use all possible values, based on the files
    # identified so far. This is done with gsub() calls in a step-by-step
    # process, for simplicity of recoding if the manufacturer changes the
    # filename pattern.
    if (!yoGiven) {
        yo <- gsub(".*/", "", pld1files) # now just filename
        gliderDebug(debug, "stage 1. ", oce::vectorShow(yo))
        # yo <- gsub("^.*.(raw|sub).([0-9]+)\\..*$", "\\2", yo) # now just yo number
        yo <- gsub(".gz$", "", yo)
        gliderDebug(debug, "stage 2. ", oce::vectorShow(yo))
        yo <- gsub("^.*\\.", "", yo) # now just yo number (at end of filename)
        gliderDebug(debug, "stage 3. ", oce::vectorShow(yo))
        yo <- as.numeric(yo)
        gliderDebug(debug, "stage 4. ", oce::vectorShow(yo))
    }
    # Narrow glifiles and pld1files, to just those that match the yo pattern
    # Identify files by pattern-matching, e.g. for yo=10, use files that end in
    # either '.10' or '.10.gz'. Do this first for gli files (glider files)
    # and then for pld1 files (payload files).
    keepglifiles <- NULL
    # message("next is glifiles");print(glifiles)
    # message("next is yo");print(yo)
    # cat("glifiles:\n");print(glifiles)
    gliderDebug(debug, "about to look for gli files for yos: ", oce::vectorShow(yo))
    for (y in yo) {
        # message("y=", y)
        # pattern <- paste0("^.*\\.", y, "\\.{0,1}(.gz){0,1}$")
        pattern <- paste0("^.*\\.", y, "\\..*$")
        # message("pattern ='", pattern, "'")
        found <- grep(pattern, glifiles)
        # message("found='", found, "'")
        if (length(found) == 1L) {
            keepglifiles <- c(keepglifiles, glifiles[found])
        }
    }
    if (!length(keepglifiles)) {
        stop("no gli file found for yo=", paste(yo, collapse = " "), sep = "")
    }
    glifiles <- keepglifiles
    keeppld1files <- NULL
    # cat("pld1files:\n");print(pld1files)
    for (y in yo) {
        # message("y=", y)
        pattern <- paste0("^.*\\.", y, "\\.{0,1}(.gz){0,1}$")
        # message("pattern=", pattern)
        found <- grep(pattern, pld1files)
        if (length(found) == 1L) {
            # message(" found")
            keeppld1files <- c(keeppld1files, pld1files[found])
        } else {
            # message(" not found")
        }
    }
    if (!length(keeppld1files)) {
        stop("no pld1 file found for yo=", paste(yo, collapse = " "))
    }
    pld1files <- keeppld1files

    if (debug > 1) {
        cat("After trimming to the yo subset, gli files:\n")
        print(glifiles)
        cat("\n")
        cat("After trimming to the yo subset, pld1 files:\n")
        print(pld1files)
        cat("\n")
    }
    # gli files
    nfiles <- length(glifiles)
    if (showProgressBar) {
        cat("* Reading", nfiles, ifelse(nfiles == 1, "gli file\n", "gli files...\n"))
        pb <- txtProgressBar(0, nfiles, 0, style = 3) # start at 0 to allow for a single yo
    }
    gli <- list()
    for (i in seq_len(nfiles)) {
        if (showProgressBar) {
            setTxtProgressBar(pb, i)
        }
        gliderDebug(debug, "reading gli file:  ", glifiles[i], "\n")
        gliData <- utils::read.delim(glifiles[i], sep = ";")
        gliData$yoNumberNav <- rep(yo[i], dim(gliData)[1])
        gli[[i]] <- gliData
    }
    gliData <- do.call(rbind.data.frame, gli)
    gliData$X <- NULL
    # pld1 files
    nfiles <- length(pld1files)
    if (showProgressBar) {
        cat("\n")
        flush.console()
        cat("* Reading", nfiles, ifelse(nfiles == 1, "pld1 file\n", "pld1 files...\n"))
        pb <- txtProgressBar(0, nfiles, 0, style = 3)
    }
    pld1 <- list()
    for (i in seq_len(nfiles)) {
        if (showProgressBar) {
            setTxtProgressBar(pb, i)
        }
        gliderDebug(debug, "reading pld1 file: ", pld1files[i], "?\n")
        pld1Data <- utils::read.delim(pld1files[i], sep = ";")
        pld1Data$yoNumber <- rep(yo[i], dim(pld1Data)[1])
        pld1[[i]] <- pld1Data
    }
    pld1Data <- do.call(rbind.data.frame, pld1)
    pld1Data$X <- NULL
    if (showProgressBar) {
        cat("\n")
    }
    # change missingValue to NA
    gliData[gliData == missingValue] <- NA
    pld1Data[pld1Data == missingValue] <- NA
    res <- new("glider")
    res@metadata$type <- "seaexplorer"
    res@metadata$subtype <- "realtime"
    # IOOS gives mapping=list(good = 1, not_evaluated = 2, suspect = 3, fail = 4, missing = 9)
    res <- initializeGliderFlagScheme(res, name = "IOOS")
    res@metadata$filename <- paste0(glifiles, ";", pld1files)
    ## 44 https://github.com/dankelley/oceglider/issues/44
    ## 44 res@metadata$yo <- yo
    res@metadata$dataNamesOriginal <- list(glider = list(), payload1 = list())
    for (name in names(gliData)) {
        res@metadata$dataNamesOriginal$glider[[name]] <- name
    }
    for (name in names(pld1Data)) {
        res@metadata$dataNamesOriginal$payload1[[name]] <- name
    }

    gliderDebug(debug, "about to rename items read from the 'gli' file\n")
    # Rename items in glider data.
    nameDict <- data.frame(
        oname = c(
            "NavState", "SecurityLevel", "Declination", "Heading",
            "Pitch", "Roll", "Declination", "Depth", "Temperature", "Pa",
            "DesiredH", "BallastCmd", "BallastPos", "LinCmd", "LinPos",
            "AngCmd", "AngPos", "Voltage", "Altitude"
        ),
        nname = c(
            "navState", "alarm", "declination", "heading", "pitch",
            "roll", "declination", "depth", "temperature", "pressureInternal",
            "desiredH", "ballastCmd", "ballastPos", "linCmd", "linPos",
            "angCmd", "angPos", "voltage", "altitude"
        )
    )
    for (row in seq_len(nrow(nameDict))) {
        nd <- nameDict[row, ]
        gliderDebug(debug, "  rename glider item", nd$oname, "as", nd$nname, "\n")
        # cat(oce::vectorShow(nd))
        namesTmp <- names(gliData)
        if (nd$oname %in% namesTmp) {
            gname <- getNextName(nd$nname, namesTmp)
            names(gliData) <- gsub(nd$oname, gname, namesTmp)
            res@metadata$dataNamesOriginal$glider[[gname]] <- nd$oname
        }
    }


    # FIXME: add more conversions here, and also to the corresponding
    # spot in the .raw() function. When both are added, adjust
    # ../man-roxygen/seaexplorer_names.R accordingly.
    if ("Timestamp" %in% names(gliData)) {
        # FIXME(DK): reading fractional seconds changes some hard-wired numbers in test_flags.R
        gliData$Timestamp <- as.POSIXct(gliData$Timestamp, format = "%d/%m/%Y %H:%M:%OS", tz = "UTC")
        names(gliData) <- gsub("Timestamp", "time", names(gliData))
        res@metadata$dataNamesOriginal$glider$time <- "Timestamp"
    }
    if ("Lat" %in% names(gliData)) {
        gliData$Lat <- degreeMinute(gliData$Lat)
        names(gliData) <- gsub("Lat", "latitude", names(gliData))
        res@metadata$dataNamesOriginal$glider$latitude <- "Lat"
    }
    if ("Lon" %in% names(gliData)) {
        gliData$Lon <- degreeMinute(gliData$Lon)
        names(gliData) <- gsub("Lon", "longitude", names(gliData))
        res@metadata$dataNamesOriginal$glider$longitude <- "Lon"
    }
    # Rename items in payload1 data.
    gliderDebug(debug, "about to rename items read from the 'pld1' file\n")

    # Salinity is not in pld1Data in my sample files; anyway, so we compute it.
    if (3 == sum(c("GPCTD_CONDUCTIVITY", "GPCTD_TEMPERATURE", "GPCTD_PRESSURE") %in% names(pld1Data))) {
        gliderDebug(debug, "computing salinity from GPCTD_CONDUCTIVITY etc\n")
        pld1Data$salinity <- swSCTp(
            pld1Data$GPCTD_CONDUCTIVITY / 4.2914,
            pld1Data$GPCTD_TEMPERATURE,
            pld1Data$GPCTD_PRESSURE
        )
        res@metadata$dataNamesOriginal$payload1$salinity <- "-"
    }
    # if (debug > 0) {
    #    cat("before legato\n")
    #    print(names(pld1Data))
    #    print(res@metadata$dataNamesOriginal$payload1)
    # }
    # name dictionary (oname = original name, nname = newname)
    # NOTE: use the same here and in seaexplorer_delayed.R
    nameDictDefault <- data.frame(
        oname = c(
            "AROD_FT_DO", "AROD_FT_TEMP",
            "NAV_RESOURCE", "NAV_LONGITUDE", "NAV_LATITUDE",
            "NAV_DEPTH", "FLBBCD_CHL_COUNT", "FLBBCD_CHL_SCALED",
            "FLBBCD_BB_700_COUNT", "FLBBCD_BB_700_SCALED",
            "FLBBCD_CDOM_COUNT", "FLBBCD_CDOM_SCALED", "GPCTD_CONDUCTIVITY",
            "GPCTD_DOF", "GPCTD_PRESSURE", "GPCTD_TEMPERATURE",
            "PLD_REALTIMECLOCK"
        ),
        nname = c(
            "oxygen", "oxygenTemperature",
            "navState", "longitude", "latitude", "pressureNav",
            "chlorophyllCount", "chlorophyll", "backscatterCount",
            "backscatter", "cdomCount", "cdom", "conductivity",
            "oxygenFrequency", "pressure", "temperature",
            "time"
        )
    )
    nameDictLegato <- data.frame(
        oname = paste0("LEGATO_", c(
            "CONDTEMP", "CONDUCTIVITY", "PRESSURE", "SALINITY",
            "TEMPERATURE"
        )),
        nname = c(
            "conductivityTemperature", "conductivity", "pressure", "salinity",
            "temperature"
        )
    )
    nameDict <- rbind(nameDictDefault, nameDictLegato)
    for (row in seq_len(nrow(nameDict))) {
        nd <- nameDict[row, ]
        gliderDebug(debug, "  rename payload item", nd$oname, "as", nd$nname, "\n")
        # cat(oce::vectorShow(nd))
        # Handle some conversions
        if (identical(nd$oname, "PLD_REALTIMECLOCK")) {
            pld1Data$PLD_REALTIMECLOCK <- as.POSIXct(pld1Data$PLD_REALTIMECLOCK,
                format = "%d/%m/%Y %H:%M:%OS", tz = "UTC"
            )
        }
        if (identical(nd$oname, "NAV_LATITUDE")) {
            pld1Data$NAV_LATITUDE <- degreeMinute(pld1Data$NAV_LATITUDE)
        }
        if (identical(nd$oname, "NAV_LONGITUDE")) {
            pld1Data$NAV_LONGITUDE <- degreeMinute(pld1Data$NAV_LONGITUDE)
        }
        namesTmp <- names(pld1Data)
        if (nd$oname %in% namesTmp) {
            gname <- getNextName(nd$nname, namesTmp)
            names(pld1Data) <- gsub(nd$oname, gname, namesTmp)
            res@metadata$dataNamesOriginal$payload1[[gname]] <- nd$oname
        }
    }
    # if (debug > 0) {
    #    cat("after legato\n")
    #    print(names(pld1Data))
    #    print(res@metadata$dataNamesOriginal$payload1)
    # }
    res@data <- list(glider = gliData, payload1 = pld1Data)
    # BOOKMARK START assure that this is echoed in read.glider.seaexplorer.realtime()
    # insert units
    for (stream in names(res@data)) {
        # FIXME: add more units here, if any of them are certain to be known
        res@metadata$units[[stream]] <- list()
        dataNames <- names(res@data[[stream]])
        if ("salinity" %in% dataNames) {
            res@metadata$units[[stream]]$salinity <- list(unit = expression(), scale = "PSS-78")
        } # FIXME: is this modern?
        if ("temperature" %in% dataNames) {
            res@metadata$units[[stream]]$temperature <- list(unit = expression(degree * C), scale = "ITS-90")
        }
        if ("pressure" %in% dataNames) {
            res@metadata$units[[stream]]$pressure <- list(unit = expression(dbar), scale = "")
        }
        if ("longitude" %in% dataNames) {
            res@metadata$units[[stream]]$longitude <- list(unit = expression(degree * E), scale = "")
        }
        if ("latitude" %in% dataNames) {
            res@metadata$units[[stream]]$latitude <- list(unit = expression(degree * N), scale = "")
        }
        if ("heading" %in% dataNames) {
            res@metadata$units[[stream]]$heading <- list(unit = expression(degree), scale = "")
        }
        if ("pitch" %in% dataNames) {
            res@metadata$units[[stream]]$pitch <- list(unit = expression(degree), scale = "")
        }
        if ("roll" %in% dataNames) {
            res@metadata$units[[stream]]$roll <- list(unit = expression(degree), scale = "")
        }
        # set up flags to value 2, which means not-checked
        len <- length(res@data[[stream]][[1]]) # all have same length
        for (name in dataNames) {
            res@metadata$flags[[stream]][[name]] <- rep(2, len)
        }
    }
    # BOOKMARK END
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste("read.glider.seaexplorer.realtime(directory=\"", directory, "\",",
            "yo=c(", paste(yo, collapse = ","), "),",
            "missingValue=", missingValue, ")",
            sep = ""
        )
    )
    gliderDebug(debug, "# END read.glider.seaexplorer.realtime()\n", unindent = 1)
    res
}
