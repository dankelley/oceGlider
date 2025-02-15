issue40 <- TRUE # read fractional seconds? (https://github.com/dankelley/oceglider/issues/40)

#' Read Delayed-Mode SeaExplorer Glider Data
#'
#' Reads raw CSV files produced by a SeaExplorer glider. This function can
#' output either "Level 0" or "Level 1" type data. Level 0 is simply the raw
#' data as written in the CSV files with no processing done. (Historical note:
#' until package varsion 0.1-14, released on 2025-02-10, longitude and latitude
#' were interpolated between surface values for level =
#' 0. This behaviour was changed for issue 127, at
#'    https://github.com/dankelley/oceglider/issues/127).
#'
#' Level 1 processing performs a number of steps to give an
#' "analysis ready" dataset, including
#'
#' \enumerate{
#'
#' \item Interpolation of the surface longitude and latitude to give
#' an estimate of the subsurface positions. This is a crude estimate of
#' subsurface location and should be taken only as a first guess.
#'
#' \item Removal of the first few sensor values from when the glider
#' is in `navState=118` (inflecting up) or `navState=110`
#' (inflecting down). The reason for this is that when the glider is
#' set to sample on alternating profiles, when the CTD is powered up
#' the first sample output to the payload computer is the *last*
#' sample recorded before power down.
#'
#' \item Interpolation, depending on the value of `interpolateToCTD`. If
#' `interpolateToCTD` is `TRUE`, then any "extra" sensors are interpolated
#' to the times for which there is CTD data. Otherwise, NAs for *all*
#' the sensors are interpolated to a common time, corresponding to the
#' raw time stamps output from the various sensors. A caution -- this
#' will produce an apparent "upsampling" of each sensor, so that the
#' apparent sample rate is higher. For example, if a Wetlabs FLBBCD
#' sensor sampled, but there is no corresponding GP-CTD sample from
#' the same time, the CTD parameters will be interpolated from the
#' ones before and after. This has the disadvantage of interpolating
#' values that were not measured, but has the advantage of assigning
#' pressures to values measured by sensors not integrated into the CTD
#' (e.g. Wetlabs FLBBCD, Rinko O2). Following the interpolation, any
#' rows with duplicated times are removed.
#'
#' \item Calculate Practical salinity from conductivity, temperature
#' and pressure using [oce::swSCTp()].
#'
#' }
#'
#' In any case, a flag scheme is set up according to the IOOS classification system (see
#' Table 2 of reference 1), as follows.
#'
#' \tabular{llll}{
#' \strong{Name}   \tab \strong{Value} \tab \strong{IOOS Name}          \tab \strong{Description}\cr
#' `pass`          \tab 1              \tab Pass                        \tab Data has passed quality control (QC) tests\cr
#' `not_evaluated` \tab 2              \tab Not Evaluated               \tab Data has not been QC tested\cr
#' `suspect`       \tab 3              \tab Suspect or of High Interest \tab Data is considered to be of suspect or high interest\cr
#' `fail`          \tab 4              \tab Fail                        \tab Data is considered to have failed on one or more QC tests\cr
#' `missing`       \tab 9              \tab Missing Data                \tab Data are missing; using a placeholder\cr
#' }
#'
#' @param directory The directory in which the delayed-mode SeaExplorer files are located.
#'
#' @param yo A numeric value (or vector) specifying the yo numbers to read. If
#' this is not provided, [read.glider.seaexplorer.raw()] will read all yo
#' numbers for which files are present in `dir`.
#'
#' @param pattern a character value used to find files to read. The
#' default, `"pld1"`, will match files with that string in the name. The
#' best choices for this depends on the setup of files. The search
#' for files is done with `list.files(directory, pattern)`, and so
#' it can be a good idea to try that first, to learn how to specify
#' the desired files.  Also, try calling with `debug=1` to get some
#' indication of the files that get read or with `debug=2` to get
#' a full listing.
#'
#' @param level A numeric value specifying the processing level, 0 or
#' 1. See Details.
#'
#' @param interpolateToCTD A logical indicating whether all sensors should be
#' interpolated to the CTD times to obtain a common time base, or whether all
#' sensors should simply be interpolated for all time stamps (which was the
#' default behaviour before 2019-12-08)
#'
## @param removeTimeSincePowerOn numeric value indicating the number of
## seconds of data to trim, after the glider sensors are powered on.
## One way to determine this is to read the whole sequence,
## and then plot say the first 10 minutes of salinity and temperature
## data, looking for a transition between aphysical values, which
## might take the form of zero salinity, followed by a relatively
## rapid ramp-up to values that seem more oceanographic.
#'
#' @param progressBar a logical value that controls whether to indicate the
#' progress made in reading and interpreting the data.  This can be useful,
#' since the work can be slow. The default is to show progress in
#' interactive sessions, but not in scripts.
#'
#' @param missingValue numeric value that indicates bad data. Any data items
#' equalling this value are converted to NA. The default is 9999. To avoid
#' changing values to NA, call the function with `missingValue=NULL`.
#'
## @param rename optional logical value indicating whether to rename variables
## from the values in the file to the oce convention, using [cnvName2oceName()]
## for the translation. This is done by default, but setting `rename=FALSE` can
## be helpful if there is a wish to control the renaming, either using a
## built-in dictionary or using a dictionary set up by the user.
#'
#' @template debug
#'
#' @template seaexplorer_names
#'
#' @examples
#' library(oceglider)
#' directory <- system.file("extdata/sea_explorer/delayed_raw", package = "oceglider")
#' g <- read.glider.seaexplorer.raw(directory, progressBar = FALSE)
#' plot(g, which = "p")
#'
#' @family functions for seaexplorer gliders
#' @family functions to read glider data
#'
#' @importFrom methods new
#' @importFrom oce swSCTp processingLogAppend
#' @importFrom stats approx median
#' @importFrom utils read.delim flush.console head setTxtProgressBar tail txtProgressBar
#'
#' @section History of changes to this function:
#'
#' * In version 0.1.16, the `removeTimeSincePowerOn` parameter was removed. The
#' new scheme is for the user to call `deleteStartupData()` on the return
#' value, to accomplish a similar thing.
#'
#' @author Clark Richards, Chantelle Layton and Dan Kelley
#'
#' @md
#'
#' @export
read.glider.seaexplorer.raw <- function(directory, pattern = "pld1.raw",
                                        yo, level = 1, interpolateToCTD = TRUE,
                                        #removeTimeSincePowerOn = 0,
                                        progressBar = interactive(),
                                        missingValue = 9999,
                                        # rename = TRUE,
                                        debug = getOption("gliderDebug", default = 0)) {
    if (missing(directory)) {
        stop("must provide 'directory', in which glider files reside")
    }
    gliderDebug(debug, "read.glider.seaexplorer.raw(\"", directory, "\", ...) START\n", sep = "", unindent = 1)
    if (level != 0 && level != 1) {
        stop("Level must be either 0 or 1")
    }
    # navfiles <- dir(directory, pattern = "*gli*", full.names = TRUE) # FIXME: not used
    #>pattern <- paste0("*.", type, ".raw.*")
    filenames <- list.files(directory, pattern = pattern, full.names = TRUE)
    # pld2files <- list.files(directory, pattern = "*.pld2.raw.*", full.names = TRUE)
    nfiles <- length(filenames)
    if (0L == nfiles) {
        stop("no files found in directory \"", directory, "\" that match the pattern \"", pattern, "\")")
    }
    # Note the removal of .gz at the end of filenames. This is so we can find yo numbers
    # as the last period-separated item in the filename.
    yoNumbers <- gsub(".gz$", "", filenames) |>
        strsplit(".", fixed = TRUE) |>
        lapply(tail, 1) |>
        unlist() |>
        as.integer()
    # Put the files into yo order. We must do this because the filenames
    # recovered by list.files() will not be in yo order (e.g. 100 will come
    # before 99), owing to insufficient use of leading zeros in the filenames.
    o <- order(yoNumbers)
    yoNumbers <- yoNumbers[o]
    filenames <- filenames[o]
    if (debug > 1) {
        print(data.frame(file = filenames, yoNumber = yoNumbers))
    } else if (debug == 1) {
        print(data.frame(file = filenames, yoNumber = yoNumbers) |> head())
    }
    if (missing(yo)) {
        yo <- yoNumbers
    }
    #cat(oce::vectorShow(filenames))
    #cat(oce::vectorShow(yo))
    #cat(oce::vectorShow(yoNumbers))
    y <- yoNumbers %in% yo
    #cat(oce::vectorShow(y))
    files <- filenames[y]
    #cat(oce::vectorShow(files))
    if (length(files) == 0) {
        stop("no files in directory '", directory, "'", sep = "")
    }
    res <- new("glider")
    res@metadata$type <- "seaexplorer"
    res@metadata$subtype <- "delayed" # FIXME: this is fixed to delayed-mode ... maybe we don't want that
    res@metadata$directory <- directory
    res@metadata$pattern <- pattern
    # IOOS gives mapping=list(good = 1, not_evaluated = 2, suspect = 3, fail = 4, missing = 9)
    res <- initializeGliderFlagScheme(res, name = "IOOS")
    # res@metadata$level <- level
    res@metadata$filename <- directory
    ## 44 https://github.com/dankelley/oceglider/issues/44
    ## 44 res@metadata$yo <- yo
    res@metadata$dataNamesOriginal <- list()

    showProgressBar <- identical(progressBar, TRUE)
    if (showProgressBar) {
        cat("* Reading", length(files), "files...\n")
        pb <- txtProgressBar(0, length(files), 0, style = 3) # start at 0 to allow for a single yo
    }
    ds <- list() # stores one entry per file. FIXME: aren't we stringing them together later?
    for (i in seq_along(files)) {
        if (showProgressBar) {
            setTxtProgressBar(pb, i)
        }
        gliderDebug(debug, "i=", i, ", file=\"", files[i], "\"\n", sep = "")
        d <- utils::read.delim(files[i], sep = ";", stringsAsFactors = FALSE, row.names = NULL)
        d$yoNumber <- rep(yo[i], dim(d)[1])
        # Rename items in payload1 data.
        gliderDebug(debug > 3, "i=", i, "  (position 1) \n")
        # if ("NAV_RESOURCE" %in% names(d)) {
        #    names(d) <- gsub("NAV_RESOURCE", "navState", names(d))
        #    res@metadata$dataNamesOriginal$payload1$navState <- "NAV_RESOURCE"
        # }
        # if ("NAV_DEPTH" %in% names(d)) {
        #    names(d) <- gsub("NAV_DEPTH", "pressureNav", names(d))
        #    res@metadata$dataNamesOriginal$payload1$pressureNav <- "NAV_DEPTH"
        # }
        # if ("NAV_LONGITUDE" %in% names(d)) {
        #    names(d) <- gsub("NAV_LONGITUDE", "longitude", names(d))
        #    d$longitude <- degreeMinute(d$longitude)
        #    res@metadata$dataNamesOriginal$payload1$longitude <- "NAV_LONGITUDE"
        # }
        # if ("NAV_LATITUDE" %in% names(d)) {
        #    names(d) <- gsub("NAV_LATITUDE", "latitude", names(d))
        #    d$latitude <- degreeMinute(d$latitude)
        #    res@metadata$dataNamesOriginal$payload1$latitude <- "NAV_LATITUDE"
        # }
        # FIXME: use a dictionary here
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
                "CONDTEMP", "CONDUCTIVITY",
                "PRESSURE", "SALINITY", "TEMPERATURE"
            )),
            nname = c(
                "conductivityTemperature", "conductivity",
                "pressure", "salinity", "temperature"
            )
        )
        nameDict <- rbind(nameDictDefault, nameDictLegato)
        for (row in seq_len(nrow(nameDict))) {
            oname <- nameDict$oname[row]
            nname <- nameDict$nname[row]
            gliderDebug(debug > 1, "    rename payload item", oname, "as", nname, "\n")
            # cat(oce::vectorShow(nd))
            # Handle some conversions
            if (identical(oname, "PLD_REALTIMECLOCK")) {
                d$PLD_REALTIMECLOCK <- as.POSIXct(d$PLD_REALTIMECLOCK,
                    format = "%d/%m/%Y %H:%M:%OS", tz = "UTC"
                )
            }
            if (identical(oname, "NAV_LATITUDE")) {
                d$NAV_LATITUDE <- degreeMinute(d$NAV_LATITUDE)
            }
            if (identical(oname, "NAV_LONGITUDE")) {
                d$NAV_LONGITUDE <- degreeMinute(d$NAV_LONGITUDE)
            }
            namesTmp <- names(d)
            # cat("nd:\n");print(nd)
            # cat("namesTmp:\n");print(namesTmp)
            if (oname %in% namesTmp) {
                gname <- getNextName(nname, namesTmp)
                names(d) <- gsub(oname, gname, namesTmp)
                res@metadata$dataNamesOriginal[[gname]] <- oname
            }
        }
        gliderDebug(debug > 1, "  finished renaming\n")
        # if ("GPCTD_TEMPERATURE" %in% names(d)) {
        #    names(d) <- gsub("GPCTD_TEMPERATURE", "temperature", names(d))
        #    res@metadata$dataNamesOriginal$payload1$temperature <- "GPCTD_TEMPERATURE"
        # }
        # if ("GPCTD_PRESSURE" %in% names(d)) {
        #    names(d) <- gsub("GPCTD_PRESSURE", "pressure", names(d))
        #    res@metadata$dataNamesOriginal$payload1$pressure <- "GPCTD_PRESSURE"
        # }
        # if ("GPCTD_CONDUCTIVITY" %in% names(d)) {
        #    names(d) <- gsub("GPCTD_CONDUCTIVITY", "conductivity", names(d))
        #    res@metadata$dataNamesOriginal$payload1$conductivity <- "GPCTD_CONDUCTIVITY"
        # }
        # if ("GPCTD_DOF" %in% names(d)) {
        #    names(d) <- gsub("GPCTD_DOF", "oxygenFrequency", names(d))
        #    res@metadata$dataNamesOriginal$payload1$oxygenFrequency <- "GPCTD_DOF"
        # }
        # gliderDebug(debug > 3, "i=", i, " (position 3) \n")
        # if ("FLBBCD_CHL_COUNT" %in% names(d)) {
        #    names(d) <- gsub("FLBBCD_CHL_COUNT", "chlorophyllCount", names(d))
        #    res@metadata$dataNamesOriginal$payload1$chlorophyllCount <- "FLBBCD_CHL_COUNT"
        # }
        # if ("FLBBCD_CHL_SCALED" %in% names(d)) {
        #    names(d) <- gsub("FLBBCD_CHL_SCALED", "chlorophyll", names(d))
        #    res@metadata$dataNamesOriginal$payload1$chlorophyll <- "FLBBCD_CHL_SCALED"
        # }
        # if ("FLBBCD_BB_700_COUNT" %in% names(d)) {
        #    names(d) <- gsub("FLBBCD_BB_700_COUNT", "backscatterCount", names(d))
        #    res@metadata$dataNamesOriginal$payload1$backscatterCount <- "FLBBCD_BB_700_COUNT"
        # }
        # if ("FLBBCD_BB_700_SCALED" %in% names(d)) {
        #    names(d) <- gsub("FLBBCD_BB_700_SCALED", "backscatter", names(d))
        #    res@metadata$dataNamesOriginal$payload1$backscatter <- "FLBBCD_BB_700_SCALED"
        # }
        # if ("FLBBCD_CDOM_COUNT" %in% names(d)) {
        #    names(d) <- gsub("FLBBCD_CDOM_COUNT", "cdomCount", names(d))
        #    res@metadata$dataNamesOriginal$payload1$cdomCount <- "FLBBCD_CDOM_COUNT"
        # }
        # gliderDebug(debug > 3, "i=", i, " (position 4) \n")
        # if ("FLBBCD_CDOM_SCALED" %in% names(d)) {
        #    names(d) <- gsub("FLBBCD_CDOM_SCALED", "cdom", names(d))
        #    res@metadata$dataNamesOriginal$payload1$cdom <- "FLBBCD_CDOM_SCALED"
        # }
        # if ("PLD_REALTIMECLOCK" %in% names(d)) {
        #    names(d) <- gsub("PLD_REALTIMECLOCK", "time", names(d))
        #    # FIXME(DK): reading fractional seconds changes some hard-wired numbers in test_flags.R
        #    if (issue40) {
        #        d$time <- as.POSIXct(d$time, format = "%d/%m/%Y %H:%M:%OS", tz = "UTC")
        #    } else {
        #        d$time <- as.POSIXct(d$time, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
        #    }
        #    res@metadata$dataNamesOriginal$payload1$time <- "-"
        # }
        gliderDebug(debug > 3, "i=", i, " (position 5) \n")
        ds[[i]] <- d
        gliderDebug(debug, "  first stage completed for file", i, "of", nfiles, "\n")
    }
    gliderDebug(debug > 3, "  (position 7) \n")
    dall <- do.call(rbind.data.frame, ds)
    gliderDebug(debug > 3, "  (position 8) \n")
    dall[["X"]] <- NULL # get rid of the weird last column
    gliderDebug(debug > 3, "  (position 9) \n")
    if (showProgressBar) {
        cat("\n")
        flush.console()
    }
    gliderDebug(debug, "  data cleanup\n")
    # First remove all duplicated lon/lat
    dall$longitude[which(duplicated(dall$longitude))] <- NA
    dall$latitude[which(duplicated(dall$latitude))] <- NA
    # Change behaviour for level=0, according to issue
    # https://github.com/dankelley/oceglider/issues/127
    if (level > 0) {
        trans <- dall$navState == 116
        dall$longitude[!trans] <- NA
        dall$latitude[!trans] <- NA
        dall$longitude <- approx(dall$time, dall$longitude, dall$time)$y
        dall$latitude <- approx(dall$time, dall$latitude, dall$time)$y
    }
    # Trim out any empty rows (no data at all)
    sub <- dall[, which(!(names(dall) %in% c("time", "navState", "longitude", "latitude", "pressureNav", "yoNumber")))]
    naRows <- apply(sub, 1, function(x) sum(is.na(x)))
    ok <- naRows < dim(sub)[2]
    dall <- dall[ok, ]
    if (!is.null(missingValue)) {
        dall[dall == missingValue] <- NA
    }
    if (level == 0) {
        res@data <- dall
        res@processingLog <- processingLogAppend(
            res@processingLog,
            paste("read.glider.seaexplorer.raw(directory=", directory, ", yo=", head(yo, 1), ":", tail(yo, 1), ", level=", level, ")", sep = "")
        )
    } else if (level == 1) {
        #if (removeTimeSincePowerOn > 0) {
        #    starts <- c(1, which(diff(dall$time) > 60) + 1) # FIXME: should 60s be an argument?
        #    dt <- median(diff(as.numeric(dall$time)))
        #    ok <- rep(TRUE, length(dall$time))
        #    n <- round(removeTimeSincePowerOn / dt)
        #    gliderDebug(debug, sprintf("  for trimming after power on: dt=%.3fs n=%d\n", dt, n))
        #    for (s in starts) ok[s:(s + n)] <- FALSE
        #    dall <- dall[ok, ]
        #}
        # Interpolate NAs
        ctd <- which(!is.na(dall$temperature)) # indices of measure CTD points
        n <- length(names(dall)) - length(c("time", "navState", "longitude", "latitude", "pressureNav", "yoNumber"))
        if (showProgressBar) {
            cat("* Interpolating NAs...\n")
            pb <- txtProgressBar(1, n, 1, style = 3)
        }
        i <- 1
        for (var in names(dall)) {
            if (!(var %in% c("time", "navState", "longitude", "latitude", "pressureNav", "yoNumber"))) {
                if (showProgressBar) {
                    setTxtProgressBar(pb, i)
                }
                if (!all(is.na(dall[[var]]))) { # in case the entire field is missing, e.g. oxygenFrequency
                    dall[[var]] <- approx(dall[["time"]], dall[[var]], dall[["time"]])$y
                }
                i <- i + 1
            }
        }
        # We want to interpolate non-CTD fields to the time stamps
        # for which there is actual measured CTD data. Since we've
        # already interpolated to all existing time stamps, we can
        # just remove the ones that were *not* from the CTD
        if (interpolateToCTD) {
            dall <- dall[ctd, ]
        }
        if (showProgressBar) {
            cat("\n")
            flush.console()
        }
        # Remove duplicated times
        dall <- dall[!duplicated(dall), ]
        # Calculate salinity
        dall$salinity <- with(dall, swSCTp(conductivity, temperature, pressure, conductivityUnit = "S/m"))
        dall$salinity[dall$salinity > 40] <- NA
        res@data <- dall
    }
    # Insert units
    res@metadata$units <- list()
    dataNames <- names(res@data)
    if ("salinity" %in% dataNames) {
        res@metadata$units$salinity <- list(unit = expression(), scale = "PSS-78")
    } # FIXME: is this modern?
    if ("temperature" %in% dataNames) {
        res@metadata$units$temperature <- list(unit = expression(degree * C), scale = "ITS-90")
    }
    if ("pressure" %in% dataNames) {
        res@metadata$units$pressure <- list(unit = expression(dbar), scale = "")
    }
    if ("longitude" %in% dataNames) {
        res@metadata$units$longitude <- list(unit = expression(degree * E), scale = "")
    }
    if ("latitude" %in% dataNames) {
        res@metadata$units$latitude <- list(unit = expression(degree * N), scale = "")
    }
    if ("heading" %in% dataNames) {
        res@metadata$units$heading <- list(unit = expression(degree), scale = "")
    }
    if ("pitch" %in% dataNames) {
        res@metadata$units$pitch <- list(unit = expression(degree), scale = "")
    }
    if ("roll" %in% dataNames) {
        res@metadata$units$roll <- list(unit = expression(degree), scale = "")
    }
    # set up flags to value 2, which means not-checked
    len <- length(res@data[[1]]) # all have same length
    for (name in dataNames) {
        res@metadata$flags[[name]] <- rep(2, len)
    }
    res@metadata$dataAreStreamed <- FALSE
    # BOOKMARK END
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste0(
            "read.glider.seaexplorer.raw(directory=\"", directory, "\", pattern=\"", pattern, "\"\", yo=", head(yo, 1),
            ":", tail(yo, 1), ", level=", level, ")"
        )
    )
    gliderDebug(debug, "read.glider.seaexplorer.raw() END")
    res
}
