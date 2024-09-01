#' oceglider: A Package for Processing Ocean Glider Data
#'
#' This package was written with two particular dataset types
#' in mind, from SeaExplorer and Slocum devices. There is a good
#' chance that the functions provided here (a) will fail on
#' other types and (b) function names and arguments will change
#' as more datasets are examined by the author.
#'
#' @importFrom methods new
#' @importFrom oce subset summary
#'
#' @name oceglider
#' @docType package
#' @keywords internal
"_PACKAGE"
NULL

#' A class to hold glider information
#'
#' As of 2024-06-23, it is not clear whether the best policy is for
#' this to inherit from 'oce' or to define itself here.  Doing the
#' former might seem sensible, since it might reduce the need for code
#' here, but it seems to cause problems (e.g. inability to build
#' vignettes in the standard way) that I have trouble understanding.
#' We ran into similar problems in the argoFloats package too, and
#' there we decided to define the object fully in the new package. For
#' now, that's the approach used here also.
#'
#' @author Dan Kelley
#'
#' @export
glider <- setClass("glider", slots = c(metadata = "list", data = "list", processingLog = "list"))
# glider <- setClass("glider", contains = "oce")

setMethod(
    f = "initialize",
    signature = "glider",
    definition = function(.Object, filename) {
        if (!missing(filename)) {
            .Object@metadata$filename <- filename
        }
        .Object@metadata$type <- "?"
        .Object@metadata$subtype <- "?"
        .Object@metadata$level <- NA # unknown at start
        .Object@processingLog$time <- as.POSIXct(Sys.time())
        .Object@processingLog$value <- "create 'glider' object"
        return(.Object)
    }
)

# _ #' Saturation of O2 in sea water
# _ #'
# _ #' Computes the solubility (saturation) of Oxygen in sea water. Based on the
# _ #' Matlab function `SW_SATO2` from the CSIRO seawater toolbox.
# _ #'
# _ #' @author Chantelle Layton
# _ #' @param temperature temperature
# _ #' @param salinity salinity
# _ #'
# _ #' DEK note: the result is in mL/L.
# _ swSatO2 <- function(temperature, salinity) {
# _     Tk <- 273.15 + temperature * 1.00024
# _     # constants for Eqn (4) of Weiss 1970
# _     a1 <- -173.4292
# _     a2 <- 249.6339
# _     a3 <- 143.3483
# _     a4 <- -21.8492
# _     b1 <- -0.033096
# _     b2 <- 0.014259
# _     b3 <- -0.0017000
# _     lnC <- a1 +
# _         a2 * (100 / Tk) +
# _         a3 * log(Tk / 100) +
# _         a4 * (Tk / 100) +
# _         salinity * (b1 + b2 * (Tk / 100) + b3 * ((Tk / 100)^2))
# _     exp(lnC)
# _ }

#' Retrieve Part of a glider Object
#'
#' Retrieve something contained in a glider object, or something that can
#' be computed from what is contained there.
#'
#' First, a check is done to see if the object's metadata contains an item
#' with name given by `i`. If this is true, then that value is returned.
#'
#' Otherwise, the item is sought somewhere within the `data` slot.
#' This may be done within `data` directly for Slocum
#' data or within `data$payload` or a similar name for SeaExplorer
#' data.  Note that `[[` can retrieve not just the actual stored
#' information, but also information that can be derived from that information.
#' For example, gliders do not store Absolute Salinity or Conservative
#' Temperature, but they can be retrieved with `g[["SA"]]` and `g[["CT"]]`
#' respectively, where `g` is a glider object (see Example 1).  Use e.g. `g[["?"]]` on
#' a given glider object, to find out what items `[[` can retrieve
#' (see Example 2).
#'
## NOTE: this paragraph may no longer apply (note added 2023-08-24 by Dan
## Kelley). For SeaExplorer objects, if is not specified, and then `i` is sought first
## in `payload1`, with `glider` being checked thereafter. For example, this means that a
## thermometer within the payload will be preferred to one attached to
## the body of the glider. This selection
## process can be controlled by setting `j` to either `"glider"`
## or `"payload1"`.  For example, both `x[["temperature"]]` and
## `x[["temperature","payload1"]]` retrieve values from
## the payload thermistor, while `x[["temperature","glider"]]` retrieves
## values from the glider thermistor. For clarity of code, it might make
## sense to always specify `j`.
##
## In addition to retrieving data stored in the object, `[[` can also
## return the following.
##
## \itemize{
##
## \item the full `data` slot, with e.g. `x[["data"]]`
##
## \item the `glider` item in `data` slot, with e.g. `x[["glider"]]`
##
## \item the `payload1` item in `data` slot, with e.g. `x[["payload1"]]`
##
## \item the Absolute Salinity is returned with e.g.
## `x[["SA"]]`. This is computed with
## [gsw::gsw_SA_from_SP()], based on the water properties
## stored in the object. (See also the item for Conservative Temperature)
##
## \item the sigma-theta density anomaly calculated using
## [oce::swSigmaTheta()] on the water properties stored in the object,
## with e.g. `x[["sigmaTheta"]]`. This obeys the setting of the
## equation of state, established with e.g. `options(oceEOS="gsw")` for the
## TEOS-10/GSW variant or `options(oceEOS="unesco")` for the
## older UNESCO variant.
##
## \item the Conservative Temperature is returned with e.g.
## `x[["CT"]]`. This is computed with
## [gsw::gsw_CT_from_t()], based on the water properties
## stored in the object. (See also the item for Absolute Salinity.)
##
## \item the sigma0 density anomaly is returned with e.g.
## `x[["sigma0"]]`. This is computed with
## [oce::swSigma0()]  based on the water properties
## stored in the object.
## Note that the computation depends on the setting of the equation of state,
## set up with `options(oceEOS="gsw")` for the TEOS-10/GSW variant
## or with `options(oceEOS="unesco")` for the older UNESCO variant.
##
## \item the spiciness0 water property is returned with e.g.
## `x[["spiciness0"]]`. This is computed with
## [gsw::gsw_spiciness0()], based on the water properties
## stored in the object. (Note that this is the TEOS-10/GSW variant.)
##
## \item glider object containing just the data for a particular yo,
## e.g. `x[["yo",1]]` yields the first yo.
##
## }
#'
#' @param x a glider object, i.e. one inheriting from [glider-class].
#'
#' @param i a character value that names the item to be retrieved.
#'
#' @param j an optional character value specifying the data-stream to be used.
#'
#' @param ... optional additional information (ignored).
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oceglider)
#' directory <- system.file("extdata/seaexplorer/raw", package = "oceglider")
#' g <- read.glider.seaexplorer.delayed(directory, progressBar = FALSE)
#'
#' # Example 1: look up Absolute Salinity
#' summary(g[["SA"]])
#'
#' # Example 2: discover what can be retrieved from this object
#' # (FIXME: not all of these work)
#' g[["?"]]
#'
#' @importFrom oce swSigmaTheta swSigma0 swSigma1 swSigma2 swSigma3 swSpice swZ
#' @importFrom gsw gsw_CT_from_t gsw_SA_from_SP gsw_sound_speed gsw_spiciness0
#'
#' @export
#'
#' @md
setMethod(
    f = "[[",
    signature(x = "glider", i = "ANY", j = "ANY"),
    definition = function(x, i, j, ...) {
        dots <- list(...)
        debug <- if (is.null(dots$debug)) getOption("gliderDebug", default = 0L) else dots$debug
        # . message("in [[, i='", i, "'")
        # debug <- getOption("gliderDebug", default = 0L)
        if (missing(i)) {
            stop("Must name a glider item to retrieve, e.g. '[[\"temperature\"]]'", call. = FALSE)
        }
        if (length(i) > 1L) {
            stop("can only access one item at a time", call. = FALSE)
        }
        if (!is.character(i)) {
            stop("glider item must be specified by name", call. = FALSE)
        }
        gliderDebug(debug, "glider[[i=\"", i, "\", j=\"",
            if (missing(j)) "(missing)" else j, ", ...]]\n",
            sep = "", unindent = 1
        )
        if (i == "filename") {
            return(x@metadata$filename)
        } else if (i == "data") {
            return(x@data)
        } else if (i == "metadata") {
            return(x@metadata)
        } else if (i == "yo" && !missing(j)) { # NOTE: not 'yoNumber'
            lines <- which(x@data$payload1$yoNumber == j)
            x@data$payload1 <- x@data$payload1[lines, ]
            for (f in names(x@metadata$flags$payload1)) {
                x@metadata$flags$payload1[[f]] <- x@metadata$flags$payload1[[f]][lines]
            }
            return(x)
        }
        type <- x@metadata$type
        if (is.null(type)) {
            stop("'type' is NULL")
        }
        if (length(grep("Flag$", i))) {
            # returns a list
            where <- "payload1"
            return(if ("flags" %in% names(x@metadata)) x@metadata$flags[[where]][[gsub("Flag$", "", i)]] else NULL)
        }
        # FIXME (DK) recognize "Unit$" as done in oce.
        if (i == "type") {
            return(type)
        }
        if (i == "sigmaTheta") {
            return(swSigmaTheta(
                salinity = x[["salinity"]],
                temperature = x[["temperature"]],
                pressure = x[["pressure"]],
                longitude = x[["longitude"]],
                latitude = x[["latitude"]]
            ))
        }
        if (i == "sigma0") {
            return(swSigma0(
                salinity = x[["salinity"]],
                temperature = x[["temperature"]],
                pressure = x[["pressure"]],
                longitude = x[["longitude"]],
                latitude = x[["latitude"]]
            ))
        }
        if (i == "sigma1") {
            return(swSigma1(
                salinity = x[["salinity"]],
                temperature = x[["temperature"]],
                pressure = x[["pressure"]],
                longitude = x[["longitude"]],
                latitude = x[["latitude"]]
            ))
        }
        if (i == "sigma2") {
            return(swSigma2(
                salinity = x[["salinity"]],
                temperature = x[["temperature"]],
                pressure = x[["pressure"]],
                longitude = x[["longitude"]],
                latitude = x[["latitude"]]
            ))
        }
        if (i == "sigma3") {
            return(swSigma3(
                salinity = x[["salinity"]],
                temperature = x[["temperature"]],
                pressure = x[["pressure"]],
                longitude = x[["longitude"]],
                latitude = x[["latitude"]]
            ))
        }
        if (i == "SA") {
            return(gsw_SA_from_SP(
                SP = x[["salinity"]], p = x[["pressure"]],
                longitude = x[["longitude"]], latitude = x[["latitude"]]
            ))
        }
        if (i == "z") {
            return(swZ(x[["pressure"]], latitude = mean(x[["latitude"]], na.rm = TRUE)))
        }
        if (i == "CT") {
            t <- x[["temperature"]]
            SP <- x[["salinity"]] # stored as practical salinity
            p <- x[["pressure"]]
            SA <- gsw_SA_from_SP(SP = SP, p = p, longitude = x[["longitude"]], latitude = x[["latitude"]])
            return(gsw_CT_from_t(SA, t, p))
        }
        if (i == "sound speed") {
            t <- x[["temperature"]]
            SP <- x[["salinity"]] # stored as practical salinity
            p <- x[["pressure"]]
            SA <- gsw_SA_from_SP(SP = SP, p = p, longitude = x[["longitude"]], latitude = x[["latitude"]])
            CT <- gsw_CT_from_t(SA, t, p)
            return(gsw_sound_speed(SA, CT, p))
        }
        if (i == "spiciness0") {
            t <- x[["temperature"]]
            SP <- x[["salinity"]] # stored as practical salinity
            p <- x[["pressure"]]
            # SA <- gsw::gsw_SA_from_SP(SP, p, x[["longitude"]], x[["latitude"]])
            # CT <- gsw::gsw_CT_from_t(SA, t, p)
            # return(gsw::gsw_spiciness0(SA=SA, CT=CT))
            SA <- gsw_SA_from_SP(SP, p, x[["longitude"]], x[["latitude"]])
            CT <- gsw_CT_from_t(SA, t, p)
            return(gsw_spiciness0(SA = SA, CT = CT))
        }
        if (i == "oxygen") {
            if (identical(x@metadata$type, "seaexplorer")) {
                dataNames <- names(x@data)
                # get payload name
                w <- grep("payload", dataNames)
                if (length(w) == 0L) {
                    stop("sea-explorer object has no payload")
                }
                if (length(w) > 1L) {
                    stop("sea-explorer object has more than one \"payload\"-type element")
                }
                payloadName <- dataNames[w]
                dataNames <- names(x@data[[payloadName]])
                # message("dataNames--")
                # print(dataNames)
                if ("oxygen" %in% dataNames) {
                    gliderDebug(debug, "  accessing \"oxygen\" directly in seaexplorer object\n")
                    return(x@data[payloadName]$oxygen)
                } else if ("oxygenFrequency" %in% dataNames) {
                    gliderDebug(debug, "  computing \"oxygen\" from \"oxygenFrequency\" in seaexplorer object\n")
                    if (!"oxycalib" %in% names(x@metadata)) {
                        warning(
                            "cannot compute oxygen, because metadata lacks an",
                            " oxycalib item, with which to compute oxygen using data$payload1$oxygenFrequency"
                        )
                        return(NULL)
                    }
                    cal <- x@metadata$oxycalib
                    oxygenFrequency <- x@data[[payloadName]]$oxygenFrequency
                    # cat(oce::vectorShow(oxygenFrequency))
                    salinity <- x@data[[payloadName]]$salinity
                    # cat(oce::vectorShow(salinity))
                    temperature <- x@data[[payloadName]]$temperature
                    # cat(oce::vectorShow(temperature))
                    pressure <- x@data[[payloadName]]$pressure
                    # cat(oce::vectorShow(pressure))
                    cc <- cal$calibrationCoefficients
                    # This Kelvin temperature is as used in swSatOw.  Note the
                    # non-standard offset and the non-unity factor
                    # Tk <- 273.15 + temperature * 1.00024
                    # _ # NOTE: the calibration formula I have for
                    # _ # sea-explorer datasets also has something called
                    # _ # Tau20, but I don't see that below. For details
                    # _ # on the formula, see
                    # _ # https://github.com/DFOglider/pilotingApp/blob/glimpseFtp/oxygenCalibrationCoefficients.R
                    # _ # and emails from Chantelle Layton and Clark
                    # _ # Richards.
                    # _ res <- cc$Soc * (oxygenFrequency + cc$Foffset) *
                    # _     (1.0 + cc$A * temperature + cc$B * temperature^2 + cc$C * temperature^3) *
                    # _     swSatO2(temperature = temperature, salinity = salinity) *
                    # _     exp(cc$Enom * pressure / Tk)
                    # _ res <- 44.6591 * res # the factor converts to umol/kg
                    return(swOxygenFrequencyToSaturation(
                        temperature = temperature, salinity = salinity,
                        pressure = pressure, frequency = oxygenFrequency, cal = cc, unit = "umol/kg"
                    ))
                } else {
                    return(NULL)
                }
            } else if (identical(x@metadata$type, "slocum")) {
                gliderDebug(debug, "  analysing \"slocum\" object\n", sep = "")
                message("FIXME: get oxygen from slocum")
                dataNames <- names(x@data)
                # I don't know whether oxygen-frequency can be in such data,
                # and I don't have docs on calibration formulae, so I return
                # NULL in such a case; the user will need to do that work.
                for (nickname in c("oxygen", "oxygenConcentration", "O2")) {
                    if (nickname %in% dataNames) {
                        return(x@data[[nickname]])
                    }
                }
                return(NULL)
            } else {
                stop("glider type must be either \"seaexplorer\" or \"slocum\"")
            }
        }
        if (i == "?") {
            dataNames <- names(x@data)
            w <- grep("payload", dataNames)
            if (length(w) > 1L) {
                stop("cannot handle files with multiple 'payloadN' items")
            } else if (length(w) == 1L) {
                dataNames <- names(x@data[[dataNames[w]]])
            }
            # the next is patterned on oce:computableWaterProperties, which I
            # will update to handle names as a parameter, but I don't want to
            # make oceglider depend on a non-CRAN version of oce.
            if (all(c("salinity", "temperature", "pressure") %in% dataNames)) {
                dataNames <- c(dataNames, c(
                    "theta", paste("potential", "temperature"), "z",
                    "depth", "spice", "Rrho", "RrhoSF", "sigmaTheta", "SP",
                    "density", "N2", paste("sound", "speed")
                ))
            }
            if (all(c("longitude", "latitude") %in% dataNames)) {
                dataNames <- c(
                    dataNames, "SR", "Sstar",
                    paste0("sigma", 0:4),
                    "SA", paste("Absolute", "Salinity"),
                    "CT", paste("Conservative", "Temperature"),
                    paste0("spiciness", 0:2)
                )
            }
            # It is possible to compute nitrate from NO2+NO3 and nitrite, if
            # it's not stored in the object already.  This occurs in data(section).
            # I also added a similar scheme for nitrite, although I don't know
            # whether that condition ever happens, in practice.
            if (!("nitrate" %in% dataNames) && ("NO2+NO3" %in% dataNames) && ("nitrite" %in% dataNames)) {
                dataNames <- c(dataNames, "nitrate")
            }
            if (!("nitrite" %in% dataNames) && ("NO2+NO3" %in% dataNames) && ("nitrate" %in% dataNames)) {
                dataNames <- c(dataNames, "nitrite")
            }
            return(sort(unique(dataNames)))
        }
        # . message("it is a seaexplorer")
        if (i == "glider") {
            return(x@data$glider)
        }
        if (grepl("payload", i)) {
            dataNames <- names(x@data)
            w <- grep("payload", dataNames)
            if (length(w) > 1L) {
                stop("cannot handle files with multiple 'payloadN' items")
            } else if (length(w) == 1L) {
                # message("returning w=",w," i.e. ", dataNames[w])
                return(x@data[[dataNames[w]]])
            } else {
                return(NULL)
            }
        }
        if (missing(j)) {
            # . message("j is missing")
            if (i %in% names(x@metadata)) {
                gliderDebug(debug, "  returning item from @metadata\n")
                return(x@metadata[[i]])
            } else if (i %in% names(x@data)) {
                gliderDebug(debug, "  returning item from @data\n")
                return(x@data[[i]])
            } else {
                if (i %in% names(x@data$payload1)) {
                    gliderDebug(debug, "  result in @data$payload1\n")
                    return(x@data$payload1[[i]])
                } else if (i %in% names(x@data$glider)) {
                    gliderDebug(debug, "  result in @data$glider\n")
                    return(x@data$glider[[i]])
                } else if (i %in% names(x@metadata$dataNamesOriginal$payload1)) {
                    gliderDebug(debug, "  result in @data$payload1 inferred from original name\n")
                    iname <- names(which(x@metadata$dataNamesOriginal$payload1 == i))[2]
                    return(x@data$payload1[[iname]])
                } else if (i %in% names(x@metadata$dataNamesOriginal$glider)) {
                    gliderDebug(debug, "  result in @data$glider inferred from original name\n")
                    iname <- names(which(x@metadata$dataNamesOriginal$glider == i))[2]
                    return(x@data$glider[[iname]])
                }
                gliderDebug(debug, "  cannot find what to return\n")
                return(NULL)
            }
        }
        # . message("j is not missing. j='", j, "'")
        # if (j == "glider")
        #     return(x@data$glider)
        # if (j == "payload")
        #     return(x@data$payload1)
        # if (j == "payload1")
        #     return(x@data$payload1)
        # if (j == "payload2")
        #     return(x@data$payload2)
        # stop("type='", type, "' not permitted; it must be 'seaexplorer' or 'slocum'")
        warning("[[", i, ",", j, "]] not understood, so returning NULL", sep = "")
        return(NULL)
    }
)

#' Convert a string from snake_case to camelCase
#'
#' Convert a snake-case string (i.e., one constructed
#' with words separated by underlines) to a camel-case
#' string (i.e. one in which the words are strung together,
#' with upper-case for the first letter of all but the
#' first word).  See \sQuote{Examples}.
#'
#' @param s character value to be converted.
#'
#' @return [toCamelCase()] returns a camelCase version of `s`.
#'
#' @examples
#' toCamelCase("profile_direction") # "profileDirection"
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
toCamelCase <- function(s) {
    s <- strsplit(s, "")[[1]]
    r <- NULL
    n <- length(s)
    i <- 1
    while (i <= n) {
        if (s[i] == "_") {
            if (i < n) {
                r <- c(r, toupper(s[i + 1]))
            } else {
                warning("trailing underscores are ignored")
            }
            i <- i + 2
        } else {
            r <- c(r, s[i])
            i <- i + 1
        }
    }
    paste(r, collapse = "")
}



#' Convert longitude and latitude from a combined degree+minute formula
#'
#' SeaExplorer gliders save longitude and latitude with e.g.
#' 4530.100 standing for 45deg 30.1min (which is 45.50167
#' in conventional decimal format).  This function converts SeaExplorer
#' coordinate values to conventional decimal values.
#'
#' @param x Numerical value in degree+minute notation (see \dQuote{Examples}).
#'
#' @return [degreeMinute()] returns a numerical value in decimal degrees.
#'
#' @examples
#' degreeMinute(4530.100) # 45+30.100/60
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
degreeMinute <- function(x) {
    s <- sign(x)
    x <- abs(x)
    degree <- floor(x / 100)
    minute <- x - 100 * degree
    s * (degree + minute / 60)
}



#' Print a debugging message
#'
#' Many glider functions decrease the `debug` level by 1 when they call other
#' functions, so the effect is a nesting, with more space for deeper function
#' level.
#'
#' @param debug an integer, less than or equal to zero for no message, and
#' greater than zero for increasing levels of debugging.  Values greater than 4
#' are treated like 4.
#'
#' @param \dots items to be supplied to [cat()], which does the
#' printing.  Almost always, this should include a trailing newline.
#'
#' @param unindent Number of levels to unindent, e.g. for start and end lines
#' from a called function.
#'
#' @author Dan Kelley
#'
#' @importFrom utils flush.console
#'
#' @md
#'
#' @export
gliderDebug <- function(debug = 0, ..., unindent = 0) {
    debug <- if (debug > 4) 4 else max(0, floor(debug + 0.5))
    if (debug > 0) {
        n <- debug - unindent
        if (n > 0) {
            cat(paste(rep("  ", n), collapse = ""), sep = "")
        }
        cat(...)
    }
    utils::flush.console()
    invisible(NULL)
}

#' Check whether a URL exists, backtracing by separators, if not
#'
#' This uses [RCurl::url.exists()] to see if the indicated URL exists.
#' If not, an attempt is made to find a lower-level URL that does exist.
#' This is done by progressively removing items separated by `"/"`
#' in `url`
#'
#' @param url Character value specifying the URL. If no `/` is present
#' at the end of this string, then it is added before checks are done.
#'
#' @param quiet Logical value indicating whether to print a suggestion
#' for an alternative website, in the case where `url` does not exist.
#'
#' @return A logical value indicating whether the website indicated
#' by `url` exists.
#'
#' @author Dan Kelley
#'
## @importFrom RCurl url.exists
#'
#' @md
#'
#' @export
urlExists <- function(url, quiet = FALSE) {
    # tack a '/' on the end, if not there already
    urlOrig <- url
    if (0 == length(grep("/$", url))) {
        url <- paste(url, "/", sep = "")
    }
    if (!requireNamespace("RCurl", quietly = TRUE)) {
        stop("must install.packages(\"RCurl\") to read this data type")
    }
    exists <- RCurl::url.exists(url)
    if (exists) {
        return(TRUE)
    } else {
        while (!quiet && TRUE) {
            w <- which("/" == strsplit(url, "")[[1]])
            if (length(w) > 1) {
                url <- strtrim(url, w[length(w) - 1])
                if (RCurl::url.exists(url)) {
                    if (!quiet) {
                        cat("'", urlOrig, "' does not exist, but '", url, "' does\n", sep = "")
                    }
                    break
                }
            }
        }
        return(FALSE)
    }
}

# a helper function to simplify code in read.glider.netcdf()
getAtt <- function(f, varid = 0, attname = NULL, default = NULL) {
    if (is.null(attname)) {
        stop("must give attname")
    }
    # message(attname)
    t <- try(ncdf4::ncatt_get(f, varid = varid, attname = attname), silent = TRUE)
    if (inherits(t, "try-error")) {
        NULL
    } else {
        if (t$hasatt) t$value else default
    }
}


#' Read a glider file in netcdf format
#'
#' \strong{This is a provisional function, written to handle some
#' particular files available to the author.}
#'
#' The data are copied directly from the file, except that `time`
#' is converted from an integer to a POSIX time. Variable names containing
#' underscores are renamed as e.g. `profile_direction`
#' to `profileDirection`, although the \code{\link{[[,glider-method}}
#' mechanism works with either name, e.g. if `g` is a glider object, then
#' `g[["profileDirection"]]` and
#' `g[["profile_direction"]]` give the same result.
#'
#' See the SeaExplorer vignette for examples of using this
#' function.
#'
#' @param file Name of a netcdf file.
#'
#' @template debug
#'
#' @return A glider object, i.e. one inheriting from [glider-class].
#' (This class inherits from [oce::oce-class] in the
#' \CRANpkg{oce} package.)
#'
#' @author Dan Kelley
#'
## @examples
## \dontrun{
## library(oceglider)
##
## # NOTE: these files are of order 100Meg, so they are
## # not provided with the package as samples. In both
## # examples, we plot a map and then an incidence-TS plot.
##
## # Seaexplorer data, from DFO (January 2019)
## g <- read.glider.netcdf("~/Dropbox/glider_dfo.nc")
## # Remove spurious times, from a year before deployment
## g <- subset(g, time > as.POSIXct("2018-01-01"))
## # Remove any observation with bad salinity
## g <- subset(g, is.finite(g[["salinity"]]))
## plot(g, which = "map")
## ctd <- as.ctd(g[["salinity"]], g[["temperature"]], g[["pressure"]],
##     longitude = g[["longitude"]], latitude = g[["latitude"]]
## )
## plotTS(ctd, useSmoothScatter = TRUE)
##
## # Slocum data,from Dalhousie CEOTR rdapp (April 2019)
## g <- read.glider.netcdf("~/Dropbox/glider_erdapp.nc")
## # Remove any observation with bad salinity
## g <- subset(g, is.finite(g[["salinity"]]))
## plot(g, which = "map")
## ctd <- as.ctd(g[["salinity"]], g[["temperature"]], g[["pressure"]],
##     latitude = g[["latitude"]], longitude = g[["longitude"]]
## )
## plotTS(ctd, useSmoothScatter = TRUE)
## }
#'
#' @family functions to read glider data
#' @family functions to read netcdf glider data
#'
#' @importFrom oce as.unit
#' @importFrom ncdf4 nc_open ncatt_get ncatt_get ncvar_get
#'
#' @md
#'
#' @export
read.glider.netcdf <- function(file, debug) {
    if (missing(debug)) {
        debug <- getOption("gliderDebug", default = 0)
    }
    gliderDebug(debug, "read.glider.netcdf(file=\"", file, "\", ...) {", unindent = 1, sep = "")
    if (missing(file)) {
        stop("must provide `file'")
    }
    if (length(file) != 1) {
        stop("file must have length 1")
    }
    capture.output({
        f <- ncdf4::nc_open(file)
    })
    res <- new("glider")

    # Next demonstrates how to detect this filetype.
    res@metadata$instrument <- getAtt(f, attname = "instrument", default = "?")
    res@metadata$instrumentManufacturer <- getAtt(f, attname = "instrument_manufacturer", default = "?")
    res@metadata$instrumentModel <- getAtt(f, attname = "instrument_model", default = "?")
    type <- getAtt(f, attname = "platform_type", default = "?")
    if (!is.null(type)) {
        type <- gsub("Glider$", "", type)
        type <- tolower(type)
        type <- trimws(type)
    }
    res@metadata$type <- type
    data <- list()
    # FIXME get units
    # FIXME change some variable names from snake-case to camel-case
    dataNames <- names(f$var)
    data$time <- numberAsPOSIXct(as.vector(ncdf4::ncvar_get(f, "time")))
    dataNamesOriginal <- list()
    # ? if (!"time" %in% dataNames)
    # ?     dataNamesOriginal$time <- "-"
    # Get all variables, except time, which is not listed in f$var
    gliderDebug(debug, "reading and renaming data, plus collecting units\n")
    units <- list()
    for (i in seq_along(dataNames)) {
        oceName <- toCamelCase(dataNames[i])
        dataNamesOriginal[[oceName]] <- dataNames[i]
        if (dataNames[i] == "time") {
            data[["time"]] <- numberAsPOSIXct(as.vector(ncdf4::ncvar_get(f, "time")))
            gliderDebug(debug, "i=", i, " ... time converted from integer to POSIXct\n", sep = "")
        } else {
            data[[oceName]] <- as.vector(ncdf4::ncvar_get(f, dataNames[i]))
            u <- ncdf4::ncatt_get(f, dataNames[i], "units")$value
            unit <- oce::as.unit(u, default = NULL)
            if (is.null(unit)) {
                unit <- list(unit = bquote(.(u)), scale = "")
                message("DDDD")
            }
            if (debug > 0) {
                cat("  original name = \"", dataNames[i], "\"\n", sep = "")
                cat("    oce name = \"", oceName, "\"\n", sep = "")
                cat("    original unit = ", u, "\n", sep = "")
                cat("    oce unit = ", as.character(unit$unit), "\n", sep = "")
            }
            units[[oceName]] <- unit
            dataNames[i] <- oceName
        }
    }
    # gliderDebug(debug, "dataNames:", paste(dataNames, collapse=";"), "\n")
    # names(data) <- if ("time" %in% dataNames) dataNames else c("time", dataNames)
    res@data$payload1 <- as.data.frame(data)
    # head(res@data$payload1$time)
    res@metadata$filename <- file
    res@metadata$dataNamesOriginal <- list(payload1 = dataNamesOriginal)
    res@metadata$units <- units
    #if (debug > 1) {
    #    message("DAN next is @metadata$units at end")
    #    print(res@metadata$units)
    #}
    gliderDebug(debug, "} # read.glider.netcdf", unindent = 1, sep = "")
    res
}

#' Read a glider data file
#'
#' This is a high-level function that passes control to [read.glider.netcdf()]
#' if the first argument is a string ending with `".nc"`, to
#' [read.glider.seaexplorer.realtime()] if it is a vector of strings, any
#' of which contains the text `"pld1.sub."` followed by one or more digits, or to
#' [read.glider.seaexplorer.delayed()] if it is a vector of strings, any
#' contains the text `"pld1.raw."` followed by one or more digits.
#'
#' @param file Character value giving the name of the file.
#'
#' @param ... Extra parameters passed to more specific `read.*` functions.
#'
#' @template debug
#'
#' @author Dan Kelley
#'
#' @return A `glider` object, i.e. one inheriting from [glider-class].
#'
#' @md
#'
#' @export
read.glider <- function(file, debug, ...) {
    if (missing(debug)) {
        debug <- getOption("gliderDebug", default = 0)
    }
    gliderDebug(debug, "read.glider() {", unindent = 1, sep = "")
    if (!is.character(file)) {
        stop("'file' must be a character value (or values) giving filename(s)")
    }
    if (length(file) == 1 && length(grep(".nc$", file))) {
        res <- read.glider.netcdf(file = file, debug = debug - 1, ...)
    } else if (0 != length(grep("pld1.sub", file))) {
        res <- read.glider.seaexplorer.realtime(file, debug = debug - 1, ...)
    } else if (0 != length(grep("pld1.raw", file))) {
        res <- read.glider.seaexplorer.delayed(file, debug = debug - 1, ...)
    } else {
        stop("only .nc and .gz files handled")
    }
    gliderDebug(debug, "} # read.glider()", unindent = 1, sep = "")
    res
}

#' Convert data to glider format
#'
#' This function returns a glider object that holds data as provided
#' in the `data` argument, with units as provided by the `units`
#' argument. The `units` argument is optional, making the function
#' easy to use in interactive sessions, but production code ought to
#' be written with units fully specified.
#'
#' @param type Character value giving the type of glider, e.g.
#' be either `seaexplorer` or `slocum`.
#'
#' @param data A data frame containing the data. This is copied straight into
#' the `payload1` item in the `data` slot of the returned value,
#' \emph{without} name translation. For most functions in this package to work,
#' `data` ought to have items named `longitude`,
#' `latitude`, `salinity`, `temperature` and
#' `pressure`.
#'
#' @param units A list holding units, with names corresponding to the
#' names in the data. See the example for the format to be used
#' for `units`, but note that there are several items in this
#' dataset that are not given units, in this example.
#'
#' @examples
#' library(oceglider)
#' directory <- system.file("extdata/seaexplorer/raw", package = "oceglider")
#' g <- read.glider.seaexplorer.delayed(directory)
#' data <- g[["payload"]]
#' units <- list(
#'     temperature = list(unit = expression(degree * C), scale = "ITS-90"),
#'     salinity = list(unit = expression(), scale = "PSS-78"),
#'     pressure = list(unit = expression(dbar), scale = ""),
#'     longitude = list(unit = expression(degree * E), scale = ""),
#'     latitude = list(unit = expression(degree * N), scale = "")
#' )
#' gg <- as.glider("seaexplorer", data, units)
#' par(mfrow = c(2, 1))
#' plot(g, which = "p")
#' plot(gg, which = "p")
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
as.glider <- function(type, data, units) {
    if (missing(type)) {
        stop("'type' must be given")
    }
    if (missing(data)) {
        stop("'data' must be given")
    }
    res <- new("glider")
    res@metadata$type <- type
    streamname <- "payload1"
    res@metadata$dataNamesOriginal <- list(names = names(data))
    res@data[[streamname]] <- data
    if (!missing(units)) {
        res@metadata$units <- list(payload1 = units)
    }
    res
}

#' Find a Deconflicted Variable Name
#'
#' This is used to name the first temperature found as `temperature`,
#' the second as `temperature2`, etc.
#'
#' @return [getNextName] returns a deconflicted version of `name`.
#'
#' @param name character value indicating the variable name.
#' @param existingNames vector of character values of existing
#' variable names.
#'
#' @examples
#' library(oceglider)
#' e <- NULL
#' e <- c(e, getNextName("S", e))
#' e <- c(e, getNextName("T", e))
#' e <- c(e, getNextName("S", e))
#' e <- c(e, getNextName("T", e))
#' e <- c(e, getNextName("p", e))
#' print(e)
#'
#' @export
#'
#' @author Dan Kelley
getNextName <- function(name, existingNames) {
    # cat(oce::vectorShow(name))
    # cat(oce::vectorShow(existingNames))
    if (!name %in% existingNames) {
        name
    } else {
        # cat("need to deconflict '", name, "'")
        siblings <- grep(paste0("^", name, "[0-9]*$"), existingNames)
        if (length(siblings) == 1) {
            paste0(name, "2")
        } else {
            # cat(oce::vectorShow(existingNames[siblings]))
            numbers <- gsub("^[_a-zA-Z]", "", existingNames[siblings])
            # cat(oce::vectorShow(numbers))
            numberNext <- 1L + max(as.integer(numbers), na.rm = TRUE)
            # cat(oce::vectorShow(numberNext))
            paste0(name, numberNext)
        }
    }
}
