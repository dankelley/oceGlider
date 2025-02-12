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

#<not used> #' Check whether a URL exists, backtracing by separators, if not
#<not used> #'
#<not used> #' This uses [RCurl::url.exists()] to see if the indicated URL exists.
#<not used> #' If not, an attempt is made to find a lower-level URL that does exist.
#<not used> #' This is done by progressively removing items separated by `"/"`
#<not used> #' in `url`
#<not used> #'
#<not used> #' @param url Character value specifying the URL. If no `/` is present
#<not used> #' at the end of this string, then it is added before checks are done.
#<not used> #'
#<not used> #' @param quiet Logical value indicating whether to print a suggestion
#<not used> #' for an alternative website, in the case where `url` does not exist.
#<not used> #'
#<not used> #' @return A logical value indicating whether the website indicated
#<not used> #' by `url` exists.
#<not used> #'
#<not used> #' @author Dan Kelley
#<not used> #'
#<not used> ## @importFrom RCurl url.exists
#<not used> #'
#<not used> #' @md
#<not used> #'
#<not used> #' @export
#<not used> urlExists <- function(url, quiet = FALSE) {
#<not used>     # tack a '/' on the end, if not there already
#<not used>     urlOrig <- url
#<not used>     if (0 == length(grep("/$", url))) {
#<not used>         url <- paste(url, "/", sep = "")
#<not used>     }
#<not used>     if (!requireNamespace("RCurl", quietly = TRUE)) {
#<not used>         stop("must install.packages(\"RCurl\") to read this data type")
#<not used>     }
#<not used>     exists <- RCurl::url.exists(url)
#<not used>     if (exists) {
#<not used>         return(TRUE)
#<not used>     } else {
#<not used>         while (!quiet && TRUE) {
#<not used>             w <- which("/" == strsplit(url, "")[[1]])
#<not used>             if (length(w) > 1) {
#<not used>                 url <- strtrim(url, w[length(w) - 1])
#<not used>                 if (RCurl::url.exists(url)) {
#<not used>                     if (!quiet) {
#<not used>                         cat("'", urlOrig, "' does not exist, but '", url, "' does\n", sep = "")
#<not used>                     }
#<not used>                     break
#<not used>                 }
#<not used>             }
#<not used>         }
#<not used>         return(FALSE)
#<not used>     }
#<not used> }

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
#' @param file character value holding the name of a netcdf file
#' that holds glider data.
#'
#' @param saveGlobalAttributes logical value indicating whether to
#' read the entiriety of the global attributes stored within the
#' file into the `metadata` slot in a list named `globalAttributes`.
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
read.glider.netcdf <- function(file, saveGlobalAttributes = TRUE,
                               debug = getOption("gliderDebug", default = 0)) {
    gliderDebug(debug, "read.glider.netcdf(file=\"", file, "\", ...) BEGIN\n", unindent = 1, sep = "")
    if (missing(file)) {
        stop("must provide file")
    }
    if (length(file) != 1) {
        stop("can only read one file at a time")
    }
    res <- new("glider")
    capture.output({
        f <- ncdf4::nc_open(file)
    })
    # get all global attributes (see https://github.com/dankelley/oceglider/issues/125)
    res@metadata$globalAttributes <- list()
    if (saveGlobalAttributes) {
        for (name in names(ncatt_get(f, ""))) {
            newname <- toCamelCase(name)
            gliderDebug(debug, "global attribute '", name, "' stored as '", newname, "'\n",
                sep = ""
            )
            res@metadata$globalAttributes[[newname]] <- ncatt_get(f, varid = 0, attname = name)$value
        }
    }
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
        gliderDebug(debug, "preparing to read ", dataNames[i], " into @data$", oceName, "\n", sep = "")
        if (dataNames[i] == "time") {
            data[["time"]] <- numberAsPOSIXct(as.vector(ncdf4::ncvar_get(f, "time")))
            gliderDebug(debug, "i=", i, " ... time converted from integer to POSIXct\n", sep = "")
        } else {
            data[[oceName]] <- as.vector(ncdf4::ncvar_get(f, dataNames[i]))
            # Infer the units, if sufficient information is available
            tmp <- ncdf4::ncatt_get(f, dataNames[i], "units")
            unit <- if (tmp$hasatt) {
                oce::as.unit(tmp$value, default = NULL)
            } else {
                list(unit = expression(), scale = "")
            }
            # ?? # some local unit decoding
            # ?? if (is.null(unit)) {
            # ??     if (u == "Celsius") {
            # ??         unit <- list(unit = expression(degree * C), scale = "")
            # ??     }
            # ?? }
            # ?? # as a last resort, don't try to make an expression (this loses superscipts,
            # ?? # for example)
            # ?? if (is.null(unit)) {
            # ??     unit <- list(unit = bquote(.(u)), scale = "")
            # ?? }
            if (debug > 0) {
                cat("  original name = \"", dataNames[i], "\"\n", sep = "")
                cat("    oce name = \"", oceName, "\"\n", sep = "")
                # cat("    original unit = ", u, "\n", sep = "")
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
    # if (debug > 1) {
    #    message("DAN next is @metadata$units at end")
    #    print(res@metadata$units)
    # }
    gliderDebug(debug, "# END read.glider.netcdf", unindent = 1, sep = "")
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
#' directory <- system.file("extdata/sea_explorer/delayed_raw", package = "oceglider")
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
