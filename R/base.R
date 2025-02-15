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
        .Object@metadata$dataAreStreamed <- FALSE
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
#' g <- read.glider.seaexplorer.raw(directory, "pld1.raw", progressBar = FALSE)
#' data <- g[["data"]]
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
    res@metadata$dataNamesOriginal <- list(names = names(data))
    res@data <- data
    res@metadata$dataAreStreamed <- FALSE
    if (!missing(units)) {
        res@metadata$units <- units
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
