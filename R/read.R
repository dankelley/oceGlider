#' Read a glider data file
#'
#' This is a high-level function that passes control to [read.glider.netcdf()]
#' if the first argument is a string ending with `".nc"`, to
#' [read.glider.seaexplorer.realtime()] if it is a vector of strings, any
#' of which contains the text `"pld1.sub."` followed by one or more digits, or to
#' [read.glider.seaexplorer.raw()] if it is a vector of strings, any
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
read.glider <- function(file, debug = getOption("gliderDebug", default = 0), ...) {
    gliderDebug(debug, "read.glider() START", unindent = 1, sep = "")
    if (!is.character(file)) {
        stop("'file' must be a character value (or values) giving filename(s)")
    }
    if (length(file) == 1L && length(grep(".nc$", file))) {
        res <- read.glider.netcdf(file = file, debug = debug - 1, ...)
    } else if (grepl("pld1.sub", file)) {
        res <- read.glider.seaexplorer.realtime(file, debug = debug - 1, ...)
    } else if (grepl("pld1.raw", file)) {
        res <- read.glider.seaexplorer.raw(file, debug = debug - 1, ...)
    } else {
        stop("only .nc and .gz files handled")
    }
    gliderDebug(debug, "read.glider() END", unindent = 1, sep = "")
    res
}
