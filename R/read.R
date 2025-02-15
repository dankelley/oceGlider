#' Read a glider data file
#'
#' This is a high-level function that passes control to [read.glider.netcdf()]
#' if the first argument is a string ending with `".nc"` or to
#' [read.glider.seaexplorer.raw()].
#'
#' @param file character value giving the name of the file.
#'
#' @param ... extra parameters passed to more specific `read.*` functions.
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
    } else { # FIXME not sure if there is a string-test way to know it's raw
        res <- read.glider.seaexplorer.raw(file, debug = debug - 1, ...)
    }
    gliderDebug(debug, "read.glider() END", unindent = 1, sep = "")
    res
}
