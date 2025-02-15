#' Delete data for an interval after each power-up event
#'
#' This trims the spurious data that instruments can produce
#' for a short time interval after each power-on events.
#'
#' @param x an [oceglider-class] object, e.g. as read by
#' `read.read.glider.seaexplorer.raw()` or similar
#' functions.
#'
#' @param gapTime numeric value, in seconds, used to infer power-on
#' events.  There is always one event at the start of the time series. In
#' addition, any time differences that exceed `startupCriterion` seconds
#' are considered to be power-off events, so the first time after such
#' an interval is taken to be a power-on event.
#'
#' @param deleteTime numeric value indicating the time interval,
#' in seconds, of data that will be removed.
#'
#' @param stream character value naming the stream to use, if the object
#' is multi-streamed, meaning that the `data` slot contains multiple data frames,
#' one for the glider (named `glider`) another for the first payload (named
#' `"payload"`), and possibly other payloads. The value of `stream` is ignored
#' unless the object's `metadata` slot contains an item named `dataAreStreamed`
#' and that item is TRUE.
#'
#' @param debug an integer, less than or equal to zero for no message, and
#' positive to print information about the processing.
#'
#' @export
#'
#' @author Dan Kelley
deleteStartupData <- function(x, gapTime = 60, deleteTime = 20, stream = "payload1",
                            debug = getOption("gliderDebug", default = 0)) {
    gliderDebug(debug, "deleteStartupData() START\n", sep = "", unindent = 1)
    if (gapTime > 0.0 && deleteTime > 0.0) {
        dataAreStreamed <- x@metadata$dataAreStreamed
        if (is.null(dataAreStreamed)) dataAreStreamed <- FALSE
        time <- if (dataAreStreamed) x@data[[stream]]$time else x@data$time
        ntime <- length(time)
        startIndices <- c(1L, which(diff(time) > gapTime) + 1L)
        keep <- rep(TRUE, length(time))
        for (startIndex in startIndices) {
            endIndex <- which(time > time[startIndex] + gapTime + deleteTime)[1]
            if (0 == length(endIndex)) {
                endIndex <- ntime
            }
            keep[seq(startIndex, endIndex)] <- FALSE
        }
        gliderDebug(debug, sprintf("trimming %.3f%% of the dataset\n", 100 * sum(!keep) / ntime))
        # FIXME what if there are streams?
        time <- if (dataAreStreamed) {
            x@data[[stream]] <- x@data[keep, ]
        } else {
            x@data <- x@data[keep, ]
        }
    }
    gliderDebug(debug, "deleteStartupData() END\n", sep = "", unindent = 1)
    x
}
