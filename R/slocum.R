# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read a Slocum Glider File in CSV Format
#'
#' This function handles a CSV format used by some files
#' made available to the author at some time in the 2010s.
#' In the meantime, the community has settled on other formats,
#' such as NetCDF (see [read.glider.netcdf()] for how to read
#' such files).  For that reason, there are no plans to
#' extend or even maintain this function.
#'
#' @param file A connection or a character string giving the name of the file to
#' load.
#'
#' @param nameMap List used to rename data columns. See \dQuote{Details}.
#'
#' @param debug integer controlling the amount of debugging output
#' printed. Use 0 for no output, 1 for some.
#'
#' @return An oce object holding the data, with variables renamed as
#' described in \dQuote{Details}, and with `salinity` added,
#' as calculated by [oce::swSCTp()] which uses the UNESCO
#' algorithm and assumes that the conductivity values are stored in S/m
#' units.
#'
#' @examples
#' library(oceglider)
#' if (file.exists("~/slocum.csv")) {
#'     g <- read.glider.slocum.csv("~/slocum.csv")
#'     summary(g)
#'
#'     # 1. Plot time-depth trace, colour-coded for temperature
#'     par(mar = c(3, 3, 1, 1), mgp = c(2, 0.7, 0)) # thin margins
#'     cm <- colormap(z = g[["temperature"]])
#'     drawPalette(colormap = cm, cex.axis = 3 / 4)
#'     t <- g[["time"]]
#'     p <- g[["depth"]]
#'     plot(t, p,
#'         ylim = rev(range(p, na.rm = TRUE)),
#'         xlab = "Time", ylab = "Pressure [dbar]",
#'         col = cm$zcol, cex = 1 / 2, pch = 20
#'     )
#'     mtext(paste("Temperature, from", t[1]), cex = 3 / 4)
#'
#'     # 2. Plot distance-depth trace, colour-coded for temperature
#'     dist <- geodDist(g[["longitude"]], g[["latitude"]], alongPath = TRUE)
#'     par(mar = c(3, 3, 1, 1), mgp = c(2, 0.7, 0)) # thin margins
#'     cm <- colormap(z = g[["temperature"]])
#'     drawPalette(colormap = cm, cex.axis = 3 / 4)
#'     p <- g[["depth"]]
#'     plot(dist, p,
#'         ylim = rev(range(p, na.rm = TRUEp)),
#'         xlab = "Distance [km]", ylab = "Pressure [dbar]",
#'         col = cm$zcol, cex = 1 / 2, pch = 20
#'     )
#'     mtext(paste("Temperature, from", t[1]), cex = 3 / 4)
#' }
#'
#' @family functions for slocum gliders
#' @family functions to read glider data
#'
#' @importFrom utils read.csv
#' @importFrom methods new
#' @importFrom oce numberAsPOSIXct swSCTp
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
read.glider.slocum.csv <- function(
    file,
    nameMap = list(
        conductivity = "sci_water_cond",
        temperature = "sci_water_temp",
        pressure = "sci_water_pressure",
        longitude = "lon",
        latitude = "lat",
        depth = "i_depth",
        debug = getOption("gliderDebug", default = 0)
    )) {
    debug <- min(1L, max(0L, as.integer(debug))) # make 0L or 1L
    if (missing(file)) {
        stop("must provide `file'")
    }
    if (length(file) != 1) {
        stop("file must have length 1")
    }
    gliderDebug(debug, "read.glider.slocum.csv(file=\"", file, "\", ...) START\n",
        unindent = 1, sep = ""
    )
    filename <- ""
    if (is.character(file)) {
        filename <- normalizePath(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    data <- utils::read.csv(filename, header = TRUE)
    names <- names(data)
    nameMapNames <- names(nameMap)
    gliderDebug(debug, 'original data names: "', paste(names, collapse = '", "'), '"\n')
    rval <- methods::new("glider")
    rval@metadata$type <- "slocum"
    rval@metadata$dataNamesOriginal <- list()
    for (iname in seq_along(names)) {
        if (names[iname] %in% nameMap) {
            newName <- nameMapNames[which(names[iname] == nameMap)]
            rval@metadata$dataNamesOriginal[[newName]] <- names[iname]
            names[iname] <- newName
        } else {
            rval@metadata$dataNamesOriginal[[names[iname]]] <- names[iname]
        }
    }
    gliderDebug(debug, 'new data names: "', paste(names, collapse = '", "'), '"\n')
    names(data) <- names
    salinity <- oce::swSCTp(data$conductivity, data$temperature, data$pressure,
        conductivityUnit = "S/m", eos = "unesco"
    )
    data$salinity <- salinity
    data$time <- oce::numberAsPOSIXct(data$unix_timestamp, "unix")
    rval@data$payload1 <- as.data.frame(data)
    rval@metadata$filename <- filename
    res@metadata$dataAreStreamed <- TRUE
    gliderDebug(debug, "END read.glider.slocum.csv\n")
    rval
}
