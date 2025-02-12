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
#' directory <- system.file("extdata/sea_explorer/delayed_raw", package = "oceglider")
#' g <- read.glider.seaexplorer.delayed(directory, pattern = "plyd1.sub")
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
            if (missing(j)) "(missing)" else j, ", ...]] START\n",
            sep = "", unindent = 1
        )
        if (i == "filename") {
            return(x@metadata$filename)
        } else if (i == "data") {
            return(x@data)
        } else if (i == "metadata") {
            return(x@metadata)
        } else if (i == "yo" && !missing(j)) { # NOTE: not 'yoNumber'
            lines <- which(x@data$yoNumber == j)
            x@data <- x@data[lines, ]
            for (f in names(x@metadata$flags$payload1)) {
                x@metadata$flags[[f]] <- x@metadata$flags[[f]][lines]
            }
            return(x)
        }
        type <- x@metadata$type
        dataIsSubdivided <- identical(sort(names(g@data)), c("glider", "payload1"))
        if (is.null(type)) {
            stop("'type' is NULL")
        }
        if (length(grep("Flag$", i))) {
            # returns a list
            return(if ("flags" %in% names(x@metadata)) x@metadata$flags[[gsub("Flag$", "", i)]] else NULL)
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
        if (missing(j)) {
            # . message("j is missing")
            if (i %in% names(x@metadata)) {
                gliderDebug(debug, "  returning item from @metadata\n")
                return(x@metadata[[i]])
            } else if (i %in% names(x@data)) {
                gliderDebug(debug, "  returning item from @data\n")
                return(x@data[[i]])
            } else {
                if (dataIsSubdivided) { # FIXME: handle original names here also
                    for (name in names(x@data)) {
                        #cat(oce::vectorShow(name))
                        trial <- x@data[[name]][[i]]
                        if (!is.null(trial)) {
                            return(trial)
                        }
                        #cat(oce::vectorShow(trial))
                    }
                    return(NULL)
                }
                # message("DAN FIXME 1")
                # cat(str(x@metadata$dataNamesOriginal$payload1))
                # message("DAN FIXME 2")
                if (i %in% names(x@data)) {
                    gliderDebug(debug, "  result in @data\n")
                    return(x@data[[i]])
                } else if (i %in% x@metadata$dataNamesOriginal) {
                    gliderDebug(debug, "  result in @data inferred from original name\n")
                    w <- which(x@metadata$dataNamesOriginal == i)
                    # print(w)
                    names <- names(x@metadata$dataNamesOriginal)
                    # print(names)
                    gliderDebug(debug, "  returning ", names[w], "\n")
                    return(x@data[[names[w]]])
                } else if (i %in% names(x@metadata$dataNamesOriginal)) {
                    gliderDebug(debug, "  result in @data$glider inferred from original name\n")
                    iname <- names(which(x@metadata$dataNamesOriginal == i))[2]
                    return(x@data$glider[[iname]])
                }
                gliderDebug(debug, "  cannot find what to return\n")
                return(NULL)
            }
        }
        warning("[[", i, ",", j, "]] not understood, so returning NULL", sep = "")
        return(NULL)
    }
)
