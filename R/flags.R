#' Handle Flags in glider Objects
#'
#' This function may be used to set suspicious data to `NA`,
#' or some other value, based on the values of corresponding data-quality
#' flags.
#'
#' The flags are stored within the object as a [list]
#' named `payload1`, which is stored within a list named `flags`
#' that is stored in the object's `metadata` slot. Both
#' `flags` and `flags$payload1` are set up when the object is
#' created, but values are inserted into `flags$payload1` are
#' inserted later, when the data are read by one of the `read.glider*`
#' functions.
#'
#' For example, [read.glider.seaexplorer.delayed()]
#' sets `flags$payload1$salinity` to be a vector of length
#' matching the data stored in `data$payload1$salinity`, and
#' does the same for temperature and some other things that are typically
#' assessed as part of quality-assessment procedures.  When these
#' things are set up, they are also assigned numerical values, one for
#' each element in the data set.  The initial value is set to
#' value 2, which means `not_evaluated`
#' in the IOOS 2017 quality-control scheme (see table 2 of reference 1).
#'
#' These numerical values provide a way to edit a dataset in an
#' convenient and traceable way, through the appropriate setting
#' of the `flags` and `actions` arguments. Flag values
#' may be altered with [setGliderFlags()], as
#' illustrated in the \dQuote{Examples} section.
#'
#' @param object An object of [glider-class].
#'
#' @param flags A `list` specifying flag values upon which
#' actions will be taken. This can take two forms. In the first, the
#' list has named elements each containing a vector of integers. For example,
#' salinities flagged with values of 3 ("suspect"), 4 ("fail")
#' or 9 ("missing") would be specified by `flags=list(salinity=c(3,4,9))`.
#' Several data items can be specified,
#' e.g. `flags=list(salinity=c(3,4,9),temperature=c(3,4,9))` indicates
#' that the actions are to take place for both salinity and temperature.
#' In the second form, `flags` is a list with unnamed vectors, and
#' this means to apply the actions to all the data entries; thus,
#' `flags=list(c(3,4,9))` means to apply not just to salinity and temperature,
#' but also to everything else for which flags have been set up. If `flags`
#' is not provided, then an attempt is made to set up a
#' useful default.
#'
#' @param actions An optional `list` that contains items with
#' names that match those in the `flags` argument.  If `actions`
#' is not supplied, the default will be to set all values identified by
#' `flags` to `NA`; this can also be specified by
#' specifying `actions=list("NA")`. It is also possible to specify
#' functions that calculate replacement values. These are provided
#' with `object` as the single argument, and must return a
#' replacement for the data item in question.
#'
#' @param where An optional string that permits data and flags to be stored
#' indirectly, e.g. with data in `object@data[[where]]` instead of
#' in `object@data`, and flags in `object@metadata$flags[[where]]` instead of in
#' `object@metadata$flags`. If `where` is NULL, the second forms are used. This
#' scheme is needed because SeaExplorer data are stored in this manner.
#'
#' @param debug An optional integer specifying the degree of debugging, with
#' value 0 meaning to skip debugging and 1 or higher meaning to print some
#' information about the arguments and the data. It is usually a good idea to set
#' this to 1 for initial work with a dataset, to see which flags are being
#' handled for each data item. If not supplied, this defaults to the value of
#' `\link{getOption}("gliderDebug",0)`.
#'
#' @examples
#' library(oceglider)
#' directory <- system.file("extdata/sea_explorer/delayed_raw", package = "oceglider")
#' g <- read.glider.seaexplorer.delayed(directory, progressBar = FALSE)
#'
#' # The histogram motivates a crude limit for anomalously low salinity.
#' par(mfrow = c(1, 2), mar = c(3, 3, 1, 1), mgp = c(2, 0.75, 0))
#' hist(g[["salinity"]], breaks = 100, xlab = "Original Salinity", main = "")
#' abline(v = 31, col = 2)
#'
#' # Flag value 3 means 'suspect' in the IOOS scheme [1, table]; other
#' # flags are pass=1, not_evaluated=2 (the default as read), fail=4, and missing=9.
#' g2 <- setGliderFlags(g, "salinity", g[["salinity"]] < 31, 3)
#' g3 <- handleGliderFlags(g2, c(3, 4, 9)) # use default action, which is "NA"
#' hist(g3[["salinity"]], breaks = 100, xlab = "Trimmed Salinity", main = "")
#'
#' @references
#' 1. U.S. Integrated Ocean Observing System.
#' "Manual for Real-Time Oceanographic Data Quality Control Flags, Version 1.2,"
#' 2020. \url{https://cdn.ioos.noaa.gov/media/2020/07/QARTOD-Data-Flags-Manual_version1.2final.pdf}.
#'
#' @author Dan Kelley
#'
#' @family functions relating to data-quality flags
#'
#' @export
#'
#' @md
handleGliderFlags <- function(object, flags = NULL, actions = NULL, where = "payload1", debug = getOption("gliderDebug", 0)) {
    # DEVELOPER 1: alter the next comment to explain your setup
    gliderDebug(debug, "handleGliderFlags()function\n", sep = "", unindent = 1)
    if (is.null(flags)) {
        flags <- c(3, 4, 9)
        if (is.null(flags)) {
            stop("must supply 'flags', or use initializeGliderFlagScheme() on the glider object first")
        }
    }
    if (is.null(actions)) {
        actions <- list("NA") # DEVELOPER 3: alter this line to suit a new data class
        names(actions) <- names(flags)
    }
    if (any(names(actions) != names(flags))) {
        stop("names of flags and actions must match")
    }
    handleGliderFlagsInternal(object = object, flags = flags, actions = actions, where = where, debug = debug)
}


## NOT EXPORTED #' Low-level function to handle flags
## NOT EXPORTED #'
## NOT EXPORTED #' @param object An `oceglider` object, i.e. an object inheriting
## NOT EXPORTED #' from [glider-class].
## NOT EXPORTED #'
## NOT EXPORTED #' @param flags A `list` that associates integer values
## NOT EXPORTED #" with names, e.g. `list(good=1, bad=2)`.
## NOT EXPORTED #'
## NOT EXPORTED #' @param actions A character vector, which is lengthened to match
## NOT EXPORTED #' the length of `flags`. The most common value is `"NA"`,
## NOT EXPORTED #' which means to set flaggd values to the missing-value code, `NA`.
## NOT EXPORTED #'
## NOT EXPORTED #' @param where An optional string that allows the user to over-ride
## NOT EXPORTED #' the automated detection of where data and flags exist, within
## NOT EXPORTED #' `object`.  If `object[["type"]]` is `"seaexplorer"`, this will
## NOT EXPORTED #' default to `payload1`; otherwise, it defaults to `NULL`. Users
## NOT EXPORTED #' are advised *not* to set `where`, and it is only included here
## NOT EXPORTED #' so that `handleFlagsoceglider` behaves like [oce::handleFlags()].
## NOT EXPORTED #'
## NOT EXPORTED #' @param debug An integer specifying the debugging level, with value
## NOT EXPORTED #' `0` meaning to act silently, and higher values meaning to print
## NOT EXPORTED #' some debugging information.
## NOT EXPORTED #'
## NOT EXPORTED #' @author Dan Kelley
## NOT EXPORTED #'
## NOT EXPORTED #' @export
## NOT EXPORTED #' @md
handleGliderFlagsInternal <- function(object, flags, actions, where = NULL, debug = 0) {
    gliderDebug(debug, "handleGliderFlagsInternal()\n", sep = "", unindent = 1)
    if (missing(flags)) {
        warning("no flags supplied (internal error; report to developer)")
        return(object)
    }
    if (debug > 0L) {
        cat("  flags=c(", paste(flags, collapse = ","), ")\n", sep = "")
        cat("  actions=c(", paste(actions, collapse = ","), ")\n", sep = "")
        cat("  where=\"", where, "\"\n", sep = "")
    }
    # Permit e.g. flags=c(1,3)
    if (!is.list(flags)) {
        flags <- list(flags)
    }
    if (missing(actions)) {
        warning("no actions supplied (internal error; report to developer)")
        return(object)
    }
    if (any(names(flags) != names(actions))) {
        stop("names of flags must match those of actions")
    }
    gliderDebug(debug, "flags=", paste(as.vector(flags), collapse = ","), "\n", sep = "")
    oflags <- if (is.null(where)) object@metadata$flags else object@metadata$flags[[where]]
    odata <- if (is.null(where)) object@data else object@data[[where]]
    if (length(oflags)) {
        singleFlag <- is.null(names(oflags)) # TRUE if there is just one flag for all data fields
        gliderDebug(debug, "singleFlag=", singleFlag, "\n", sep = "")
        if (singleFlag && (length(actions) > 1 || !is.null(names(actions)))) {
            stop("if flags is a list of a single unnamed item, actions must be similar")
        }
        gliderDebug(debug, "names(odata)=c(\"", paste(names(odata),
            collapse = "\", \""
        ), "\")\n", sep = "")
        if (singleFlag) {
            gliderDebug(debug, "single flag\n")
            # apply the same flag to *all* data.
            actionsThis <- actions[[1]] # FIXME: this seems wrong
            oflags <- unlist(oflags)
            gliderDebug(debug, "singleFlag: head(oflags)=c(",
                paste(head(oflags), collapse = ","), "), to be used for *all* data types.\n",
                sep = ""
            )
            for (name in names(odata)) {
                gliderDebug(debug, "handling flags for '", name, "'\n", sep = "")
                dataItemLength <- length(odata[[name]])
                gliderDebug(debug, "  initially, ", sum(is.na(odata[[name]])), " out of ", dataItemLength, " are NA\n", sep = "")
                actionNeeded <- oflags %in% if (length(names(flags))) flags[[name]] else flags[[1]]
                if (is.function(actionsThis)) {
                    gliderDebug(debug > 1, "  actionsThis is a function\n")
                    odata[[name]][actionNeeded] <- actionsThis(object)[actionNeeded]
                } else if (is.character(actionsThis)) {
                    gliderDebug(debug > 1, "  actionsThis is a string, '", actionsThis, "'\n", sep = "")
                    gliderDebug(debug > 1, "  head(actionNeeded)=c(", paste(head(actionNeeded), collapse = ","), ")\n", sep = "")
                    if (actionsThis == "NA") {
                        odata[[name]][actionNeeded] <- NA
                    } else {
                        stop("the only permitted character action is 'NA'")
                    }
                } else {
                    stop("action must be a character string or a function")
                }
                gliderDebug(debug, "  after handling flags, ", sum(is.na(odata[[name]])),
                    " out of ", length(odata[[name]]), " are NA\n",
                    sep = ""
                )
            }
            gliderDebug(debug, "done handling flags for all data in object\n")
        } else { # multiple flags: Apply individual flags to corresponding data fields
            gliderDebug(debug, "multiple flags\n")
            for (name in names(odata)) {
                flagsObject <- oflags[[name]]
                if (length(flagsObject) > 0L) {
                    gliderDebug(debug, "handling flags for '", name, "'\n", sep = "")
                    gliderDebug(debug, "  initially, ", sum(is.na(odata[[name]])),
                        " out of ", length(odata[[name]]), " are NA\n",
                        sep = ""
                    )
                    # if (debug) {
                    #    tab <- table(flagsObject)
                    #    if (length(tab) > 0L) {
                    #        cat("  unique(flagsObject) for ", name, ":\n")
                    #        print(table(flagsObject))
                    #    }
                    # }
                    if (!is.null(flagsObject)) {
                        dataItemLength <- length(odata[[name]])
                        # flagsThis <- oflags[[name]]
                        # gliderDebug(debug, "before converting to numbers, flagsThis=", paste(flagsThis, collapse=","), "\n")
                        if (name %in% names(oflags)) {
                            actionsThis <- if (length(names(actions))) actions[[name]] else actions[[1]]
                            gliderDebug(debug > 1, "  actionsThis: \"", paste(actionsThis, collapse = ","), "\"\n", sep = "")
                            actionNeeded <- oflags[[name]] %in% if (length(names(flags))) flags[[name]] else flags[[1]]
                            gliderDebug(debug > 1, "  head(actionNeeded)=c(", paste(head(actionNeeded), collapse = ","), ")\n", sep = "")
                            if (any(actionNeeded)) {
                                #  gliderDebug(debug, "\"", name, "\" has ", dataItemLength, " data, of which ",
                                #           sum(actionNeeded), " are flagged\n", sep="")
                                # if (debug > 1) {
                                #    cat("  actionsThis follows...\n")
                                #    print(actionsThis)
                                # }
                                if (is.function(actionsThis)) {
                                    odata[[name]][actionNeeded] <- actionsThis(object)[actionNeeded]
                                } else if (is.character(actionsThis)) {
                                    if (actionsThis == "NA") {
                                        odata[[name]][actionNeeded] <- NA
                                    } else {
                                        stop("the only permitted character action is 'NA'")
                                    }
                                } else {
                                    stop("action must be a character string or a function")
                                }
                            } else {
                                gliderDebug(debug, "  no action needed, since no \"", name, "\" data are flagged as stated\n", sep = "")
                            }
                        }
                    }
                    gliderDebug(debug, "  finally, ", sum(is.na(odata[[name]])),
                        " out of ", length(odata[[name]]), " are NA\n",
                        sep = ""
                    )
                }
            }
        } # multiple flags
    } else {
        gliderDebug(debug, "object has no flags in metadata\n")
    }
    if (is.null(where)) {
        object@data <- odata
    } else {
        object@data[[where]] <- odata
    }
    object@processingLog <- processingLogAppend(
        object@processingLog,
        paste("handleFlagsInternal(flags=c(",
            paste(substitute(flags, parent.frame()), collapse = ","),
            "), actions=c(",
            paste(substitute(actions, parent.frame()), collapse = ","),
            "))",
            collapse = " ", sep = ""
        )
    )
    gliderDebug(debug, "  returning from handleFlagsInternal()\n", sep = "", unindent = 1)
    object
}

#' Set data-quality flags within a glider object
#'
#' This function changes specified entries in the data-quality
#' flags of `glider` objects. Those flags are stored within
#' a list named `flags$payload1` that resides in the `metadata`
#' slot.
#'
#' @param object A glider object, i.e. an object inheriting from `glider-class`.
#'
#' @param name Character string indicating the name of the variable to be flagged. If
#' this variable is not contained in the object's `data` slot, an error is reported.
#'
#' @param i There are three choices for `i`. First, if
#' `i=="all"`, then any existing flags for the named item are discarded, and
#' replaced with the new `value`.  Second, if `i` is a vector of
#' integers, then flags are set to `value` at indices given by `i`.
#' Third, if it is a logical vector of the same length as the data, then just
#' those indices that match `TRUE` values in `i` are set to `value`.
#'
#' @param value The value to be inserted in the flag.
#'
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with flags set as indicated.
#'
#' @family functions relating to data-quality flags
#'
#' @seealso See [handleGliderFlags()] for an example of use.
#'
#' @author Dan Kelley
#'
#' @md
#'
#' @export
setGliderFlags <- function(object, name = NULL, i = NULL, value = NULL, debug = getOption("gliderDebug", 0)) {
    gliderDebug(debug, "setGliderFlags(object, name=\"", name, "\", value=", value,
        ", i=c(", paste(head(i), collapse = ","), "...), debug=", debug, ") {\n",
        sep = "",
        unindent = 1
    )
    res <- object
    # Ensure proper argument setup.
    if (is.null(name)) {
        stop("must supply a name")
    }
    if (is.null(i)) {
        stop("must supply 'i'")
    }
    setAll <- length(i) == 1 && i == "all"
    if (is.null(value)) {
        stop("must supply 'value'")
    }
    if (length(name) > 1) {
        stop("must specify one 'name' at a time")
    }
    where <- "payload1"
    if ("flags" %in% names(object@metadata) && where %in% names(object@metadata$flags)) {
        if (!(name %in% names(object@metadata$flags[[where]]))) {
            stop("object has no flag for \"", name, "\"; try one of: \"", paste(names(object@metadata$flags[[where]]), collapse = " "), "\"")
        }
        if (is.logical(i) && length(i) != length(res@metadata$flags[[where]][[1]])) {
            stop(
                "length of 'i' (", length(i), ") does not match length of object@data$payload1[[1]] (",
                length(res@metadata$flags[[where]][[1]])
            )
        }
        if (setAll) {
            i <- seq_along(object@data[[where]][[1]])
        }
        # Permit 'value' to be a character string, if a scheme already
        # exists and 'value' is one of the stated flag names.
        valueOrig <- value
        if (is.character(value)) {
            if (is.null(res@metadata$flagScheme)) {
                stop("cannot have character 'value' because initializeGliderFlagScheme() has not been called on object")
            } else {
                if (value %in% names(res@metadata$flagScheme$mapping)) {
                    value <- res@metadata$flagScheme$mapping[[value]]
                } else {
                    stop("value=\"", value, "\" is not defined in the object's flagScheme; try one of: \"",
                        paste(names(res@metadata$flagScheme$mapping), "\", \""), "\"",
                        sep = ""
                    )
                }
            }
        }
        # Finally, apply the value
        res@metadata$flags[[where]][[name]][i] <- value
    }
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste("setGliderFlags(object, name=\"", name, "\",",
            "i=c(", paste(head(i, collapse = ",")), "...),",
            "value=", valueOrig, ")",
            collapse = "", sep = ""
        )
    )
    gliderDebug(debug, "} # setGliderFlags\n", sep = "", unindent = 1)
    res
}

initializeGliderFlagScheme <- function(object, name = "IOOS", mapping = NULL, default = NULL, update = NULL, debug = 0) {
    gliderDebug(debug, "initializeGliderFlagScheme(object, name=\"", name, "\", debug=", debug, ") {", sep = "", unindent = 1)
    res <- object
    if (!is.null(object@metadata$flagScheme) && !(is.logical(update) && update)) {
        warning("cannot alter a flagScheme that is already is place")
    } else {
        predefined <- c("argo", "BODC", "DFO", "WHP bottle", "WHP CTD", "IOOS")
        if (name %in% predefined) {
            if (!is.null(mapping)) {
                stop("cannot redefine the mapping for existing scheme named \"", name, "\"")
            }
            if (name == "argo") {
                # The argo mapping and default were changed in June 2020,
                # to accomodate new understanding of argo flags, developed
                # by Jaimie Harbin for the argoCanada/argoFloats project.  See
                # https://github.com/ArgoCanada/argoFloats/issues/133
                # https://github.com/dankelley/oce/issues/1705
                mapping <- list(
                    not_assessed = 0,
                    passed_all_tests = 1,
                    probably_good = 2,
                    probably_bad = 3,
                    bad = 4,
                    changed = 5,
                    not_used_6 = 6,
                    not_used_7 = 7, # until 2020-jun-10, named 'averaged'
                    estimated = 8, # until 2020-jun-10, named 'interpolated'
                    missing = 9
                )
                if (is.null(default)) {
                    # until 2020-jun-10, next was more cautious, namely
                    # default <- c(0, 2, 3, 4, 7, 8, 9) # retain passed_all_tests
                    default <- c(0, 3, 4, 9)
                }
            } else if (name == "BODC") {
                mapping <- list(
                    no_quality_control = 0, good = 1, probably_good = 2,
                    probably_bad = 3, bad = 4, changed = 5, below_detection = 6,
                    in_excess = 7, interpolated = 8, missing = 9
                )
                if (is.null(default)) {
                    default <- c(0, 2, 3, 4, 5, 6, 7, 8, 9) # retain good
                }
            } else if (name == "DFO") {
                mapping <- list(
                    no_quality_control = 0, appears_correct = 1, appears_inconsistent = 2,
                    doubtful = 3, erroneous = 4, changed = 5,
                    qc_by_originator = 8, missing = 9
                )
                if (is.null(default)) {
                    default <- c(0, 2, 3, 4, 5, 8, 9) # retain appears_correct
                }
            } else if (name == "WHP bottle") {
                mapping <- list(
                    no_information = 1, no_problems_noted = 2, leaking = 3,
                    did_not_trip = 4, not_reported = 5, discrepency = 6,
                    unknown_problem = 7, did_not_trip = 8, no_sample = 9
                )
                if (is.null(default)) {
                    default <- c(1, 3, 4, 5, 6, 7, 8, 9) # retain no_problems_noted
                }
            } else if (name == "WHP CTD") {
                mapping <- list(
                    not_calibrated = 1, acceptable = 2, questionable = 3,
                    bad = 4, not_reported = 5, interpolated = 6,
                    despiked = 7, missing = 9
                )
                if (is.null(default)) {
                    default <- c(1, 3, 4, 5, 6, 7, 9) # retain acceptable
                }
            } else if (name == "IOOS") {
                # U.S. Integrated Ocean Observing System. "Manual for
                # Real-Time Oceanographic Data Quality Control Flags,
                # Version 1.2," 2020.
                # https://cdn.ioos.noaa.gov/media/2020/07/QARTOD-Data-Flags-Manual_version1.2final.pdf.
                mapping <- list(
                    good = 1, not_evaluated = 2, questionable = 3,
                    bad = 4, missing = 9
                )
                if (is.null(default)) {
                    default <- c(3, 4, 9) # retain acceptable
                }
            } else {
                stop("internal coding error in initializeGliderFlagSchemeInternal(); please report to developer")
            }
        } else {
            if (is.null(mapping)) {
                stop("must supply 'mapping' for new scheme named \"", name, "\"")
            }
        }
        res@metadata$flagScheme <- list(name = name, mapping = mapping, default = default)
    }
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste("initializeGliderFlagScheme(object, name=\"", name,
            "\", mapping=",
            gsub(" ", "", paste(as.character(deparse(mapping)),
                sep = "", collapse = ""
            )),
            ")",
            ", default=c(", paste(default, collapse = ","), "))",
            sep = ""
        )
    )
    gliderDebug(debug, "} # initializeGliderFlagScheme", sep = "", unindent = 1)
    res
}
