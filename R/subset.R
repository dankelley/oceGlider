#' Subset a glider Object
#'
#' Select a portion of an glider object, specified according to one
#' of several possible schemes, based on the form of the argument named
#' `subset`. Note that the schemes cannot be combined, so
#' nested calls must be used to accomplish combinations.
#'
#' Scheme 1: if `subset` is a logical expression written
#' in terms of data that are stored in the yos, then this expression is applied
#' to the `payload1` item in the `data` slot of the object
#' (see Example 1).
#'
#' Scheme 2: if `subset` is a logical expression containing
#' the word `"yolength"`, then the expression is used as a filter
#' to select yos based on the number of samples they
#' contain (see Example 2). Typically, this might be used to avoid
#' very short yos that might have been inferred erroneously
#' by the glider instrumentation.
#'
#' Scheme 3: If `subset` is the string `"ascending"`, then
#' only ascending segments of yos are retained. This is done
#' by selecting for `navState==117` in both the `glider`
#' and `payload1` streams of the `data` slot of the object.
#'
#' Scheme 4: If `subset` is the string `"descending"`, then
#' only descending segments of yos are retained. This is done
#' by selecting for `navState==100` in both the `glider`
#' and `payload1` streams of the `data` slot of the object.
#'
#' @param x an oceglider object, i.e. one inheriting from the [glider-class].
#'
#' @param subset a logical expression or a character string that indicates
#' how to take the subset. See \dQuote{Details}.
#'
#' @param ... Additional arguments, of which the only one permitted at the
#' moment is `debug`, an integer indicating the level of debugging information
#' to be permitted.
#'
#' @return An oceglider object.
#'
#' @examples
#' \dontrun{
#'
#' # Example 1. remove wild salinities
#' library(oceglider)
#' g <- read.glider(filename)
#' gg <- subset(g, 0 < salinity & salinity < 40)
#' par(mfrow = c(2, 1))
#' hist(g[["salinity"]], main = "S original")
#' hist(gg[["salinity"]], main = "S cleaned")
#'
#' # Example 2. remove short yos
#' gg <- subset(g, yolength > 4)
#'
#' # Example 3. retain only ascending portions of yos
#' gascending <- subset(g, "ascending")
#'
#' # Example 4. retain only descending portions of yos
#' gdescending <- subset(g, "descending")
#' }
#'
#' @author Dan Kelley
#'
#' @export
#'
#' @aliases subset,glider-method
#'
#' @section Bugs:
#' The 'ascending' and 'descending' methods do not work. This seems
#' to be a problem of exporting classes using roxygen2 tags. I am looking
#' into this.  DK 2019-03-28.
#'
#' @md
setMethod(
    f = "subset",
    signature = "glider",
    definition = function(x, subset, ...) {
        if (missing(subset)) {
            stop("must give 'subset'")
        }
        dots <- list(...)
        debug <- if ("debug" %in% names(dots)) dots$debug else getOption("gliderDebug", 0)
        gliderDebug(debug, "subset,glider-method() START\n", unindent = 1)
        dataAreStreamed <- x@metadata$dataAreStreamed
        if (is.null(dataAreStreamed)) {
            stop("the object does not have an entry in @metadata$dataAreStreamed; please report an issue")
        }
        gliderDebug(debug, "dataAreStreamed: ", dataAreStreamed, "\n")
        subsetString <- paste(deparse(substitute(subset)), collapse = " ")
        gliderDebug(debug, "subsetString: \"", subsetString, "\"\n", sep = "")
        xtype <- x@metadata$type # direct lookup, to guard against 'type' being in data
        gliderDebug(debug, "object type: \"", xtype, "\"\n", sep = "")
        if (is.character(substitute(subset))) {
            gliderDebug(debug, "handling a character-valued subset\n")
            # subset is a character string
            if (subset %in% c("ascending", "descending")) {
                if (xtype == "seaexplorer") {
                    res <- x
                    if (dataAreStreamed) {
                        keep <- res@data$glider$navState == if (subset == "ascending") 117 else 100
                        for (i in seq_along(x@metadata$flags[["payload1"]])) {
                            res@metadata$flags[["payload1"]][[i]] <- res@metadata$flag[["payload1"]][[i]][keep]
                        }
                        res@data$glider <- subset(res@data$glider, keep)
                        res@data$payload1 <- subset(res@data$payload1, keep)
                    } else {
                        keep <- res@data$navState == 117
                        for (i in seq_along(x@metadata$flags[["payload1"]])) {
                            res@metadata$flags[[i]] <- res@metadata$flag[[i]][keep]
                        }
                        res@data <- res@data[keep, ]
                    }
                } else {
                    stop("subset to 'ascending' and 'descending' only work for seaexplorer objects (please report)")
                }
            }
        } else {
            gliderDebug(debug, "subset is not a character value\n")
            if (1 == length(grep("yolength", subsetString))) {
                gliderDebug(debug, "subset relates to yolength\n")
                if (xtype != "seaexplorer") {
                    stop("subset by 'yolength' only works for seaexplorer objects (please report)")
                }
                if (!"payload1" %in% names(x@data)) {
                    stop("In subset,glider-method() : only works for 'raw' datasets,",
                        " not for 'sub' ones; contact package authors,",
                        " if you need to handle sub data",
                        call. = FALSE
                    )
                }
                s <- split(x@data$payload1, x[["yoNumber"]])
                # warning removed for issue (https://github.com/dankelley/oceglider/issues/41)
                thisYolength <- as.integer(lapply(s, function(ss) length(ss[["pressure"]])))
                keepYo <- eval(substitute(subset), list(yolength = thisYolength))
                # message("sum(keepYo)=", sum(keepYo), " length(keepYo)=", length(keepYo))
                res <- x
                # 44 https://github.com/dankelley/oceglider/issues/44
                # 44 res@metadata$yo <- x@metadata$yo[keepYo]
                keepData <- unlist(lapply(seq_along(s), function(si) rep(keepYo[si], thisYolength[si])))
                # NOTE: the following was a much slower (10s of seconds compared to perhaps 1s or less)
                # res@data$payload <- do.call(rbind.data.frame, x@data$payload[keepYo, ])
                res@data$glider <- x@data$glider[keepData, ]
                res@data$payload1 <- x@data$payload1[keepData, ]
                for (i in seq_along(x@metadata$flags[["payload1"]])) {
                    res@metadata$flags[["payload1"]][[i]] <- res@metadata$flag[["payload1"]][[i]][keepData]
                }
            } else {
                gliderDebug(debug, "subset does not relate to yolength\n")
                if (xtype == "seaexplorer") {
                    if (dataAreStreamed) {
                        keep <- eval(substitute(subset), x@data[["payload1"]], parent.frame())
                    } else {
                        keep <- eval(substitute(subset), x@data, parent.frame())
                    }
                    keep[is.na(keep)] <- FALSE
                    res <- x
                    if (dataAreStreamed) {
                        gliderDebug(debug, "keeping", sum(keep), "of", length(keep), "elements (streamed-data case)\n")
                        res@data[["payload1"]] <- x@data[["payload1"]][keep, ]
                        for (i in seq_along(x@metadata$flags[["payload1"]])) {
                            res@metadata$flags[["payload1"]][[i]] <- res@metadata$flag[["payload1"]][[i]][keep]
                        }
                    } else {
                        gliderDebug(debug, "keeping", sum(keep), "of", length(keep), "elements (non streamed-data case)\n")
                        res@data <- x@data[keep, ]
                        for (i in seq_along(x@metadata$flags[["payload1"]])) {
                            res@metadata$flags[[i]] <- res@metadata$flag[[i]][keep]
                        }
                    }
                } else {
                    where <- "payload1"
                    gliderDebug(debug, "doing subset with respect to @data$", where, "\n", sep = "")
                    keep <- eval(substitute(subset), x@data[[where]], parent.frame())
                    keep[is.na(keep)] <- FALSE
                    gliderDebug(debug, "keeping", sum(keep), "of", length(keep), "elements\n")
                    res <- x
                    res@data[[where]] <- res@data[[where]][keep, ]
                }
            }
        }
        res@processingLog <- processingLogAppend(
            res@processingLog,
            paste(deparse(match.call(call = sys.call(sys.parent(1)))),
                sep = "", collapse = ""
            )
        )
        gliderDebug(debug, "subset,glider-method() END\n", sep = "", unindent = 1)
        res
    }
)
