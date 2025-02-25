% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve.R
\name{[[,glider-method}
\alias{[[,glider-method}
\title{Retrieve Part of a glider Object}
\usage{
\S4method{[[}{glider}(x, i, j, ...)
}
\arguments{
\item{x}{a glider object, i.e. one inheriting from \linkS4class{glider}.}

\item{i}{a character value that names the item to be retrieved.}

\item{j}{an optional character value specifying the data-stream to be used.}

\item{...}{optional additional information (ignored).}
}
\description{
Retrieve something contained in a glider object, or something that can
be computed from what is contained there.
}
\details{
First, a check is done to see if the object's metadata contains an item
with name given by \code{i}. If this is true, then that value is returned.

Otherwise, the item is sought somewhere within the \code{data} slot.
This may be done within \code{data} directly for Slocum
data or within \code{data$payload} or a similar name for SeaExplorer
data.  Note that \code{[[} can retrieve not just the actual stored
information, but also information that can be derived from that information.
For example, gliders do not store Absolute Salinity or Conservative
Temperature, but they can be retrieved with \code{g[["SA"]]} and \code{g[["CT"]]}
respectively, where \code{g} is a glider object (see Example 1).  Use e.g. \code{g[["?"]]} on
a given glider object, to find out what items \code{[[} can retrieve
(see Example 2).
}
\examples{
library(oceglider)
directory <- system.file("extdata/sea_explorer/delayed_raw", package = "oceglider")
g <- read.glider.seaexplorer.raw(directory, pattern = "pld1.raw")

# Example 1: look up Absolute Salinity
summary(g[["SA"]])

# Example 2: discover what can be retrieved from this object
# (FIXME: not all of these work)
g[["?"]]

}
\author{
Dan Kelley
}
