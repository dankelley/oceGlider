% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base.R, R/netcdf.R
\docType{class}
\name{glider-class}
\alias{glider-class}
\alias{glider}
\title{A class to hold glider information}
\description{
As of 2024-06-23, it is not clear whether the best policy is for
this to inherit from 'oce' or to define itself here.  Doing the
former might seem sensible, since it might reduce the need for code
here, but it seems to cause problems (e.g. inability to build
vignettes in the standard way) that I have trouble understanding.
We ran into similar problems in the argoFloats package too, and
there we decided to define the object fully in the new package. For
now, that's the approach used here also.

As of 2024-06-23, it is not clear whether the best policy is for
this to inherit from 'oce' or to define itself here.  Doing the
former might seem sensible, since it might reduce the need for code
here, but it seems to cause problems (e.g. inability to build
vignettes in the standard way) that I have trouble understanding.
We ran into similar problems in the argoFloats package too, and
there we decided to define the object fully in the new package. For
now, that's the approach used here also.
}
\author{
Dan Kelley
}
