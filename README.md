# **oceglider**

<!-- badges: start -->

[![R-CMD-check](https://github.com/dankelley/oce/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dankelley/oce/actions/workflows/R-CMD-check.yaml)
[![GitHub last commit](https://img.shields.io/github/last-commit/dankelley/oceglider)](https://img.shields.io/github/last-commit/dankelley/oceglider)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)


<!-- badges: end -->


`oceglider` is an R package for processing ocean glider data, with
support for Slocum and SeaExplorer devices.

Since `oceglider` is not on CRAN, it must be installed from source. An
easy way to do that is to use the `remotes` package to install it from
the github source. There are two main branches to consider. The one
called "develop" is in active development, and is likely the best one
to install if you want the latest features.  Use

    remotes::install_github("dankelley/oceglider", ref = "develop")

to install "develop".  You may also want to consider installing the
"main" branch, which is updated less frequently than "develop".  This
may be installed with

    remotes::install_github("dankelley/oceglider", ref = "main")

but note that it may lack some important new features (and bug fixes)
that are to be found in "develop".  The authors work with "develop"
from day to day, only merging to "main" occasionally.
