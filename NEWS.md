# oceglider 0.1.10

* Rename flag functions to avoid conflict with oce, e.g. the `oce`
functions named `setFlags()` and `handleFlags()` are replaced with
`setGliderFlags()` and `handleGliderFlags()` in `oceglider`.
* Correct the formula used to convert oxygen from ml/l to umol/kg,
used in `swOxygenFrequencyToSaturation()`.

# oceglider 0.1.9

* Change oxygen calibration.  I think the computation was wrong
before, but I've not checked and so there is no issue for this.

# oceglider 0.1.8

* Change `read.glider.slocum.netcdf()` to default the `debug`
parameter (issue #101).

# oceglider 0.1.7

* Change to `plot,glider-method()` fix map axis mix-up (issue #98).
* Make `[["z"]]` and `[["?"]]` work.

# oceglider 0.1.6

* Change `plot,glider-method()` by adding `colorbylim` argument.
* Require R version 4.1.0 or later (for pipes).

# oceglider 0.1.5

* Rename from `oceanglider` to `oceglider`.
* Add `plot,glider-method()`.

# oceglider 0.1.4

* Add `read.glider.slocum.netcdf()`
* Revamp README badges

# oceglider 0.1.3

* fix up some test-suite problems identified in today's R

# oceglider 0.1.2

* add `navStateCodes()`
* add `read.glider.netcdf()`
* add `read.seaexplorer.*()`
* rename `read.glider()` to `read.slocum()`

# oceglider 0.1.0

* crude code tested for some Dalhousie Slocum files.
