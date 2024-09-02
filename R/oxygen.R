#' Convert SBE Oxygen Frequency Value to Oxygen Saturation
#'
#' The calibration formula comes from SBE calibration sheets. This
#' code is based on `sbeO2Hz2Sat()` in
#' \url{https://github.com/DFOglider/pilotingApp}, with minor
#' modifications: (1) it has a new name to avoid conflicts, (2) the
#' calibration coefficients are collected into a single parameter,
#' `cal`, (3) `oxygenFrequency` is renamed `frequency`, (3) `unit`
#' is added, to permit specifying the unit of the return value,
#' and (5) the conversion to umol/kg takes into account the density,
#' as indicated in Reference 1.
#'
#' @param temperature numeric value holding temperature in degrees C.
#'
#' @param salinity numeric value holding Practical Salinity.
#'
#' @param pressure numeric value holding pressure in dbar.
#'
#' @param frequency the frequency reading from the oxygen sensor
#' system, in Hz.
#'
#' @param cal list or data frame holding calibration parameters named
#' `Soc`, `Foffset`, `A`, `B`, `C`, `D` and `Enom`.  The last of these
#' is called "E nominal" in the calibration sheets.
#'
#' @param unit character value indicating the desired unit. The
#' default is `ml/l` but `umol/kg` is also accepted, the latter being
#' computed by multiplying the former by `44.6591*1000/(1000+sigma0)`,
#' where `sigma0` is seawater potential density anomaly in kg/m^3
#' (see Reference 1, which uses the mole fraction stated in Reference 2).
#'
#' @references
#'
#' 1. Thierry, Virginie, Henry Bittig, Denis Gilbert, Taiyo Kobayashi,
#' Sato Kanako, and Claudia Schmid. "Processing Argo Oxygen Data at the
#' DAC Level (version 2.3.3)". Ifremer, 2022.
#' \url{https://doi.org/10.13155/39795}.
#'
#'
#' 2. Garcia, Herncin E., and Louis I. Gordon. "Oxygen Solubility in
#' Seawater: Better Fitting Equations." Limnology and Oceanography 37,
#' no. 6 (September 1992): 1307–12.
#' `https://doi.org/10.4319/lo.1992.37.6.1307`.
## the URL above works in my browser but fails for R.
#'
#' @author Chantelle Layton and Dan Kelley
#'
#' @export
swOxygenFrequencyToSaturation <- function(temperature,
                                          salinity,
                                          pressure,
                                          frequency,
                                          cal,
                                          unit = "ml/l") {
    Tk <- 273.15 + temperature * 1.0002 # convert temperature to units kelvin
    mll <- cal$Soc * (frequency + cal$Foffset) *
        (1.0 + cal$A * temperature + cal$B * temperature^2 + cal$C * temperature^3) *
        swOxygenSolubility(temperature = temperature, salinity = salinity) * exp(cal$Enom * pressure / Tk)
    if (identical(unit, "ml/l")) {
        mll
    } else if (identical(unit, "umol/kg")) {
        sigma0 <- oce::swSigma0(
            salinity = salinity,
            temperature = temperature, pressure = pressure, eos = "unesco"
        )
        44.6591 * mll * 1000 / (1000 + sigma0)
    } else {
        stop("unit must be 'ml/l' or 'umol/kg', but it is '", unit, "'")
    }
}

#' Compute Oxygen Solubility of Seawater
#'
#' This function is a copy of Chantelle Layton's `swOxygen()` function,
#' from the \url{https://github.com/DFOglider/pilotingApp}
#' repository, renamed to avoid name-resolution conflicts.
#'
#' @param temperature numeric value holding temperature in degrees C.
#'
#' @param salinity numeric value holding Practical Salinity.
#'
#' @author Chantelle Layton
#'
#' @export
swOxygenSolubility <- function(temperature, salinity) {
    Tk <- 273.15 + temperature * 1.00024
    # constants for Eqn (4) of Weiss 1970
    a1 <- -173.4292
    a2 <- 249.6339
    a3 <- 143.3483
    a4 <- -21.8492
    b1 <- -0.033096
    b2 <- 0.014259
    b3 <- -0.0017000
    exp(a1 +
        a2 * (100 / Tk) +
        a3 * log(Tk / 100) +
        a4 * (Tk / 100) +
        salinity * (b1 + b2 * (Tk / 100) + b3 * ((Tk / 100)^2)))
}

# ___ temperature <- c(2, 26)
# ___ salinity <- 0
# ___ pressure <- 0
# ___ oxygenFrequency <- c(1256.172, 5005.613)
# ___
# ___ # calibration data for sensor 3336 (in sea021/M71) dated Feb 1, 2022
# ___ cc <- list(
# ___     Soc = 3.0757e-4,
# ___     Soc = 3.0667e-4,
# ___     Foffset = -878.86,
# ___     Tau20 = 1.26,
# ___     A = -4.6593e-3, B = 1.7954e-4, C = -2.2483e-6, Enom = 0.036
# ___ )
# ___
# ___ O2 <- swOxygenFrequencyToSaturation(temperature, salinity, pressure, oxygenFrequency, cc)
# ___ O2expected <- c(1.11, 6.90)
# ___ library(testthat)
# ___ expect_equal(O2, O2expected, tolerance = 0.005) # results in calibration sheet to 2 digits (rounded? truncated?)
# ___ O2mr <- sbeO2Hz2Sat(temperature, salinity, pressure, oxygenFrequency, cc, unit = "ug/kg")
# ___ print(data.frame(actual = O2expected, computed = O2, computed_mass_ratio = O2mr))
