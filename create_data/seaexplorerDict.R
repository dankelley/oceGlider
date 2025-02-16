nameDictDefault <- data.frame(
    gliderName = c(
        "AROD_FT_DO", "AROD_FT_TEMP",
        "NAV_RESOURCE", "NAV_LONGITUDE", "NAV_LATITUDE",
        "NAV_DEPTH", "FLBBCD_CHL_COUNT", "FLBBCD_CHL_SCALED",
        "FLBBCD_BB_700_COUNT", "FLBBCD_BB_700_SCALED",
        "FLBBCD_CDOM_COUNT", "FLBBCD_CDOM_SCALED", "GPCTD_CONDUCTIVITY",
        "GPCTD_DOF", "GPCTD_PRESSURE", "GPCTD_TEMPERATURE",
        "PLD_REALTIMECLOCK"
    ),
    oceName = c(
        "oxygen", "oxygenTemperature",
        "navState", "longitude", "latitude", "pressureNav",
        "chlorophyllCount", "chlorophyll", "backscatterCount",
        "backscatter", "cdomCount", "cdom", "conductivity",
        "oxygenFrequency", "pressure", "temperature",
        "time"
    )
)
nameDictLegato <- data.frame(
    gliderName = paste0("LEGATO_", c(
        "CONDTEMP", "CONDUCTIVITY",
        "PRESSURE", "SALINITY", "TEMPERATURE"
    )),
    oceName = c(
        "conductivityTemperature", "conductivity",
        "pressure", "salinity", "temperature"
    )
)

# Next from seaexplorer files that CL sent me in early Feb, 2025
feb2025 <- "
gliderName,oceName
Altitude,altitude
AngCmd,angleCmd
AngPos,anglePos
BallastCmd,ballastCmd
BallastPos,ballastPos
DeadReckoning,deadReckoning
Declination,declination
Depth,depth
DesiredH,desiredHeading
Heading,heading
Lat,latitude
LinCmd,lineCmd
LinPos,linePos
Lon,longitude
NavState,navState
Pa,pressureAir
Pitch,pitch
Roll,roll
SecurityLevel,securityLevel
Temperature,temperature
Timestamp,timestamp
Voltage,voltage
X,X"

feb2025Dict <- read.csv(text = feb2025)

seaexplorerDict <- rbind(nameDictDefault, nameDictLegato, feb2025Dict)
#head(seaexplorerDict)
o <- order(seaexplorerDict$gliderName)
seaexplorerDict <- seaexplorerDict[o, ]
write.csv(seaexplorerDict, "seaexplorerDict.csv", row.names = FALSE)
