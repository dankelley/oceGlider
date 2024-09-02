# Sea-Explorer, realtime binary
# Downloaded 2024-09-02
file <- "dfo-eva035-20190718_delayed.nc"
url <- paste0(
    "https://cproof.uvic.ca/gliderdata/deployments/",
    "dfo-eva035/dfo-eva035-20190718/",
    "/L0-timeseries/"
)
try(download.file(paste0(url, file), file))
