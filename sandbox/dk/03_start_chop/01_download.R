n <- 5 # number of files to download
# Samples URLs:
#   https://cproof.uvic.ca/gliderdata/deployments/dfo-eva035/dfo-eva035-20231019/delayed_raw/sea035.118.gli.sub.1
#   https://cproof.uvic.ca/gliderdata/deployments/dfo-eva035/dfo-eva035-20231019/delayed_raw/sea035.118.gli.raw.1
urlbase <- "https://cproof.uvic.ca/gliderdata/deployments/dfo-eva035/dfo-eva035-20231019/delayed_raw/"
subbase <- "sea035.118.gli.sub."
rawbase <- "sea035.118.pld1.raw."
subs <- paste0(subbase, 1:n)
raws <- paste0(rawbase, 1:n)
print(data.frame(subs = subs, raws = raws))
for (i in 1:n) {
    if (!file.exists(subs[i])) {
        download.file(paste0(urlbase, subs[i]), subs[i])
    } else {
        cat("using cached file ", subs[i], "\n")
    }
    if (!file.exists(raws[i])) {
        download.file(paste0(urlbase, raws[i]), raws[i])
    } else {
        cat("using cached file ", raws[i], "\n")
    }
}
