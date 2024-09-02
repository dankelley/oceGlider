# Sea-Explorer, delayed mode
# downloaded 2024-09-02
n <- 6 # number of files to download
urlRaw <- paste0(
    "https://cproof.uvic.ca/gliderdata/deployments/",
    "dfo-eva035/dfo-eva035-20190718/",
    "delayed_raw/"
)
subs <- paste0("sea035.12.gli.sub.", 1:n)
raws <- paste0("sea035.12.pld1.raw.", 1:n)
for (i in 1:n) {
    if (!file.exists(subs[i])) {
        download.file(paste0(urlRaw, subs[i]), subs[i])
    }
    if (!file.exists(raws[i])) {
        download.file(paste0(urlRaw, raws[i]), raws[i])
    }
}
