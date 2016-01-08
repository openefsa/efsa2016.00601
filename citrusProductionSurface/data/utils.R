condDownload <- function(url,destFile) {
    if(!file.exists(destFile)) {
        download.file(url,destFile)
    }
}

readNutsLevels <- function() {
    nutsLevels <- read_csv("NUTS_2013L.csv")
    names(nutsLevels)[names(nutsLevels)=="NUTS-Code"] <- "NUTS.Code"
    nutsLevels <- nutsLevels %>% select(Level,NUTS.Code,Description)
    nutsLevels
}
