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
makeColNamesUserFriendly <- function(ds) {
                                        # FIXME: Repetitive.

                                        # Convert any number of consecutive dots to a single space.
    names(ds) <- gsub(x = names(ds),
                      pattern = "(\\.)+",
                      replacement = " ")

    names(ds) <- gsub(x = names(ds),
                      pattern = "(/)+",
                      replacement = "-")

                                        # Drop the trailing spaces.
    names(ds) <- gsub(x = names(ds),
                      pattern = "( )+",
                      replacement = "_")
    ds
}

iso88591Locale <- function() {
    locale("pt",encoding="ISO-8859-1")
    
}
