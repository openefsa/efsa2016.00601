condDownload <- function(url,destFile) {
    if(!file.exists(destFile)) {
        download.file(url,destFile)
    }
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


read_nuts3Change2013 <- function() {
    nuts3Change2013 <- read_excel(paste0(getwd(),"/geo/NUTS 2010 - NUTS 2013.xls"),4)
                                        #,col_types=rep("text",12),col_names=paste0("X",seq(1,12)))
    names(nuts3Change2013)[1:2]<-c("Code_2010","Code_2013")
    nuts3Change2013 <- nuts3Change2013 %>%
        select(Code_2010,Code_2013) %>%
        separate(Code_2013,into=c("Code_2013","change2010_2013_comment"),sep=" ",fill="right")
    
    nuts3Change2013
}

