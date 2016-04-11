makeDataObject <- function(filename,format="text/csv") {
    id <-  paste0("urn:uuid:", uuid::UUIDgenerate())
    do <- new("DataObject",id,format=format,filename=filename)
    do
    
}

makeDp <- function() {
    countries <- c("croatia","cyprus","france","greece","italy","portugal","spain")
    extensions <- c("csv","xls","xlsx")
    dp <- new("DataPackage")
    citrusDo <- makeDataObject(format="application/x-R-data",filename="data/euCitrusSurface.rda")
    datapackage::addData(dp, citrusDo)

    euSurfaceData <- makeDataObject(filename="analysis/output/euCitrusSurfaceData.csv")
    datapackage::addData(dp,euSurfaceData)
    datapackage::recordDerivation(dp,
                     
                                  citrusDo@sysmeta@identifier,
                                  euSurfaceData@sysmeta@identifier
                                  )

    for (extension in extensions) {
        for (country in countries) {
            files <- dir(system.file(paste0("extdata/",country),
                                    package =  "efsa2016.00601"),
                        recursive = T,
                        pattern = paste0(".",extension),
                        full.names = T)
            for (file in files) {
                                        #print(file)
                do <- makeDataObject(format=mime::guess_type(file), filename=file)
                datapackage::addData(dp,do)
                datapackage::recordDerivation(dp,euSurfaceData@sysmeta@identifier,
                                              do@sysmeta@identifier)
            }
        }


    }

   
    ascoSporeReleases <- makeDataObject(filename="./inst/extdata/asco_3_15.csv")
    datapackage::addData(dp,ascoSporeReleases)

    magTable1 <- makeDataObject(filename="./inst/extdata/mag2015_table1.csv")
    datapackage::addData(dp,magTable1)

    rainMagarey <- makeDataObject(filename="./analysis/output/rainForMagereyPoints.csv")
    datapackage::addData(dp,rainMagarey)

    datapackage::recordDerivation(dp,rainMagarey@sysmeta@identifier,
                                  c(ascoSporeReleases@sysmeta@identifier,
                                    magTable1@sysmeta@identifier))

    
    
    before <- getwd()
                                        #datapackage::serializeToBagIt(dp)
    setwd(before)
    dp
    
}


plotRelations <- function(dp) {

    identifiers <- dplyr::data_frame(identifier=datapackage::getIdentifiers(dp));
    identifiers$fileName <- sapply(identifiers$identifier,function(x) datapackage::getMember(dp,x)@sysmeta@fileName,USE.NAMES = F)

    
    
    
    rels <- datapackage::getRelationships(dp) %>% tbl_df() %>%
        left_join(identifiers,by=c("subject"="identifier")) %>%
        left_join(identifiers,by=c("object"="identifier")) %>%
        select(fileName.x,fileName.y) %>%
        rename(subject=fileName.x,
               object=fileName.y)
        
    
    net <- network::network(as.matrix(rels))
    plot(net,displaylabels=T)
}
