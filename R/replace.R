##' Modifies 'data' by adding new values supplied in newDataFileName
##'
##' newDataFileName is expected to have columns 
##' c(lookupVariable,lookupValue,newVariable,newValue,source)
##' 
##' Within the column 'newVariable', replace values that
##' match 'lookupValue' within column 'lookupVariable' with the value
##' newValue'.  If 'lookupVariable' is NA, then replace *all* elements
##' of 'newVariable' with the value 'newValue'.
##'
##' Note that lookupVariable can be the same as newVariable.
##'
##' @param newDataFileName name of lookup table
##' @param data existing data.frame
##' @return modified data.frame
addNewData <- function(data,newDataFileName){

    import <- readNewData(newDataFileName)
    if( !is.null(import)){    
        for(i in seq_len(nrow(import))){  #Make replacements
            col.to <- import$newVariable[i] 
            col.from <- import$lookupVariable[i]
            if(is.na(col.from)){ # apply to whole column
                data[col.to] <- import$newValue[i]
            } else { # apply to subset
                rows <- data[[col.from]] == import$lookupValue[i]
                data[rows,col.to] <- import$newValue[i]
            }
        }   
    }      
    data
}

##' Utility function to read/process newDataFileName for addNewData
##' 
##' @param newDataFileName name of lookup table
##' @return data.frame with columns c(lookupVariable,lookupValue,newVariable,newValue,source)
readNewData <- function(newDataFileName){
    
   
    import <- read.csv(newDataFileName, header=TRUE, stringsAsFactors=FALSE,
                       strip.white=TRUE,quote=NULL)
    
    if( nrow(import)> 0 ){
            
                                        #Check columns names for import are right
        expectedColumns<- c("lookupVariable","lookupValue","newVariable","newValue")
        nameIsOK <-  expectedColumns %in% names(import)
        if(any(!nameIsOK))
            stop("Incorrect name in lookup table for ",
                 newDataFileName, "--> ", paste(expectedColumns[!nameIsOK],
                                                collapse=", "))
            
                                        #Check values of newVariable are in list of allowed variables
        import$lookupVariable[import$lookupVariable == ""] <- NA
        
    } else {
        import <- NULL
    }


    import
}
