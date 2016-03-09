#' @export
createAllOutputs <- function(){
    rmarkdown::render(system.file("analysis/mapProposals.Rmd",package = "efsa2016.00601"),
                      rmarkdown::html_document(),output_dir = "/tmp")
    
}
