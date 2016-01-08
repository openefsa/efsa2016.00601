condDownload <- function(url,destFile) {
    if(!file.exists(destFile)) {
        download.file(url,destFile)
    }
}
