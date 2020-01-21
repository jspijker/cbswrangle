#' Get CBS table of contents
#'
#' Download the table of contents from the CBS API
#'
#' This function downloads the table of contents from the CBS website
#' and stores is into a file. If the file exists, the data is not
#' reloaded from the CBS, reducing network overhead.
#' 
#' downloaded files are stored in a directory called ./data
#'
#' @return A list with the CBS toc
#'
#' @param fname file name of the toc file
#'
#' @examples
#' cbstoc <- getcbstoc()
#'
#' @export
#'


getcbstoc <-
function(fname,base_url=NA){
    # filename van de cbs toc
    if(!file.exists(fname)){
        cat("download toc\n")
        cat("base url:",base_url,"\n")
        cat("filename:",fname,"\n")
        if(is.na(base_url)) {
            cbstoc <- cbs_get_toc(Language="nl") # Retrieve list of tables
        } else {
            cbstoc <- cbs_get_toc(Language="nl",base_url=base_url) # Retrieve list of tables
        }
        saveRDS(cbstoc,fname)
    } else {
        cbstoc <- readRDS(fname)
    }
    return(cbstoc)
}
