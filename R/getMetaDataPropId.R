getMetaDataPropId <-
function(id,cbsobj) {
    if(!is.list(cbsobj)) {
        stop("cbsobj is not a list")
    }

    fid <- cbsobj$meta$DataProperties %>% filter(ID==id)
    if(nrow(fid)!=1) {
        return(NA)
    }
    return(fid)
}
