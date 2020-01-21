getMetaDataPropField <-
function(name,cbsobj) {
    if(!is.list(cbsobj)) {
        stop("cbsobj is not a list")
    }

    fid <- cbsobj$keyName %>% filter(fieldname==name)
    if(nrow(fid)!=1) {
        return(NA)
    }
    i <- cbsobj$meta$DataProperties %>% filter(Key==fid$Key)
    return(i)
}
