getParent <-
function(i,cbsobj) {

    if(!is.list(cbsobj)) {
        stop("cbsobj is not a list")
    }
    p <- cbsobj$meta$DataProperties %>% filter(ID==i) %>%
        select(ParentID)
    if(nrow(p)==0) p <- NA
    return(as.numeric(p))
}
