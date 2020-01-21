#' Get grouping info
#'
#' retrieve the hierarchical groupings for a getcbs object
#'
#' CBS fieldnames folow a certain hierarchical grouping order. For
#' example in the data about agriculture you find the groups farms and
#' animals. In each group there is the variable pigs_amount
#' (varkens_aantallen). In the farm group this is refering to the
#' amount of farms with pigs, in the animal group this is refering to
#' the amount of pigs. From the fieldname it's not clear to which
#' group the field belongs, This function get's this information.
#'
#' @return a vector with the hierarchical groups, starting with the
#' top group
#'
#' @param fieldn field name
#' @param cbsobj object which was returned by getcbs
#'
#' @examples
#' cbstoc <- getcbstoc()
#` landbouwtelling <- getcbs("80781ned","cbs_landbouwtelling")
#` fieldn <- "Leghennen_aantal.1"
#` getFieldParents(fieldn,landbouwtelling)
#'
#' @export
#'


getFieldParents <-
function(fieldn,cbsobj){
    if(!is.list(cbsobj)) {
        stop("cbsobj is not a list")
    }

    key <- getMetaDataPropField(fieldn,cbsobj)
    if(length(key)==1 && is.na(key)) {
        return(NA)
    }

    i <- key$ID
    crumble <- c(getMetaDataPropId(i,cbsobj)$Title)
    while(TRUE) {
        p <- getParent(i,cbsobj)
        if(is.na(p)) {
            break
        }
        crumble <- append(crumble,getMetaDataPropId(p,cbsobj)$Title)
        i <- p
    }
    crumble <- crumble[length(crumble):1]
    return(crumble)
}
