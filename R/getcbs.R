#' download data from CBS
#'
#' Download data and meta data from CBS 
#'
#' This function downloads the data and the meta data from a CBS
#' table and stores it to disk. It checks if the table exists using the TOC and then checks
#' if the file already exists. If not, the data is downloaded. If the
#' file is allready present, then the data is read from the file.
#' 
#' After downloading the fieldnames of the data are renamed and the
#' unit of the field is added to the fieldname. A translation table is
#' created with the original fieldname and the fieldname created by
#' this function.
#' 
#' CBS fieldnames folow a certain hierarchilcal grouping order. For
#' example in the data about agriculture you find the groups farms and
#' animals. In each group there is the variable pigs_amount
#' (varkens_aantallen). In the farm group this is refering to the
#' amount of farms with pigs, in the animal group this is refering to
#' the amount of pigs. From the fieldname it's not clear to which
#' group the field belongs, use the function \code{getFieldParents} to
#' get this information.
#' 
#' downloaded files are stored in a directory called ./data
#'
#' @return A list with several objects: data contains the data.frame
#' with data, meta contains the meta data, tocentry contains the meta
#' data from the TOC, keyName is the translation table, and fieldnames
#' is a table with filednames and grouping id's.
#'
#' @param id CBS identifier for the data, see Identifier field in TOC
#' @param name name of file to store CBS data
#' @param datadir directory to store data
#' @param toc name of cbstoc object
#'
#' @examples
#' 
#'
#' @export
#'



getcbs <-
function(id,fname,toc=cbstoc,base_url=NA) {
    if(!is.character(id)) {
        stop("id is not character")
    }
    
    if(!is.character(fname)) {
        stop("name is not character")
    }

    if(!is.list(toc)) {
        stop("toc is not a list")
    }


    tocentry <- toc %>% select(Identifier,ShortTitle) %>% filter(Identifier == id)
    if(!nrow(tocentry)) {
        stop("id not found in toc")
    }

        
    #fname <- paste(here(datadir),"/",name,".rds",sep="")
    cat("using file",fname,"\n")
    
    if(!file.exists(fname)) {


        # interessante tabel

        if(is.na(base_url)) {

            meta <- cbs_get_meta(id)
            dat <-  cbs_get_data(id) %>% cbs_add_label_columns() %>%
                cbs_add_date_column()
        } else {

            meta <- cbs_get_meta(id,base_url=base_url)
            dat <-  cbs_get_data(id,base_url=base_url) %>% cbs_add_label_columns() %>%
                cbs_add_date_column()
        }
    

        fieldnames.meta <- meta$DataProperties %>%
            select(Key,Title,Unit,ID,ParentID) %>%
            filter(!is.na(Unit)) 
        fieldnames.meta$Unit <- gsub(" ","",fieldnames.meta$Unit) 
        fieldnames.meta  <- mutate(fieldnames.meta,
                                   nameUnit=paste(sub("_[0-9]*","",Key),Unit,sep="_"))

        fieldnames.data <- data.frame(Key=names(dat),stringsAsFactors=FALSE)

        fieldnames <- full_join(fieldnames.meta,fieldnames.data,by="Key")
        fieldnames <- mutate(fieldnames,fieldname=ifelse(is.na(nameUnit),Key,nameUnit))

        namelst <- as.character(fieldnames.data$Key)
        for (i in 2:nrow(fieldnames)) {
            namelst[which(namelst==fieldnames$Key[i])] <- fieldnames$fieldname[i]
        }
        namelst <- make.names(namelst,unique=TRUE)
        keyName <- data.frame(Key=names(dat),fieldname=namelst,stringsAsFactors=FALSE)
        names(dat) <- namelst

        result <- list(data=dat,meta=meta,tocentry=tocentry,keyName=keyName,
                       fieldnames=fieldnames)
        saveRDS(result,fname)
    } else {
        result <- readRDS(fname)
    }

    return(result)
}
