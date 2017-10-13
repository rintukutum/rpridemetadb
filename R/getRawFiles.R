#' @title  check raw files
#' @description extract raw files from meta files of a PRIDE project
#' @param meta metafile downloaded from PRIDE using getProjectMeta function
#' @examples uri <- 'ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2017/01/PXD003701/'
#' pmeta <- getProjectMeta(uri)
#' checkRawFiles(pmeta)
#' @export
#'
checkRawFiles <- function(meta, size=TRUE){
  idx.raw <- meta$meta.df$TYPE %in% 'RAW'
  if(any(idx.raw)){
    raw.files <- meta$meta.df$NAME[idx.raw]
    if(size){
      size <- sum(meta$meta.df$size.gb[idx.raw])
      raw.files <- list(raw.files = raw.files,
                        total.size.gb = round(size,2))
    }
  }else{
    raw.files <- list(raw.files = NA,
                      total.size.gb = NA)
  }
  return(raw.files)
}

#' @title Get raw files
#' @description download raw files
#' @param meta metafile downloaded from PRIDE using getProjectMeta function
#' @export
#' @examples uri <- 'ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2017/01/PXD003701/'
#' pmeta <- getProjectMeta(uri)
#' # caution before downloading please check file size using checkRawFiles function
#' checkRawFiles(pmeta)
#' getRawFiles(meta = pmeta)
getRawFiles <- function(meta, path=NULL){
  if(is.null(path)){
    cat('using current directory to save files\n')
    path <- './'
  }
  idx.raw <- meta$meta.df$TYPE %in% 'RAW'
  if(any(idx.raw)){
    prideID <- strsplit(meta$uri,split = '\\/')[[1]][9]
    raw.files <- meta$meta.df$NAME[idx.raw]
    sizes <- meta$meta.df$size.gb[idx.raw]
    uri <- paste0(meta$uri,
                  gsub(' ','%20',raw.files))
    for(i in seq_along(uri)){
      path.now <- paste0(path,
                         prideID,
                         '/')
      dir.create(path = path.now)
      destFile <- paste0(path.now,
                         gsub(' ',
                              'xyzyx',
                              raw.files[i]
                              )
                         )
      download.file(url = uri[i],
                    destfile = destFile,
                    quiet = TRUE)
      if(grepl('xyzyx',destFile)){
        file.rename(destFile, gsub('xyzyx', ' ',destFile))
      }
    }
    cat('Downloading finish\n')
  }else{
    cat('Nothing to download!\n')
  }
}
