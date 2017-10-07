getProjectMeta <- function(uri){
  # uri <- 'ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2017/01/PXD003701/'
  status.uri <- suppressWarnings(RCurl::url.exists(uri))
  if(status.uri){
    content <- getMeta(uri = uri)
    out <- as.character(sapply(content,rmUN))
    readme.avial <- any(grepl('README.txt',out))
    if(readme.avial){
      uri.readme <- paste0(uri,'README.txt')
      meta.files <- read.delim(uri.readme,sep='\t',stringsAsFactors = FALSE)
      size <- c()
      for(i in 1:nrow(meta.files)){
        meta.file <- meta.files$NAME[i]
        out.file <- out[grep(paste0(meta.file,'$'),
                         out)]
        size[i] <- as.numeric(getSize(out.file))/1e9

      }
      meta.df <- data.frame(meta.files,
                            size.gb = size,
                            stringsAsFactors = FALSE)
      meta.df <- meta.df[,c('ID','NAME','TYPE','MAPPINGS','size.gb')]
      meta <- list(uri = uri,
                   meta.df = meta.df)
      return(meta)
    }
  }else{
    meta.df <- matrix(data=NA,ncol = 5,nrow = 1)
    colnames(meta.df) <- c('ID','NAME','TYPE','MAPPINGS','size.gb')
    meta <- list(uri = uri,
                 meta.df = meta.df)
    return(meta)
  }
}

