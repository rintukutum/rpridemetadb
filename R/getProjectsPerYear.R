#' @title Get yearwise projects available at PRIDE database
#' @description Get PRIDE projects information from PRIDE database based on year
#' @examples getProjectsPerYear(year = 2017)
#' @param year numeric
#' @export
getProjectsPerYear <- function(year='2017'){
  yearsAvail <- getArchiveYears()
  if(!(year %in% yearsAvail)){
    stop(paste0('PRIDE archive contains only these years : ',
                paste(yearsAvail,collapse = ', '))
         )
  }
  prideFTP <- 'ftp://ftp.pride.ebi.ac.uk/pride/data/archive/'
  uri <- paste0(prideFTP,year,'/')
  content <- getMeta(uri = uri)
  months <- sapply(content, getLast)
  months <- onlyNumeric(months)
  months.pride <- list()
  for(i in seq_along(months)){
    uri.month <- paste0(uri,months[i],'/')
    cat(paste0('Downloading from ', uri.month, '\t# project folders : '))
    months.prideIDs <- sapply(getMeta(uri = uri.month),
                           getLast)
    cat(paste0(length(months.prideIDs),'\n'))
    months.pride[[i]] <- list(month = months[i],
                         uri = uri.month,
                         pride.ids = as.character(months.prideIDs))

  }
  return(months.pride)
}
