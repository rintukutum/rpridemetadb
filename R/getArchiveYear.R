getArchiveYears <- function(){
  prideFTP <- 'ftp://ftp.pride.ebi.ac.uk/pride/data/archive/'
  content <- getMeta(uri = prideFTP)
  years <- sapply(content, getLast)
  years <- onlyNumeric(years)
  return(years)
}

