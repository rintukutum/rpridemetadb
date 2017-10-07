getLast <- function(x){
  x <- strsplit(x,split = '\\s')[[1]]
  x <- as.character(x[length(x)])
  return(x)
}

onlyNumeric <- function(x){
  y <- x[grep('[[:digit:]]',x)]
  return(y)
}

getMeta <- function(uri){
  content <- RCurl::getURI(url = uri)
  content <- strsplit(content,split = '\n')[[1]]
  return(content)
}

rmUN <- function(x){
  y <- strsplit(x,split = 'ftp')[[1]][3]
  y <- gsub('\\s\\s',' ',y)
  y.out <- strsplit(y, split = '\\s')[[1]]
  pos <- grep('[[:digit:]]',y.out)[1]
  x <- paste(y.out[pos:length(y.out)],collapse = ' ')
  return(x)
}

getSize <- function(x){
  x <- strsplit(x,split = '\\s')[[1]][1]
  return(x)
}

checkValidPRIDuri <- function(uri){
  len <- length(strsplit(uri,split = '\\/')[[1]])
  if(len < 10){
    return(FALSE)
  }else{
    return(TRUE)
  }
}
