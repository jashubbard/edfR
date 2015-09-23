get_events <- function(filename,typelist=c('ENDFIX'),fields=c('sttime','entime','gavx','gavy')){

  tmp <- .Call('get_events',PACKAGE = 'edfR',filename,typelist,fields)

  colnames(tmp) <- fields
  as.data.frame(tmp)

}
