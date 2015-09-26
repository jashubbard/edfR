blinkmask <- function(EDFfile)
{
  events <- edf.events(EDFfile,c('ENDBLINK','ENDSACC'),c('sttime','entime','type'))
  samp <- edf.samples(f)

  events <- events[order(events$entime),]

  isblink<- findRealBlinks(events$sttime,events$entime,events$type)

  blinksamp <- events2samples(isblink$sttime, isblink$entime, isblink$blink)
  blinksamp <- data.table(blinksamp)
  names(blinksamp) <- c('time','sttime','entime','blink')


  samp <- data.table(samp)
  samp <- setkey(samp)

  blinksamp <- data.table(blinksamp[c('time','blink')])
  blinksamp <- setkey(blinksamp)

  newsamp <- merge(samp,blinksamp,by='time',all.x=T)
  newsamp$blink[is.na(newsamp$blink)] <- 0

  return(newsamp)


}
