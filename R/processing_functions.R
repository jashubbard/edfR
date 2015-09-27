blinkmask <- function(EDFfile)
{
  events <- edf.events(EDFfile,c('ENDBLINK','ENDSACC','ENDFIX'),c('sttime','entime','type'))


  events <- events[order(events$entime),]
  sacc_and_blinks <- subset(events,type!=8)
  fixes <- subset(events,type==8)

  isblink<- findRealBlinks(events$sttime,events$entime,events$type)

  blinksamp <- events2samples(isblink$sttime, isblink$entime, isblink$blink)
  colnames(blinksamp) <- c('time','sttime','entime','blink')
  blinksamp <- data.table(blinksamp,key='time')
  blinksamp <- subset(blinksamp,select=c('time','blink'))



  fixsamp <- events2samples(fixes$sttime, fixes$entime, fixes$type)
  colnames(fixsamp) <- c('time','sttime','entime','fix')
  fixsamp <- data.table(fixsamp,key='time')
  fixsamp <- subset(fixsamp,select=c('time','fix'))
  fixsamp[fixsamp$fix==8] <- 1


  samp <- edf.samples(f)
  samp <- data.table(samp,key='time')

  newsamp <- merge(samp,blinksamp,by='time',all.x=T)
  newsamp <- merge(newsamp,fixsamp,by='time',all.x=T)
  newsamp$blink[is.na(newsamp$blink)] <- 0
  newsamp$fix[is.na(newsamp$fix)] <- 0

  return(newsamp)


}
