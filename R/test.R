# #
# # samp <- edf.samples(f)
# # samp <- data.table(samp)
# # samp <- setkey(samp,'time')
# #
# msg <- mes
#
#
#  gsub('[A-Z!@$%^&()?*+=,./~` \n]','',msg$message);
#
#
#  unique(msg$message[grepl('^TRIAL.*[0-9]',msg$message)])
#  unique(msg$message[grepl('^BLOCK.[0-9]',msg$message)])
#
#  temp1 <- gsub('[A-Z \\n]+','',msg$message)
#
#
#  msg$numbers <- as.numeric(regmatches(msg$message, temp1))
#
#  event <- 'STIMONSET'
#  epoch = c(-100,300)
#
#  epoch_width=(epoch[2] - epoch[1])
#
#  timelock <- msg$sttime[grepl(event,msg$message)]
#
#  epoch_st <- timelock+epoch[1]
#  epoch_en <- timelock+epoch[2]-1
#  epoch_num <- 1:length(epoch_st)
#
#  if(any((epoch_st - lag(epoch_en,1))<0,na.rm=T))
#    stop('At least some epochs are overlapping. Try a smaller epoch')
#
#
#  test <- as.data.frame(events2samples(epoch_st,epoch_en,epoch_num))
#  names(test) <- c('time','st','en','epoch')
#  test <- subset(test,select=c('time','epoch'))
#  test$time <- as.integer(test$time)
#
#  test2 <- merge(samp,test,all.y=T)
#  test2 <- subset(test2,!is.na(epoch))
#  tmp <- subset(test2,select=c('time','paL','epoch'))
#
#  check <- test %>% group_by(epoch) %>% summarise(n=n())
#
#
# #  tmp2 <- split(tmp$paL,tmp$epoch)
# #
#  tmp2 <- lapply(tmp2,function(x) matrix(x,nrow=1,ncol=epoch_width))
#
#
#  tmp3 <- do.call(rbind,tmp2)
#
#
#
#
