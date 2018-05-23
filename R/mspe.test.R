mspe.test <-
function(tdf,discard.extreme=FALSE,mspe.limit=20){
  t.mspe<-tdf$loss.v
  if(!discard.extreme) {
  n<-tdf$n
  pre<-subset(tdf$df,year < tdf$t1 & year >= tdf$t0)
  post<-subset(tdf$df,year >= tdf$t1)
  unit.names<-as.character(tdf$names.and.numbers$unit.names)
  mspe.placs<-tdf$mspe.placs
  } else {
    extremes<-which(tdf$mspe.placs/tdf$loss.v >= mspe.limit)
    tdf$df<-tdf$df[,-c(extremes,extremes+tdf$n)]
    pre<-subset(tdf$df,year < tdf$t1 & year >= tdf$t0)
    post<-subset(tdf$df,year >= tdf$t1)
    n<-tdf$n-length(extremes)
    unit.names<-as.character(tdf$names.and.numbers$unit.names[-extremes])
    mspe.placs<-data.frame(tdf$mspe.placs[-extremes,])
  }
  test<-data.frame(matrix(0,ncol=1,nrow=n))
  for(i in 1:n){test[i,1]<-mspe(post[,i],post[,i+n])/mspe.placs[i,]}
  test[n+1,1]<-mspe(post[,ncol(post)-2],post[,ncol(post)-1])/t.mspe
  test[1:n,2]<-unit.names
  test[nrow(test),2]<-tdf$treated.name
  colnames(test)<-c('MSPE.ratios','unit')
  p.val<-sum(test[1:nrow(test),1] >= test[nrow(test),1])/nrow(test)
  res.mspe<-list(p.val = p.val, test = test)
  return(res.mspe)
}
