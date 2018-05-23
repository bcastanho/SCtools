plac.dist<-function(multiple.synth,nboots){
  post.treat.t<-subset(multiple.synth$b.path, year > multiple.synth$treatment.time)
  require(Hmisc)
  att.t<-mean(post.treat.t$tr)-mean(post.treat.t$cont)
  df.cont<-multiple.synth$df.plac[multiple.synth$df.plac$year > multiple.synth$treatment.time,]
  storage.matrix<-matrix(NA,ncol=1,nrow=nboots)
  for(i in 1:nboots){
    cs<-sample(c(1:length(multiple.synth$control.units)),length(multiple.synth$treated.units),rep=F)
    df.cont.temp<-df.cont[,c(c(cs),c(cs+length(multiple.synth$control.units)))]
    storage.vector<-matrix(NA,ncol=1,nrow=length(cs))
    for(j in 1:length(storage.vector)){
      m<-mean(df.cont.temp[,j])-mean(df.cont.temp[,j+length(cs)])
      storage.vector[j,1]<-m
    }
    storage.matrix[i,1]<-mean(storage.vector)
  }
  storage.matrix<-data.frame(storage.matrix)
  colnames(storage.matrix)<-'atts'
  p<-ggplot(data=storage.matrix,aes(x = atts))+geom_histogram()+geom_vline(xintercept=att.t,linetype='dotted',size=2)+
    theme_bw()
    out<-list(p = p, att.t = att.t, df = storage.matrix)
  return(out)
}
