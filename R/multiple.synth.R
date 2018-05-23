multiple.synth<-function(foo,predictors,predictors.op,dependent,unit.variable,time.variable,
                         special.predictors,treated.units,control.units,
                         time.predictors.prior,time.optimize.ssr,unit.names.variable,time.plot,
                         treatment.time,
                         generate.placebos=F){
  require(Synth)
  require(ggplot2)
  syn<-function(i){
    dataprep.out<-dataprep(
      foo = foo,
      predictors = predictors,
      predictors.op = predictors.op,
      dependent = dependent,
      unit.variable = unit.variable,
      time.variable = time.variable,
      special.predictors = special.predictors,
      treatment.identifier = treated.units[[i]],
      controls.identifier = control.units,
      time.predictors.prior = time.predictors.prior,
      time.optimize.ssr = time.optimize.ssr,
      unit.names.variable = unit.names.variable,
      time.plot = time.plot)
    synth.out<-synth(dataprep.out)
    a<-data.frame(dataprep.out$Y0plot %*% synth.out$solution.w)
    colnames(a)<-paste('unit',i,sep='')
    out<-list(a = a, synth.out=synth.out, dataprep.out = dataprep.out)
    return(out)
  }
  df<-data.frame(time.plot)
  for(i in 1:length(treated.units)){df[,i]<-syn(i)$a}
  for(i in 1:length(treated.units)){df[,i+length(treated.units)]<-foo[,dependent][foo[,unit.variable] == treated.units[[i]] & foo[,time.variable] %in% time.plot]}
  var<-NULL
  var1<-NULL
  for(i in 1:length(treated.units)){var[[i]]<-paste('control',i,sep='')}
  for(i in 1:length(treated.units)){var1[[i]]<-paste('treated',i,sep='')}
  colnames(df)<-c(var,var1)
  df$year<-time.plot
  b<-NULL
  for(i in 1:length(treated.units)) {
    a<-cbind(df$year,df[,i],df[,length(treated.units)+i],i)
    b<-rbind(b,a)
  }
  b<-data.frame(b)
  colnames(b)<-c('year','cont','tr','id')
  b.path<-data.frame(matrix(time.plot,length(time.plot),1))
  years<-time.plot
  for(i in 1:length(time.plot)) {b.path[i,2]<-mean(b$tr[b$year==years[[i]]])}
  for(i in 1:length(time.plot)) {b.path[i,3]<-mean(b$cont[b$year==years[[i]]])}
  colnames(b.path)<-c('year','tr','cont')
  p<-ggplot(data=b.path,aes(x = year, y=tr))+geom_line()+geom_line(aes(y=cont),linetype='dashed')+geom_vline(xintercept = treatment.time)+
    theme_bw()
  if(generate.placebos){
    out.temp<-syn(treated.units[[1]])
    tdf<-generate.placebos(dataprep.out=out.temp$dataprep.out,synth.out=out.temp$synth.out)
    df.plac<-data.frame(tdf$df)
    df.plac<-df.plac[,-c(ncol(df.plac)-1,ncol(df.plac)-2)]
    out2<-list(df=df,p=p,b=b,b.path=b.path,treatment.time=treatment.time,treated.units=treated.units,
               control.units=control.units,df.plac=df.plac) ## Return placebos without including the original treated observation
  } else
  {out2<-list(df = df, p=p, b = b,b.path=b.path,treatment.time=treatment.time,treated.units=treated.units,
              control.units=control.units)}
  return(out2)
}

