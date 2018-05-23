plot.placebos <-
function(tdf=tdf,discard.extreme=FALSE,mspe.limit=20,xlab = NULL,ylab = NULL,title=NULL,
         alpha.placebos=1, ...){
n<-tdf$n
t1<-tdf$t1
tr<-tdf$tr
names.and.numbers<-tdf$names.and.numbers
treated.name<-as.character(tdf$treated.name)
df.plot<-NULL
for(i in 1:n){
  a<-cbind(tdf$df$year,tdf$df[,i],tdf$df[,n+i],i)
  df.plot<-rbind(df.plot,a)
}
df.plot<-data.frame(df.plot)
colnames(df.plot)<-c('year','cont','tr','id')
if(discard.extreme) {
  df.plot<-df.plot[ ! df.plot$id %in% which(tdf$mspe.placs/tdf$loss.v >= mspe.limit),] 
}
else {df.plot<-df.plot}
p.gaps<-ggplot(data=data.frame(df.plot),aes(x=year,y=(tr-cont)))+geom_line(aes(group=id,color='2'))+geom_vline(xintercept = t1,linetype='dotted')+
    geom_hline(yintercept = 0, linetype = 'dashed')+ geom_line(data=data.frame(tdf$df),aes(x = year, y=(Y1-synthetic.Y1),color='1'),alpha=alpha.placebos)+ ylim(c(1.5*min(c(min(tdf$df$Y1 - tdf$df$synthetic.Y1),min(df.plot$tr-df.plot$cont))),1.5*max(c(max(tdf$df$Y1-tdf$df$synthetic.Y1),max(df.plot$tr-df.plot$cont))))) +
  labs(y=ylab,x=xlab,title=title)+scale_color_manual(values = c('2' = 'gray80', '1' = 'black'),labels = c(tdf$treated.name, 'Control units'), guide = guide_legend(NULL))+
    theme(panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),axis.line.x = element_line(colour = 'black'),axis.line.y = element_line(colour = 'black'),legend.key = element_blank(),
          axis.text.x = element_text(colour = 'black'),axis.text.y = element_text(colour = 'black'),legend.position='bottom'
              )
  return(p.gaps)
}
