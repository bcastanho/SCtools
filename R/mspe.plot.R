mspe.plot <-
function(tdf,
         discard.extreme=TRUE,
         mspe.limit=20, 
         plot.hist = FALSE,
         title=NULL,
         xlab='Post/Pre MSPE ratio',
         ylab=NULL) {
    data<-mspe.test(tdf,discard.extreme=discard.extreme,mspe.limit=mspe.limit)
    test<-data.frame(data$test)
    test$treat<-'control units'
    test[nrow(test),3]<-as.character(test[nrow(test),2])
    if(!plot.hist) {
    p.dot<-ggplot(data=test,aes(x = MSPE.ratios,
                                y=reorder(unit,MSPE.ratios,max),
                                colour=factor(treat),
                                shape=factor(treat)))+
      geom_point(size=4)+
      scale_color_manual(values = c('gray50','black'),
                         guide = FALSE)+
      scale_shape_manual(values = c(17,15),guide = FALSE) + 
      labs(y=ylab,
           x=xlab,
           title=title)+
      theme(panel.grid.major = element_line(colour = 'gray80'),
            panel.grid.minor=element_line(colour='gray90'),
            panel.background = element_blank(),
            axis.line.x = element_line(colour = 'black'),
            axis.line.y = element_line(colour = 'black'),
            axis.text.y=element_text(colour='black'),
            axis.text.x=element_text(colour='black')
            )
    return(p.dot)
  } else {
    p.dens<-ggplot(data=test,aes(x = MSPE.ratios,
                                 fill=factor(treat)))+
      geom_histogram()+
      scale_fill_manual(values = c('gray50','black'),
                        guide=guide_legend(title=NULL)) + 
      labs(y='Frequency',x=xlab,title=title)+
      theme(panel.grid.major = element_line(colour = 'gray80'),
            panel.grid.minor=element_line(colour='gray90'),
            panel.background = element_blank(),
            axis.line.x = element_line(colour = 'black'),
            axis.line.y = element_line(colour = 'black'),
            axis.text.y=element_text(colour='black'),
            axis.text.x=element_text(colour='black'),
            legend.position='bottom',
            legend.key=element_blank()
                           )
  return(p.dens)
  }
}
