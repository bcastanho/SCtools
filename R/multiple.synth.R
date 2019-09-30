#' @title Function to Apply Synthetic Controls to Multiple Treated Units
#' @description 
#' @param Generates one synthetic control for each treated unit and calculates 
#'    the difference between the treated and the synthetic control for each. 
#'    Returns a vector with outcome values for the synthetic controls, 
#'    a plot of average treatment effects, and if required generates placebos 
#'    out of the donor pool to be used in conjunction with \link{plac.dist}. 
#'    All arguments are the same used for \link{dataprep} in the \link{Synth} 
#'    package, except for \link{treated.units}, \link{treatment.time}, and 
#'    \link{generate.placebos}.
#' @param foo Dataframe with the panel data.
#' @param predictors Vector of column numbers or column-name character strings 
#'     that identifies the predictors' columns. All predictors have to be numeric.
#' @param predictors.op A character string identifying the method (operator) 
#'    to be used on the predictors. Default is ``mean''.
#' @param dependent The column number or a string with the column name that 
#'    corresponds to the dependent variable.
#' @param unit.variable The column number or a string with the column name 
#'    that identifies unit numbers. The variable must be numeric.
#' @param time.variable The column number or a string with the column name 
#'    that identifies the period (time) data. The variable must be numeric.
#' @param special.predictors A list object identifying additional predictors and 
#'    their pre-treatment years and operators. 
#' @param treated.units A vector identifying the ``unit.variable'' numbers of 
#'    the treated units.
#' @param control.units A vector identifying the ``unit.variable'' numbers of 
#'    the control units.
#' @param time.predictors.prior A numeric vector identifying the pretreatment 
#'    periods over which the values for the outcome predictors should be averaged.
#' @param time.optimize.ssr A numeric vector identifying the periods of the 
#'    dependent variable over which the loss function should be minimized 
#'    between each treated unit and its synthetic control.
#' @param unit.names.variable The column number or string with column name 
#'    identifying the variable with units' names. The variable must be a character.
#' @param time.plot A vector identifying the periods over which results are 
#'    to be plotted with \link{path.plot}
#' @param treatment.time A numeric value with the value in ``time.variable'' 
#'    that marks the intervention.
#' @param generate.placebos Logical. Whether a placebo (a synthetic control) 
#'    for each unit in the donor pool should be constructed. Will increase 
#'    computation time.
#' @param Sigf.ipop The Precision setting for the ipop optimization routine. 
#'    Default of 5.
#' @details The function runs \link{dataprep} and \link{synth} for each unit 
#'    identified in ``treated.units''. It saves the vector with predicted values 
#'    for each synthetic control, to be used in estimating average treatment 
#'    effects in applications of Synthetic Controls for multiple treated units.
#'    
#'    For further details on the arguments, see the documentation of \link{Synth}.
#' @return Data frame. Each column contains the outcome values for every time-point for one unit or its synthetic control. The last column contains the time-points.
#'@export


multiple.synth<-function(foo,
                         predictors, 
                         predictors.op,
                         dependent,
                         unit.variable,
                         time.variable,
                         special.predictors,
                         treated.units,
                         control.units,
                         time.predictors.prior,
                         time.optimize.ssr,
                         unit.names.variable,
                         time.plot,
                         treatment.time,
                         generate.placebos=F, 
                         Sigf.ipop = 5){

  df<-data.frame(time.plot)
  
  for(i in 1:length(treated.units)){
    df[,i]<-syn(i)$a
    }
  for(i in 1:length(treated.units)){
    df[,i+length(treated.units)]<-foo[,dependent][foo[,unit.variable] == treated.units[[i]] & foo[,time.variable] %in% time.plot]
    }
  var<-NULL
  var1<-NULL
  for(i in 1:length(treated.units)){
    var[[i]]<-paste('control',i,sep='')
    }
  for(i in 1:length(treated.units)){
    var1[[i]]<-paste('treated',i,sep='')
    }
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
  for(i in 1:length(time.plot)) {
    b.path[i,2]<-mean(b$tr[b$year==years[[i]]])
    }
  for(i in 1:length(time.plot)) {
    b.path[i,3]<-mean(b$cont[b$year==years[[i]]])
    }
  colnames(b.path)<-c('year','tr','cont')
  
  p<-ggplot(data=b.path,aes(x = year, y=tr))+
    geom_line()+
    geom_line(aes(y=cont),linetype='dashed')+
    geom_vline(xintercept = treatment.time)+
    theme_bw()
  
  if(generate.placebos){
    out.temp<-syn(treated.units[[1]])
    tdf<-generate.placebos(dataprep.out=out.temp$dataprep.out,synth.out=out.temp$synth.out)
    df.plac<-data.frame(tdf$df)
    df.plac<-df.plac[,-c(ncol(df.plac)-1,ncol(df.plac)-2)]
    out2<-list(df=df,
               p=p,
               b=b,
               b.path=b.path,
               treatment.time=treatment.time,
               treated.units=treated.units,
               control.units=control.units,
               df.plac=df.plac) ## Return placebos without including the original treated observation
  } else{
    out2<-list(df = df, 
              p=p, 
              b = b,
              b.path=b.path,
              treatment.time=treatment.time,
              treated.units=treated.units,
              control.units=control.units)}
  return(out2)
}

