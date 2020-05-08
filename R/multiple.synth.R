#' @title Function to Apply Synthetic Controls to Multiple Treated Units
#' @description Generates one synthetic control for each treated unit and calculates 
#'    the difference between the treated and the synthetic control for each. 
#'    Returns a vector with outcome values for the synthetic controls, 
#'    a plot of average treatment effects, and if required generates placebos 
#'    out of the donor pool to be used in conjunction with \code{\link{plac.dist}}. 
#'    All arguments are the same used for \code{\link[Synth]{dataprep}} in the \code{\link[Synth:synth]{Synth}}
#'    package, except for \code{treated.units}, \code{treatment.time}, and 
#'    \code{\link{generate.placebos}}.
#' @param foo Dataframe with the panel data.
#' @param predictors Vector of column numbers or column-name character strings 
#'     that identifies the predictors' columns. All predictors have to be numeric.
#' @param predictors.op A character string identifying the method (operator) 
#'    to be used on the predictors. Default is \code{mean}.
#' @param dependent The column number or a string with the column name that 
#'    corresponds to the dependent variable.
#' @param unit.variable The column number or a string with the column name 
#'    that identifies unit numbers. The variable must be numeric.
#' @param time.variable The column number or a string with the column name 
#'    that identifies the period (time) data. The variable must be numeric.
#' @param special.predictors A list object identifying additional predictors and 
#'    their pre-treatment years and operators. 
#' @param treated.units A vector identifying the \code{unit.variable} numbers of 
#'    the treated units.
#' @param control.units A vector identifying the \code{unit.variable} numbers of 
#'    the control units.
#' @param time.predictors.prior A numeric vector identifying the pretreatment 
#'    periods over which the values for the outcome predictors should be averaged.
#' @param time.optimize.ssr A numeric vector identifying the periods of the 
#'    dependent variable over which the loss function should be minimized 
#'    between each treated unit and its synthetic control.
#' @param unit.names.variable The column number or string with column name 
#'    identifying the variable with units' names. The variable must be a character.
#' @param time.plot A vector identifying the periods over which results are 
#'    to be plotted with \code{\link{path.plot}}
#' @param treatment.time A numeric value with the value in \code{time.variable}
#'    that marks the intervention.
#' @param gen.placebos Logical. Whether a placebo (a synthetic control) 
#'    for each unit in the donor pool should be constructed. Will increase 
#'    computation time.
#' @param Sigf.ipop The Precision setting for the ipop optimization routine. 
#'    Default of 5.
#' @param strategy The processing method you wish to use 
#'    "sequential" or "multiprocess". Use "multiprocess" to parallelize operations
#'     and reduce computing time. Default is \code{sequential}.
#' @details The function runs \code{\link[Synth]{dataprep}} and \code{\link[Synth]{synth}} 
#'    for each unit identified in \code{treated.units}. It saves the vector with 
#'    predicted values for each synthetic control, to be used in estimating 
#'    average treatment effects in applications of Synthetic Controls for 
#'    multiple treated units.
#'    
#'    For further details on the arguments, see the documentation of 
#'    \code{\link[Synth:synth]{Synth}}.
#'    
#' @return Data frame. Each column contains the outcome values for every 
#' time-point for one unit or its synthetic control. The last column contains 
#' the time-points.
#' @examples 
#' ## Using the toy data from 'Synth':
#' 
#' library(Synth)
#' data(synth.data)
#' set.seed(42)
#'
#' multi <- multiple.synth(foo = synth.data,
#'                        predictors = c("X1"),
#'                        predictors.op = "mean",
#'                        dependent = "Y",
#'                        unit.variable = "unit.num",
#'                        time.variable = "year",
#'                        treatment.time = 1990,
#'                        special.predictors = list(
#'                          list("Y", 1991, "mean")
#'                        ),
#'                        treated.units = c(2,7),
#'                        control.units = c(29, 13, 17),
#'                        time.predictors.prior = c(1984:1989),
#'                        time.optimize.ssr = c(1984:1990),
#'                        unit.names.variable = "name",
#'                        time.plot = 1984:1996, gen.placebos =  FALSE, 
#'                        Sigf.ipop = 2)

#' ## Plot with the average path of the treated units and the average of their
#' ## respective synthetic controls:
#' 
#' multi$p
#' 
#'@export
#'@importFrom magrittr "%>%"



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
                         gen.placebos=FALSE,
												 strategy = 'sequential',
                         Sigf.ipop = 5){
  # Input Checking
  if(!is.data.frame(foo)){
    stop("Please pass a data.frame to the argument for `foo`")
  }
  
  if(!all(predictors %in% names(foo))){
    stop("One or more of `predictors` is not found in `foo`.")
  }
  
  
  if(Sigf.ipop <0 | !is.integer(as.integer(Sigf.ipop))){
    stop("Please pass a positive integer to the Sigf.ipop argument.")
  }
  
  gen.placebos <- match_logical(gen.placebos)
  
  strategy_match <- match.arg(strategy, c("sequential", "multiprocess"))
  
  if(!all(is.numeric(foo[[unit.variable]]))){
    stop("`unit.variable` must be a numeric column in `foo`")
  }
  
  # Now run code
  

  strategy <- paste0("future::", strategy_match)
  
  
  plan(strategy)
  oplan <- plan()
  
  n <- length(treated.units)
  
  df <- furrr::future_map(1:n, ~make_syn(.x,
  																			 foo,
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
  																			 Sigf.ipop))
  b <- dplyr::bind_cols(purrr::map(df,'a'))
  colnames(b)<-purrr::map_chr(treated.units,~paste0('Control.',.x))
  
  temp<-foo %>% 
  	dplyr::filter(!!as.name(unit.variable) %in% treated.units) %>%
  	dplyr::select(!!as.name(unit.variable), !!as.name(dependent), !!as.name(time.variable)) %>% 
  	tidyr::spread(!!as.name(unit.variable), !!as.name(dependent)) %>%
  	dplyr::filter(!!as.name(time.variable) %in% time.plot)
  
  colnames(temp)[2:(n+1)]<-purrr::map_chr(treated.units,~paste0('Treated.',.x))  
  
  b<-dplyr::bind_cols(temp,b)
  c1<-colnames(b)[2]
  c2<-colnames(b)[ncol(b)]
  
  v <- var <- value <- NULL
  
	df.plot <- b %>% tidyr::pivot_longer(!!as.name(c1):!!as.name(c2), names_to = 'v', values_to = 'value') %>%
  						tidyr::separate(v, c('var','id')) %>%
  						dplyr::group_by(!!as.name(time.variable),var) %>%
  						dplyr::summarise(value = mean(value))
	
	p <- ggplot2::ggplot(df.plot, ggplot2::aes(!!as.name(time.variable), value, linetype = var)) + 
		ggplot2::geom_path() +
		ggplot2::scale_linetype_manual(values = c('dashed','solid')) + 
		ggplot2::theme_minimal() +
		ggplot2::theme(legend.position = 'bottom', legend.title = ggplot2::element_blank()) + 
		ggplot2::ylab(dependent) + ggplot2::geom_vline(xintercept = treatment.time, linetype = 'dotted')
	
  
 	b <- b %>% tidyr::pivot_longer(!!as.name(c1):!!as.name(c2), names_to = 'v', values_to = 'value') %>%
 	   tidyr::separate(v, c('var','id')) %>%
 	   tidyr::spread(var, value)
 	
 	colnames(b)[c(1,2)] <- c('Time','id')

  if(gen.placebos){
    out.temp<-make_syn(index = 1,
                  foo,
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
                  Sigf.ipop)
    tdf<-generate.placebos(dataprep.out=out.temp$dataprep.out,
                           synth.out=out.temp$synth.out,
    											 strategy=strategy_match)
    df.plac<-data.frame(tdf$df)
    df.plac<-df.plac[,-c(ncol(df.plac)-1,ncol(df.plac)-2)]
    out2<-list(df=df,
               p=p,
               b=b,
               df.plot=df.plot,
               treatment.time=treatment.time,
               treated.units=treated.units,
               control.units=control.units,
               df.plac=df.plac) ## Return placebos without including the original treated observation
  } else{
    out2<-list(df = df, 
              p=p, 
              b = b,
              df.plot=df.plot,
              treatment.time=treatment.time,
              treated.units=treated.units,
              control.units=control.units)
  }
  
  class(out2) <- append(class(out2),"tdf_multi")
  
  return(out2)
}
#' @rdname multiple.synth
#' @export
multiple_synth <- multiple.synth


