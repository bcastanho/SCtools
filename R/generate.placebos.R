#' @title Function to generate placebo synthetic controls
#' @description Constructs a synthetic control unit for each unit in the 
#'    donor pool of an implementation of the synthetic control method for a 
#'    single treated unit. Used for placebo tests (see \link{plot_placebos}, 
#'    \link{mspe.test}, \link{mspe.plot}) to assess the strength and 
#'    significance of a causal inference based on the synthetic control method. 
#'    On placebo tests, see Abadie and Gardeazabal (2003), and Abadie, Diamond, 
#'    and Hainmueller (2010, 2011, 2014).
#' @param dataprep.out A data.prep object produced by the \code{dataprep} command
#' @param synth.out A synth.out object produced by the \code{synth} command
#' @param Sigf.ipop The Precision setting for the ipop optimization routine. 
#'     Default of 5.
#' @param strategy The processing method you wish to use 
#'    "sequential" or "multiprocess". Use "multiprocess" to parallelize operations
#'     and reduce computing time. Default is \code{sequential}.
#' @return \describe{
#'    \item{df }{Data frame with outcome data for each control unit and their 
#'    respective synthetic control and for the original treated and its control}
#'    \item{mspe.placs }{Mean squared prediction error for the pretreatment 
#'    period for each placebo}
#'    \item{t0}{First time unit in \code{time.optimize.ssr}}
#'    \item{t1}{First time unit after the highest value in \code{time.optimize.ssr}}
#'    \item{tr}{Unit number of the treated unit}
#'    \item{names.and.numbers}{Dataframe with two columns showing all unit 
#'    numbers and names from control units}
#'    \item{n}{Number of control units}
#'    \item{treated.name}{Unit name of the treated unit}
#'    \item{loss.v}{Pretreatment MSPE of the treated unit's synthetic control}
#'}
#' @examples 
#' \dontshow{## Example with toy data from Synth
#' library(Synth)
#' # Load the simulated data
#' data(synth.data)
#' 
#' # Execute dataprep to produce the necessary matrices for synth
#' dataprep.out<-
#'   dataprep(
#'     foo = synth.data,
#'     predictors = c("X1"),
#'     predictors.op = "mean",
#'     dependent = "Y",
#'     unit.variable = "unit.num",
#'     time.variable = "year",
#'     special.predictors = list(
#'       list("Y", 1991, "mean")
#'     ),
#'     treatment.identifier = 7,
#'     controls.identifier = c(29, 2, 13),
#'     time.predictors.prior = c(1984:1989),
#'     time.optimize.ssr = c(1984:1990),
#'     unit.names.variable = "name",
#'     time.plot = 1984:1996
#' )
#' 
#' # run the synth command to create the synthetic control
#' synth.out <- synth(dataprep.out, Sigf.ipop=2)
#' 
#' ## run the generate.placebos command to reassign treatment status
#' ## to each unit listed as control, one at a time, and generate their
#' ## synthetic versions. Sigf.ipop = 2 for faster computing time. 
#' ## Increase to the default of 5 for better estimates. 
#' tdf <- generate.placebos(dataprep.out,synth.out, Sigf.ipop = 2)
#' }
#' \dontrun{## Example with toy data from Synth
#' library(Synth)
#' # Load the simulated data
#' data(synth.data)
#' 
#' # Execute dataprep to produce the necessary matrices for synth
#' dataprep.out<-
#'   dataprep(
#'     foo = synth.data,
#'     predictors = c("X1"),
#'     predictors.op = "mean",
#'     dependent = "Y",
#'     unit.variable = "unit.num",
#'     time.variable = "year",
#'     special.predictors = list(
#'       list("Y", 1991, "mean")
#'     ),
#'     treatment.identifier = 7,
#'     controls.identifier = c(29, 2, 13, 17),
#'     time.predictors.prior = c(1984:1989),
#'     time.optimize.ssr = c(1984:1990),
#'     unit.names.variable = "name",
#'     time.plot = 1984:1996
#' )
#' 
#' # run the synth command to create the synthetic control
#' synth.out <- synth(dataprep.out, Sigf.ipop=2)
#' 
#' ## run the generate.placebos command to reassign treatment status
#' ## to each unit listed as control, one at a time, and generate their
#' ## synthetic versions. Sigf.ipop = 2 for faster computing time. 
#' ## Increase to the default of 5 for better estimates. 
#' tdf <- generate.placebos(dataprep.out,synth.out, Sigf.ipop = 2)
#' }
#' @importFrom future plan 
#' @importFrom stats setNames
#' @export

generate.placebos <- function(dataprep.out,
                              synth.out,
                              Sigf.ipop = 5,
                              strategy = "sequential") {
  
  # Inputs Checkss
  if(!all(names(dataprep.out) %in% dataprep_object_names)){
    stop("Invalid dataprep object. Have you run `dataprep` on your data?")
  }
  
  if(!all(names(synth.out) %in% synth_object_names)){
    stop("Invalid synth output. Have you run the `synth` function?")
  }
  
  if(is.integer(Sigf.ipop) | Sigf.ipop <= 0){
    stop("You have not passed a valid argument for Signf.ipop. Please pass a positive integer")
  }
  
  strategy_match <- match.arg(strategy, c("sequential", "multiprocess"))
  
  unit.numbers <- NULL
  
  tr <- as.numeric(dataprep.out$tag$treatment.identifier)
  
  names.and.numbers <-
    subset(dataprep.out$names.and.numbers, unit.numbers != tr)
  
  n <- length(dataprep.out$tag$controls.identifier)
  
  b <-
    data.frame(matrix(
      0,
      ncol = n,
      nrow = length(dataprep.out$tag$time.plot)
    ))
  
  #mspe.placs <- data.frame(matrix(0, ncol = 1, nrow = n))
  
  # for (i in 1:n) {
  #   temp <- syn_plac(i, dataprep.out, Sigf.ipop, tr, names.and.numbers)
  #   b[, i] <- temp$a
  #   colnames(b)[i] <-
  #     paste('synthetic', as.character(names.and.numbers[i, 2]), sep = '.')
  #   mspe.placs[i, ] <- temp$s.mspe
  # }
  
  strategy <- paste0("future::", strategy_match)
  
  plan(strategy)
  oplan <- plan()
  
  mspe2 <- furrr::future_map(1:n, ~syn_plac(.x, 
                                            dataprep.out, 
                                            Sigf.ipop, 
                                            tr, names.and.numbers))
  
  b <- dplyr::bind_cols(purrr::map(mspe2,"a"))
  b <- setNames(b, paste('synthetic', as.character(names.and.numbers[ ,2]), sep = '.'))
  
  mspe.placs <- purrr::map(mspe2, "s.mspe")
  mspe.placs <- as.data.frame(unlist(mspe.placs))
    
  
  on.exit(plan(oplan), add = TRUE)
  
  df <-
    cbind(
      b,
      dataprep.out$Y0,
      dataprep.out$Y1,
      dataprep.out$Y0plot %*% synth.out$solution.w,
      dataprep.out$tag$time.plot
    )
  colnames(df)[(ncol(df) - 2):ncol(df)] <-c('Y1', 'synthetic.Y1', 'year')
  
  t0 <- as.numeric(dataprep.out$tag$time.optimize.ssr[1])
  t1 <-
    as.numeric(dataprep.out$tag$time.optimize.ssr[length(dataprep.out$tag$time.optimize.ssr)]) +
    1
  treated.name <-
    as.character(dataprep.out$names.and.numbers$unit.names[dataprep.out$names.and.numbers[, 2] %in% dataprep.out$tag$treatment.identifier])
  
  loss.v <- synth.out$loss.v
  
  res2 <-
    list(
      df = df,
      mspe.placs = mspe.placs,
      t0 = t0,
      t1 = t1,
      tr = tr,
      names.and.numbers = names.and.numbers,
      n = n,
      treated.name = treated.name,
      loss.v = loss.v
    )
  
  class(res2) <- append(class(res2),"tdf")
  
  return(res2)
}

#' @rdname generate.placebos
#' @export
generate_placebos <- generate.placebos

