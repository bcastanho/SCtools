#' @title Function to compute the post/pre treatment MSPE ratio for the 
#'     treated unit and placebos
#' @description Computes the post/pre treatement mean square prediction error 
#'    ratio for a treated unit in a synthetic control analysis and all placebos 
#'    produced with \code{\link{generate.placebos}}. Returns a matrix with 
#'    ratios and a p-value of how extreme the treated unit's ratio is in 
#'    comparison with that of placebos. Equivalent to a significance testing 
#'    of a synthetic controls result.
#' @param tdf An object constructed by \code{\link{generate.placebos}}
#' @param discard.extreme Logical. Whether or not placebos with high 
#'    pre-treatement MSPE should be excluded from the count and significance 
#'    testing.
#' @param mspe.limit Numerical. Used if \code{discard.extreme} is \code{TRUE}. 
#'    It indicates how many times the pretreatment MSPE of a placebo should be 
#'    higher than that of the treated unit to be considered extreme and 
#'    discarded. Default is \code{20}.
#' @details Post/pre-treatement mean square prediction error ratio is the 
#'    difference between the observed outcome of a unit and its synthetic 
#'    control, before and after treatement. A higher ratio means a small 
#'    pre-treatment prediction error (a good synthetic control), and a 
#'    high post-treatment MSPE, meaning a large difference between the unit 
#'    and its synthetic control after the intervention. 
#'    By calculating this ratio for all placebos, the test can be interpreted 
#'    as looking at how likely the result obtained for a single treated case 
#'    with a synthetic control analysis could have occurred by chance given 
#'    no treatement. For more detailed description, see Abadie, Diamond, 
#'    and Hainmueller (2011, 2014). 
#' @return \describe{
#'    \item{p.val}{The p-value of the treated unit post/pre MSPE ratio. 
#'    It is the proportion of units (placebos and treated) that have a ratio 
#'    equal or higher that of the treated unit}
#'    \item{test}{Dataframe with two columns. The first is the post/pre MSPE 
#'    ratio for each unit. The second indicates unit names}
#'}
#' @seealso  \code{\link{generate.placebos}}, \code{\link{mspe.plot}}, 
#'    \code{\link[Synth]{synth}}
#'    
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
#'     controls.identifier = c(29, 2, 17),
#'     time.predictors.prior = c(1984:1989),
#'     time.optimize.ssr = c(1984:1990),
#'     unit.names.variable = "name",
#'     time.plot = 1984:1996
#' )
#' 
#' # run the synth command to create the synthetic control
#' synth.out <- synth(dataprep.out, Sigf.ipop=1)
#' 
#' tdf <- generate.placebos(dataprep.out,synth.out, Sigf.ipop = 1)
#' ## Test how extreme was the observed treatment effect given the placebos:
#' ratio <- mspe.test(tdf)
#' ratio$p.val
#' 
## Check visually how extreme is this value in the distribution:
#' mspe.plot(tdf, discard.extreme = FALSE)
#' }
#' \dontrun{## Example with toy data from 'Synth'
#' library(Synth)
#' # Load the simulated data
#' data(synth.data)
#' 
#' # Execute dataprep to produce the necessary matrices for 'Synth'
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
#' 
#' ## Test how extreme was the observed treatment effect given the placebos:
#' ratio <- mspe.test(tdf)
#' ratio$p.val
#' 
## Check visually how extreme is this value in the distribution:
#' mspe.plot(tdf, discard.extreme = FALSE)
#' }   
#' @export

mspe.test <- function (tdf, discard.extreme = FALSE, mspe.limit = 20) 
{
	discard.extreme <- match_logical(discard.extreme)
	if (!is_tdf(tdf)) {
		stop("Please pass a valid `tdf` object the tdf argument.\nThese are generated by the `generate.placebos` function.")
	}
	if (!is.logical(discard.extreme)) {
		stop("Please pass a logical (TRUE/FALSE) to `discard.extreme`")
	}
	t.mspe <- tdf$loss.v
	year <- mspe <- NULL
	if(discard.extreme) {
		extremes <- which(tdf$mspe.placs/tdf$loss.v[1] >= mspe.limit)
		if(length(extremes) < 1){
			warning('No placebos have a pre-MSPE above mspe.limit. No units were dropped')
			discard.extreme <- FALSE
		} }
	if (!discard.extreme) {
		n <- tdf$n
		pre <- subset(tdf$df, year < tdf$t1 & year >= tdf$t0)
		post <- subset(tdf$df, year >= tdf$t1)
		unit.names <- as.character(tdf$names.and.numbers$unit.names)
		mspe.placs <- tdf$mspe.placs
	}
	else {
		extremes <- which(tdf$mspe.placs/tdf$loss.v[1] >= mspe.limit)
		tdf$df <- tdf$df[, -c(extremes, extremes + tdf$n)]
		pre <- subset(tdf$df, year < tdf$t1 & year >= tdf$t0)
		post <- subset(tdf$df, year >= tdf$t1)
		n <- tdf$n - length(extremes)
		unit.names <- as.character(tdf$names.and.numbers$unit.names[-extremes])
		mspe.placs <- data.frame(tdf$mspe.placs[-extremes, ])
	}
	test <- data.frame(matrix(0, ncol = 1, nrow = n))
	for (i in 1:n) {
		test[i, 1] <- cvTools::mspe(post[, i], post[, i + n])/mspe.placs[i, 
																																		 ]
	}
	test[n + 1, 1] <- cvTools::mspe(post[, ncol(post) - 2], post[, 
																															 ncol(post) - 1])/t.mspe
	test[1:n, 2] <- unit.names
	test[nrow(test), 2] <- tdf$treated.name
	colnames(test) <- c("MSPE.ratios", "unit")
	p.val <- sum(test[1:nrow(test), 1] >= test[nrow(test), 1])/nrow(test)
	res.mspe <- list(p.val = p.val, test = test)
	return(res.mspe)
}

#' @rdname mspe.test
#' @export
mspe_test <- mspe.test


