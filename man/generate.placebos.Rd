\name{generate.placebos}
\alias{generate.placebos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to generate placebo synthetic controls
}
\description{
Constructs a synthetic control group for each unit in the donor pool of an implementation of the synthetic control method for a single treated unit. Used for placebo tests (see \link{plot.placebos}, \link{mspe.test}, \link{mspe.plot}) to assess the strength and significance of a causal inference based on the synthetic control method. On placebo tests, see Abadie and Gardeazabal (2003), and Abadie, Diamond, and Hainmueller (2010, 2011, 2014).
}
\usage{
generate.placebos(dataprep.out, synth.out)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataprep.out}{
A data.prep object produced by the dataprep() command}
  \item{synth.out}{
A synth.out object produced by the synth() command
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{df }{Data frame with outcome data for each control unit and their respective synthetic control and for the original treated and its control}
\item{mspe.placs }{Mean squared prediction error for the pretreatment period for each placebo}
\item{t0}{First time unit in \code{time.optimize.ssr}}
\item{t1}{First time unit after the highest value in \code{time.optimize.ssr}}
\item{tr}{Unit number of the treated unit}
\item{names.and.numbers}{Dataframe with two columns showing all unit numbers and names from control units}
\item{n}{Number of control units}
\item{treated.name}{Unit name of the treated unit}
\item{loss.v}{Pretreatment MSPE of the treated unit's synthetic control}
%% ...
}
\references{
Abadie, A., Diamond, A., Hainmueller, J. (2014). Comparative Politics and the Synthetic Control Method. American Journal of Political Science Forthcoming 2014.

Synthetic : An R Package for Synthetic Control Methods in Comparative Case Studies. Journal of Statistical Software 42 (13) 1–17.

Abadie, A., Diamond, A., Hainmueller, J. (2011). Synth: An R Package for Synthetic Control Methods in Comparative Case Studies. Journal of Statistical Software 42 (13) 1–17.

Abadie A, Diamond A, Hainmueller J (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. Journal of the American Statistical Association 105 (490) 493–505.

Abadie, A. and Gardeazabal, J. (2003) Economic Costs of Conflict: A Case Study of the Basque Country American Economic Review 93 (1) 113–132.

}
\author{
Bruno Castanho Silva
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## First prepare the required objects

# Load simulated data from Synth
data(synth.data)

# Execute dataprep to produce the necessary matrices for synth
dataprep.out<-
  dataprep(
   foo = synth.data,
   predictors = c("X1", "X2", "X3"),
   predictors.op = "mean",
   dependent = "Y",
   unit.variable = "unit.num",
   time.variable = "year",
   special.predictors = list(
      list("Y", 1991, "mean"),
      list("Y", 1985, "mean"),
      list("Y", 1980, "mean")
                            ),
   treatment.identifier = 7,
   controls.identifier = c(29, 2, 13, 17, 32, 38),
   time.predictors.prior = c(1984:1989),
   time.optimize.ssr = c(1984:1990),
   unit.names.variable = "name",
   time.plot = 1984:1996
   )

# run the synth command to create the synthetic control
synth.out <- synth(dataprep.out)

## run the generate.placebos command to reassign treatment status
## to each unit listed as control, one at a time, and generate their
## synthetic versions. 
tdf <- generate.placebos(dataprep.out,synth.out)

}