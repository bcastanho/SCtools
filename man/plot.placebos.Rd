\name{plot.placebos}
\alias{plot.placebos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot placebos of a synthetic control analysis
}
\description{
Creates plots with the difference between observed units and synthetic controls for the treated and control units. See Abadie, Diamond, and Hainmueller (2011). 
}
\usage{
plot.placebos(tdf = tdf, discard.extreme = FALSE, mspe.limit = 20, xlab = NULL, ylab = NULL, title = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tdf}{
An object with a list of outcome values for placebos, constructed by \link{generate.placebos}
}
  \item{discard.extreme}{
Logical. Whether or not units with high pretreatement MSPE should be excluded from the plot.
}
  \item{mspe.limit}{
Numerical. Used if \code{discard.extreme} is \code{TRUE}. It indicates how many times the pretreatment MSPE of a placebo should be higher than that of the treated unit to be considered extreme and discarded. Default is \code{20}.
}
  \item{xlab}{
Character. Optional. Label of the x axis.
}
  \item{ylab}{
Character. Optional. Label of the y axis.
}
  \item{title}{
Character. Optional. Title of the plot.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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
\code{\link{generate.placebos}, \link[Synth]{gaps.plot}, \link[Synth]{synth}, \link[Synth]{dataprep}
}}
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
tdf<-generate.placebos(dataprep.out,synth.out)

## Plot the gaps in outcome values over time of each unit --
## treated and placebos -- to their synthetic controls

p <- plot.placebos(tdf,discard.extreme=T,mspe.limit=10,xlab='Year')
p

}