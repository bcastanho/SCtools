#' Synth Data
#' Synthetic data that can be used to explore SCtools.
#' @format a data.frame with 168 rows and 7 columns:
#' \describe{
#'   \item{unit.num}{The experimental unit number}
#'   \item{year}{year}
#'   \item{name}{name of the experimental unit}
#'   \item{Y}{outcome of interest}
#'   \item{X1}{Covariate 1}
#'   \item{X2}{Covariate 2}
#'   \item{X3}{Covariate 3}
#' }

"synth.data"

#' World Alcohol per Capita Consumption
#' 
#' This data set has been compiled from data from the World Health
#' Organization (WHO) and the World Bank (WB). The primary purpose was to investigate
#' the effects of policy changes in the Russian Federation enacted in 2003
#' around alcohol consumption. This is an excellent case study for SCM 
#' approaches to be used. You can read more about the policy changes at
#' \url{https://www.theguardian.com/world/2019/oct/01/russian-alcohol-consumption-down-40-since-2003-who}
#' 
#' WHO data available at \url{http://apps.who.int/gho/data/node.main.A1039?lang=en}.
#' 
#' WB data available at \url{https://data.worldbank.org/}.
#' 
#' @format a data.frame with 5107 rows and 8 columns:
#' 
#' \describe{
#'   \item{country_name}{The name of the country}
#'   \item{year}{year}
#'   \item{consumption}{Alcohol consumption per capita (liters/person); all types}
#'   \item{country_code}{Three letter country code}
#'   \item{labor_force_participation_rate}{Labor force participation rate, total (percent of total population ages 15+)}
#'   \item{mobile_cellular_subscriptions}{Mobile cellular subscriptions (per 100 people)}
#'   \item{inflation}{Inflation, consumer prices (annual percent)}
#'   \item{manufacturing}{Manufacturing, value added (percent of GDP)}
#'   \item{country_num}{The country number}
#' }


"alcohol"