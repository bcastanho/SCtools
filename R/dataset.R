#' Synth Data
#' @description Synthetic data that can be used to explore SCtools.
#' @format a data.frame with 168 rows and 7 columns:
#' \describe{
#'   \item{unit.num}{The experimental unit number}
#'   \item{year}{year}
#'   \item{name}{name of the experimental unit}
#'   \item{Y}{outcome of interest}
#'   \item{X1}{Covariate 1}
#'   \item{X2}{Covariate 2}
#'   \item{X3}{Covariate 3}
#'    ...
#' }

"synth.data"

#' World Alcohol per Capita Consumption
#' @description This
#' @format a data.frame with 5107 rows and 8 columns:
#' \describe{
#'   \item{country_name}{The name of the country}
#'   \item{year}{year}
#'   \item{consumption}{name of the experimental unit}
#'   \item{country_code}{outcome of interest}
#'   \item{labor_force_participation_rate}{Covariate 1}
#'   \item{mobile_cellular_subscriptions}{Covariate 2}
#'   \item{inflation}{Covariate 3}
#'   \item{manufacturing}{Covariate 3}
#'   \item{country_num}{Covariate 3}
#'    ...
#' }
#' @source World Health Organization Alcohol Consumption available at  
#'     \url{http://apps.who.int/gho/data/node.main.A1039?lang=en}
#'     
#'    World Bank economic indicators available at 
#'    \url{https://data.worldbank.org/}

"alcohol"