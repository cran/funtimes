#' @description
#' Advances in multiple aspects of time-series analysis are documented in this package.
#' See available vignettes using \cr
#' \code{browseVignettes(package = "funtimes")}
#'
#' Tests for trends applicable to autocorrelated data, see \cr
#' \code{vignette("trendtests", package = "funtimes")} \cr
#' include bootstrapped versions of t-test and Mann--Kendall test \insertCite{Noguchi_etal_2011}{funtimes}
#' and bootstrapped version of WAVK test for possibly non-monotonic trends \insertCite{Lyubchich_etal_2013_wavk}{funtimes}.
#' The WAVK test is further applied in testing synchronicity of trends \insertCite{Lyubchich_Gel_2016_synchronism}{funtimes};
#' see an implementation to climate data in \insertCite{Lyubchich_2016_trends;textual}{funtimes}.
#' With iterative testing, the synchronicity test is also applied for identifying clusters of
#' multiple time series \insertCite{Ghahari_etal_2017_MBDCE}{funtimes}.
#'
#' Additional clustering methods are implemented using functions \code{BICC} \insertCite{Schaeffer_etal_2016_trust}{funtimes}
#' and \code{DR} \insertCite{Huang_etal_2018_riding}{funtimes};
#' function \code{purity} can be used to assess the accuracy of clustering if true classes are known.
#'
#' Changepoint detection methods include modified CUSUM-based bootstrapped test \insertCite{Lyubchich_etal_2020_changepoints}{funtimes}.
#'
#' Additional functions include implementation of the Beale's ratio estimator, see \cr
#' \code{vignette("beales", package = "funtimes")} \cr
#' Nonparametric comparison of tails of distributions is implemented using small bins defined based on
#' quantiles \insertCite{Soliman_etal_2015_insurance}{funtimes}
#' or intervals in the units in which the data are recorded \insertCite{Lyubchich_Gel_2017_insurance}{funtimes}.
#'
#' For a list of currently deprecated functions, use \code{?'funtimes-deprecated'}
#'
#' For a list of defunct (removed) functions, use \code{?'funtimes-defunct'}
#'
#' @references
#' \insertAllCited{}
#'
#' @importFrom dbscan dbscan
#' @importFrom Kendall MannKendall
#' @importFrom Rdpack reprompt
#' @importFrom stats acf ar arima arima0 arima.sim as.formula cov dist embed filter IQR lm loess median na.omit pnorm qnorm quantile rnorm sd var
#' @importFrom utils combn

"_PACKAGE"
