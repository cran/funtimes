#' Quantile-Based Tails Comparison
#'
#' Compare right tails of two sample distributions using
#' a quantile-based approach (QBA);
#' see \insertCite{Soliman_etal_2014_insurance;textual}{funtimes},
#' \insertCite{Soliman_etal_2015_insurance;textual}{funtimes},
#' and \insertCite{Lyubchich_Gel_2017_insurance;textual}{funtimes}.
#'
#' @details Sturges' formula is used to calculate the number of intervals (\eqn{k})
#' to split the upper \eqn{100(1 - q)}\% portion of \code{x0} and \code{x1}
#' (the right tails). Then, each tail is divided into equally-filled intervals
#' with a quantile step \eqn{d=(1 - q)/k}. \code{Pk} reports the difference between
#' corresponding intervals' centers obtained from \code{x0} and \code{x1}.
#'
#'
#' @param x0,x1 vectors of the same length (preferably).
#' Tail in \code{x1} is compared against the tail in \code{x0}.
#' @param q a quantile defining the right tail for both \code{x0} and \code{x1}.
#' Values above the  thresholds \code{quantile(x0, probs = q)} and
#' \code{quantile(x1, probs = q)} are considered as the respective right tails.
#'
#'
#' @return A list with two elements:
#' \item{d}{the step in probabilities for defining the quantiles.}
#' \item{Pk}{vector of differences of the intervals' centers.}
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso \code{\link{i.tails}}
#'
#' @keywords ts
#'
#' @author Vyacheslav Lyubchich, Yulia R. Gel
#'
#' @export
#' @examples
#' x0 <- rnorm(1000)
#' x1 <- rt(1000, 5)
#' tails_q(x0, x1)
#'
tails_q <- function(x0, x1, q = 0.99){
  n <- length(x0)*(1 - q)
  # Sturges' formula
  k <- ceiling(log2(n) + 1)
  d <- (1 - q)/k
  m1 <- sapply(1:k, function(i) mean(quantile(x0, probs = c((q + (i - 1)*d), (q + i*d)))))
  m2 <- sapply(1:k, function(i) mean(quantile(x1, probs = c((q + (i - 1)*d), (q + i*d)))))
  Pk <- m2 - m1
  return(list(d = d, Pk = Pk))
}
