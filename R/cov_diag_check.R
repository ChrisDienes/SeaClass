#' Covariance Diagonal Check
#'
#' Identifies any zero entries along the diagonal of a matrix.
#' @details Intended to be a helper function for flagging zero variance variables whenever the input is a covariance matrix. A covariance matrix containing zeros on the diagonal will be singular; and hence, non-invertible.
#' @param cov a matrix.
#' @return \itemize{
#'\item{\code{remove_vars} }{locations of zero entries.}
#'\item{\code{remove_length} }{number of zero entries.}
#'\item{\code{remove_text} }{a message detailing an action taken by the covariance check wrapper function \code{cov_check()}.}
#' }
#' @seealso \code{\link{cov_check}}
#' @examples
#' cov1 <- diag(c(1,0,3,0))
#' cov2 <- diag(1:4)
#' cov_diag_check(cov1)
#' cov_diag_check(cov2)
#' @export

cov_diag_check = function(cov){
  covs0 <- ifelse(diag(cov) == 0,1,0)
  remove_length <- sum(covs0)
  remove_covs0 <- NA
  if(remove_length > 0){remove_covs0 <- which(covs0 == 1)}
  remove_text <- paste0(remove_length, " variable(s) excluded due to zero variance.")
  return(list(remove_vars = remove_covs0, remove_length = remove_length, remove_text = remove_text))
}
