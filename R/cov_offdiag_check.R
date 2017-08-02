#' Covariance Off Diagonal Check
#'
#' Converts a covariance matrix to a correlation matrix and then identifies variables which should be dropped due to high dependence.
#' @details Intended to be a helper function for flagging highly dependent variables which cause the covariance matrix to be singular or near singular; and hence, non-invertible.
#' @param cov a matrix.
#' @param cut_point threshold which signifies two variables are highly dependent and one should be omitted. Specifically, when a group of variables are highly dependent, only the first variable is kept and all others are dropped. Defaults to 0.9999.
#' @return \itemize{
#'\item{\code{remove_vars} }{locations of variables to be dropped.}
#'\item{\code{remove_length} }{number of variables to be dropped.}
#'\item{\code{remove_text} }{a message detailing an action taken by the covariance check wrapper function \code{cov_check()}.}
#' }
#' @seealso \code{\link{cov_check}}
#' @examples
#' cov1 <- matrix(c(1,0,0,0,1,1,0,1,1),ncol=3,nrow=3)
#' cov2 <- diag(c(1,1,1))
#' cov_offdiag_check(cov1, cut_point = 0.9999)
#' cov_offdiag_check(cov2, cut_point = 0.9999)
#' @export

cov_offdiag_check = function(cov, cut_point = 0.9999){
  my_cor <- cov2cor(cov)
  cors1 <- NULL
  p <- ncol(cov)
  for(pp in 1:(p-1)){
    test_cor <- which(abs(my_cor[(pp+1):p,pp]) > cut_point) + pp
    if(length(test_cor) > 0 ){cors1 <- c(cors1, test_cor)}
  }
  remove_length <- length(cors1)
  if(remove_length > 0){
    cors1 <- unique(cors1)
    remove_length <- length(cors1)
  }
  remove_text <- paste0(remove_length, " variable(s) excluded due to (near) linear redundancy.")
  return(list(remove_vars = cors1, remove_length = remove_length, remove_text = remove_text))
}
