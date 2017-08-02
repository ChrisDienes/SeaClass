#' Covariance Check
#'
#' Checks a covariance matrix and identifies variables which should be dropped due to zero variance or high dependence.
#' @details Used to flag variables which will likely cause the covariance matrix to be singular or near singular; and hence, non-invertible.
#' @param cov a matrix.
#' @param cut_point threshold which signifies two variables are highly dependent and one should be omitted. Specifically, when a group of variables are highly dependent, only the first variable is kept and all others are dropped. Defaults to 0.9999.
#' @return \itemize{
#'\item{\code{remove_vars} }{locations of variables to be dropped.}
#'\item{\code{remove_length} }{number of variables to be dropped.}
#'\item{\code{remove_text} }{a message detailing the recommended action.}
#' }
#' @seealso \code{\link{cov_diag_check}}, \code{\link{cov_offdiag_check}}
#' @examples
#' cov1 <- matrix(c(1,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1),ncol=4,nrow=4)
#' cov2 <- diag(c(1,1,1,1))
#' cov_check(cov1, cut_point = 0.9999)
#' cov_check(cov2, cut_point = 0.9999)
#' @export

cov_check = function(cov, cut_point = 0.9999){
  p <- ncol(cov)
  keep_cols <- 1:p
  diag_check <- cov_diag_check(cov)
  remove_text <- ""
  if(diag_check$remove_length > 0){
    keep_cols <- keep_cols[-diag_check$remove_vars]
    cov <- cov[,keep_cols][keep_cols,]
    remove_text <- diag_check$remove_text
  }
  offdiag_check <- cov_offdiag_check(cov = cov, cut_point = cut_point)
  if(offdiag_check$remove_length > 0){
    keep_cols <- keep_cols[-offdiag_check$remove_vars]
    if(remove_text != ""){remove_text <- paste(remove_text, offdiag_check$remove_text)}
    if(remove_text == ""){remove_text <- offdiag_check$remove_text}
  }
  remove_cols <- (1:p)[-keep_cols]
  remove_length <- length(remove_cols)
  return(list(remove_vars = remove_cols, remove_length = remove_length, remove_text = remove_text))
}
