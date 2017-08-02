#' Score a Binary Threshold
#'
#' Scores a user specified threshold.
#' @details Output quantities for false positive, true positive, and misclassification are percentages.
#' @param x a numeric predictor.
#' @param group binary grouping variable.
#' @param pos_class group level for positive class.
#' @param cut user specified threshold to divide \code{x} input.
#' @param type direction of positive group. Options are "upper" (default) and "lower".
#' @return \itemize{
#' \item{\code{FP} }{false positive percentage.}
#' \item{\code{TP} }{true positive percentage.}
#' \item{\code{Misclass} }{misclassification percentage.}
#' \item{\code{Direction} }{text summarizing threshold rule.}
#' }
#' @seealso \code{\link{accuracy_threshold}}, \code{\link{max_fp_threshold}}
#' @examples
#' x <- c(rnorm(100,0,1),rnorm(100,2,1))
#' group <- c(rep(0,100),rep(2,100))
#' score_threshold(x=x, group=group, pos_class=2, cut=1, type="upper")
#' score_threshold(x=x, group=group, pos_class=0, cut=1, type="lower")
#' @export

score_threshold = function(x, group, pos_class, cut, type = "upper"){
  p_1 <- mean(group == pos_class)
  if(type == "upper"){
    FP <- 100*mean(x[group != pos_class] > cut)
    TP <- 100*mean(x[group == pos_class] > cut)
    MR <- 100*(p_1*(1-TP/100) + (1-p_1)*FP/100)
    Direction <- paste0("Flag if > ", cut)
  }
  if(type == "lower"){
    FP <- 100*mean(x[group != pos_class] <= cut)
    TP <- 100*mean(x[group == pos_class] <= cut)
    MR <- 100*(p_1*(1-TP/100) + (1-p_1)*FP/100)
    Direction <- paste0("Flag if <= ", cut)
  }
  return(list(FP=FP, TP=TP, Misclass=MR, Direction = Direction))
}
