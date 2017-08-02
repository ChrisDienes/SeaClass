#' Best Threshold Rule for a User's Maximum False Positive Rate
#'
#' Returns the best threshold rule based on true positive rate, where the rule has a false positive rate not exceeding a user specified maximum.
#' @details Output quantities for false positive, true positive, and misclassification are percentages.
#' @param x a numeric predictor.
#' @param group binary grouping variable.
#' @param pos_class group level for positive class.
#' @param max_fp maximum false positive rate. Defaults to 0.05.
#' @return \itemize{
#' \item{\code{FP} }{false positive percentage.}
#' \item{\code{TP} }{true positive percentage.}
#' \item{\code{Misclass} }{misclassification percentage.}
#' \item{\code{Direction} }{text summarizing threshold rule.}
#' }
#' @seealso \code{\link{accuracy_threshold}}, \code{\link{score_threshold}}
#' @examples
#' x <- c(rnorm(100,0,1),rnorm(100,2,1))
#' group <- c(rep(0,100),rep(2,100))
#' max_fp_threshold(x=x, group=group, pos_class=2, max_fp = 0.05)
#' max_fp_threshold(x=x, group=group, pos_class=0, max_fp = 0.05)
#' @export

max_fp_threshold = function(x, group, pos_class, max_fp = 0.05){
  if(max_fp > 1 | max_fp < 0){return(warning("ERROR: max_fp is not in [0,1]",call.=FALSE))}
  group_01 <- ifelse(group == pos_class, 1, 0)
  all_splits <- sort(unique(x))
  pos_index <- (group_01 == 1)
  pos_splits <- sort(unique(x[pos_index]))
  pos_loc <- match(pos_splits, all_splits)
  possible_splits <- sort(unique(c(all_splits[pos_loc[which(diff(c(0,pos_loc)) != 1)]-1],pos_splits, max(all_splits))))
  fp <- Vectorize(function(cut) mean(x[!pos_index] > cut) )
  tp <- Vectorize(function(cut) mean(x[pos_index] > cut) )
  fp_up   <- fp(possible_splits)
  fp_down <- 1 - fp_up
  if(min(fp_up) <= max_fp){
    up_possible   <- possible_splits[which(fp_up <= max_fp)[1]]
    tp_up <- tp(up_possible)
    fp_up <- fp(up_possible)
  }else{
    tp_up <- 0
  }
  if(min(fp_down) <= max_fp){
    down_possible <- tail(possible_splits[which(fp_down <= max_fp)],1)
    tp_down <- 1 - tp(down_possible)
    fp_down <- 1 - fp(down_possible)
  }else{
    tp_down <- 0
  }
  if(tp_up + tp_down == 0){
    return(list(FP=0, TP=0, Misclass=100*mean(group_01), Direction = "Flag None"))
  } else if (tp_up >= tp_down){
    misclass_up <- mean(c(0, 1)[(x > up_possible) + 1] != group_01)
    return(list(FP=100*fp_up, TP=100*tp_up, Misclass=100*misclass_up, Direction = paste0("Flag if > ", up_possible)))
  } else {
    misclass_down <- mean(c(0, 1)[(x <= down_possible) + 1] != group_01)
    return(list(FP=100*fp_down, TP=100*tp_down, Misclass=100*misclass_down, Direction = paste0("Flag if <= ", down_possible)))
  }
}
