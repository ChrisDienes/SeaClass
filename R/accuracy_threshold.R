#' Accuracy Threshold
#'
#' Returns the best binary threshold rule based on accuracy.
#' @details Optimizes threshold rule based on misclassification rate (1 - accuracy). Upper rules are defined as greater than \code{cut}, and lower is less than or equal to \code{cut}.
#' @param x a numeric predictor.
#' @param group binary grouping variable.
#' @param pos_class group level for positive class.
#' @return \itemize{
#'\item{\code{misclass} }{rate of misclassification.}
#'\item{\code{cut} }{selected threshold.}
#'\item{\code{direction} }{indicator for positive group direction.}
#' }
#' @seealso \code{\link{max_fp_threshold}}, \code{\link{score_threshold}}
#' @examples
#' ### General Use: ###
#' set.seed(123)
#' x <- c(rnorm(100,0,1),rnorm(100,2,1))
#' group <- c(rep(0,100),rep(2,100))
#' accuracy_threshold(x=x, group=group, pos_class=2)
#' accuracy_threshold(x=x, group=group, pos_class=0)
#' ### Bagged Example ###
#' set.seed(123)
#' replicate_function = function(index){accuracy_threshold(x=x[index], group=group[index], pos_class=2)$cut}
#' sample_cuts = replicate(100, {
#'   sample_index = sample.int(n=length(x),replace=TRUE)
#'   replicate_function(index=sample_index)
#' })
#' bagged_scores = sapply(x, function(x) mean(x > sample_cuts))
#' unbagged_cut    = accuracy_threshold(x=x, group=group, pos_class=2)$cut
#' unbagged_scores = ifelse(x > unbagged_cut, 1, 0)
#' # Compare AUC:
#' PRROC::roc.curve(scores.class0 = bagged_scores,weights.class0 = ifelse(group==2,1,0))$auc
#' PRROC::roc.curve(scores.class0 = unbagged_scores,weights.class0 = ifelse(group==2,1,0))$auc
#' bagged_prediction = ifelse(bagged_scores > 0.50, 2, 0)
#' unbagged_prediction = ifelse(x > unbagged_cut, 2, 0)
#' # Compare Confusion Matrix:
#' table(bagged_prediction, group)
#' table(unbagged_prediction, group)
#' @export

accuracy_threshold = function(x, group, pos_class){
  group_01 <- ifelse(group == pos_class, 1, 0)
  baseline_accuracy <- 1 - mean(group_01)
  all_splits <- sort(unique(x))
  pos_splits <- sort(unique(x[group_01 == 1]))
  pos_loc <- match(pos_splits, all_splits)
  possible_splits <- sort(unique(c(all_splits[pos_loc[which(diff(c(0,pos_loc)) != 1)]-1],pos_splits, max(all_splits))))
  accuracy_up <- Vectorize(function(cut) mean(c(0, 1)[(x > cut) + 1] == group_01))
  tmp_up   <- accuracy_up(possible_splits)
  tmp_down <- 1 - tmp_up
  max_up   <- max(tmp_up)
  max_down <- max(tmp_down)
  my_accuracy <- max(c(max_up,max_down))
  misclass <- 1 - my_accuracy
  if(baseline_accuracy == my_accuracy){
    cut <- NA
    direction <- NA
  }else if(max_up >= max_down & baseline_accuracy != my_accuracy) {
    cut <- possible_splits[which.max(tmp_up)]
    direction <- "upper"
  } else {
    cut <- possible_splits[which.max(tmp_down)]
    direction <- "lower"
  }
  return(list(misclass = misclass, cut = cut, direction = direction))
}
