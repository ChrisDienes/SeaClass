#' Boxplot Outlier Cut Points
#'
#' Constructs boxplot style rules for flagging outliers.
#' @details Computes two outlier rules. The standard boxplot outlier rule based on \code{Q1 - IQR*iqr_mult} and \code{Q3 + IQR*iqr_mult}. The adjusted boxplot rule which uses the medcouple to account for data skewness. The medcouple depends on the \pkg{robustbase} package.
#' @param x a numeric vector.
#' @param iqr_mult interquartile range multiplier. Defaults to 3.
#' @return \itemize{
#' \item{\code{medcouple} }{medcouple value returned from \code{robustbase::mc(x)}.}
#' \item{\code{standard_rule} }{lower and upper limits.}
#' \item{\code{adjusted_rule} }{lower and upper limits.}
#' }
#' @references  M. Hubert; E. Vandervieren (2008). "An adjusted boxplot for skewed distributions". Computational Statistics and Data Analysis. 52 (12): 5186-5201.
#' @seealso \code{\link[robustbase]{mc}}, \code{\link{score_threshold}}
#' @examples
#' x1 = rnorm(1000,0,1)
#' x2 = rexp(1000,1)
#' boxplot_cutpoints(x=x1, iqr_mult=1.5)
#' boxplot_cutpoints(x=x2, iqr_mult=1.5)
#' @export

boxplot_cutpoints = function(x, iqr_mult = 3){
  my_mc <- robustbase::mc(x)
  qa <- quantile(x, probs = c(.25,.75))
  iqr <- diff(qa)
  standard_rule_upper <- qa[2] + iqr_mult*iqr
  standard_rule_lower <- qa[1] - iqr_mult*iqr
  if(my_mc >= 0){
    adjusted_rule_upper <- qa[2] + iqr_mult*exp(3*my_mc)*iqr
    adjusted_rule_lower <- qa[1] - iqr_mult*exp(-4*my_mc)*iqr
  }
  if(my_mc < 0){
    adjusted_rule_upper <- qa[2] + iqr_mult*exp(4*my_mc)*iqr
    adjusted_rule_lower <- qa[1] - iqr_mult*exp(-3*my_mc)*iqr
  }

  return(list(medcouple = my_mc, standard_rule = c(standard_rule_lower,standard_rule_upper), adjusted_rule = c(adjusted_rule_lower,adjusted_rule_upper)))
}
