#' Compute the fold change between expression A and expression B of each gene
#' @description compute the fold change
#' @param x dataframe of expression A
#' @param y dataframe of expression B
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' fc = FC(x,y)
FC = function(x,y)
{
  mean_x = apply(x, 1, mean)
  mean_x = sapply(mean_x,simplify = T,function(x) x+0.25)
  mean_y = apply(y,1,mean)
  mean_y = sapply(mean_y,simplify = T,function(x) x+0.25)
  fold_change = mean_y/mean_x
  return(fold_change)
}
