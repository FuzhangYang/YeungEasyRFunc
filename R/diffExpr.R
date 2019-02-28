#' Select DE result by cutoff
#'
#' @param data1 a dataframe of expression A
#' @param data2 a dataframe of expression B
#' @param x a numeric vector of fold change result from 'FC' func
#' @param y a numeric vector of p value result from 'pValue' func
#' @param cut_pvalue pvalue cutoff
#' @param cut_fc fold change cutoff , value is abs(log2FC)
#'
#' @return a dataframe of differential expression genes, which contains the mean expression and fold change and p-value
#' @export
#'
#' @examples
#' de = diffExpr(data1,data2,p_value,fc,0.05,0.5)
diffExpr = function(data1,data2,x,y,cut_pvalue,cut_fc)
{
  ## x means fold change,y means p value
  index = y<cut_pvalue
  foldchange = abs(log2(x))>cut_fc
  ave1 = apply(data1,1,mean)
  ave1 = ave1[index & foldchange]
  ave2 = apply(data2,1,mean)
  ave2 = ave2[index & foldchange]
  fc_de = x[index & foldchange]
  p_de = y[index & foldchange]
  result = cbind(ave1,ave2,fc_de,p_de)
  result = as.data.frame(result)
  return(result)
}
