#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#'
#' @title Compute p-value using t.test from expression A to expression B
#'
#' @param x a dataframe of expression A
#' @param y a dataframe of expression B
#'
#' @return a numeric vector of pvalue between genes
#' @export
#'
#' @examples
#' pvalue = pValue(x,y)
pValue = function(x,y)
{
  p_value = numeric()
  for (i in 1:dim(x)[1])
  {
    if(sd(x[i,])==0 && sd(y[i,])==0)
    {
      p_value[i] = 1
    }
    else
    {
      t = t.test(x[i,],y[i,],var.equal = T)
      p_value[i] = t$p.value
    }

  }
  return(p_value)
}
