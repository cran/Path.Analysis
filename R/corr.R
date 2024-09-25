#' @name corr
#' @title Correlation Analysis
#' @author {
#' Ali Arminian <abeyran@gmail.com>
#' }
#' 
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' * `corr()` estimates Pearson correlation coefficients 
#' among parametric numerical characteristics as follows:
#' 
#' * `The Pearson correlation coefficient:`
#' \loadmathjax
#' \mjsdeqn{
#' r_{x,y} = \frac{n\sum{xy}-(\sum{x})(\sum{y})}
#' {\sqrt{(n\sum{x^2}-(\sum{x})^2)(n\sum{y^2}-(\sum{y})^2)}}}
#' 
#' or:
#' \mjsdeqn{
#' r_{x,y} =\frac{\Sigma(x-\bar{x})(y-\bar{y})}
#' {\sqrt{\Sigma{(x-\bar{x})^2\Sigma(y-\bar{y}})^2}} }
#' 
#' where \mjseqn{r_{x,y}} is the `correlation coefficient` 
#' between \mjseqn{x} and \mjseqn{y} variables.
#' 
#' @details
#' The `corr()` function estimates correlation coefficients
#' and their significance in the form of a table of one or 
#' more independent (exogenous) variables on a dependent
#' (endogenous) variable along with testing the significance.
#'
#' @param datap The data set
#' @param verbose If `verbose = TRUE` then some results are
#' printed in the console.
#' @importFrom corrplot cor.mtest
#' @importFrom stats cor lm pt sd
#' @importFrom Hmisc rcorr
#' @importFrom pastecs stat.desc
#' @importFrom grDevices dev.new
#'
#' @seealso `correlation`
#'
#' @usage corr(datap, verbose = FALSE)
#' @return Returns a list of two objects:
#' \describe{
#'   \item{Correlations}{the data frame of Pearson's correlation coefficients}
#'   \item{P_values}{the data frame of significance of correlation coefficients (r):}
#'    \itemize{
#'    \item{\code{p} p-value for testing the r}
#'    \item{\code{lowCI} lower confidence interval of r}
#'    \item{\code{uppCI} upper confidence interval of r}
#'    }
#'   }
#' @examples
#' \donttest{
#' data(dtsimp)
#' corr(dtsimp, verbose = FALSE)
#' }
#' @examples
#' \donttest{
#' data(dtraw)
#' corr(dtraw[, -1], verbose = FALSE)
#' }
#' @export

corr <- function(datap, verbose = FALSE)
{
  datap <- dataprep(datap)
  
  corr <- rcorr(as.matrix(datap), type="pearson")
  
  datam <- as.matrix(datap)
  corp <- cor.mtest(datam)
  c.p. <- list(corr, corp)
  class(c.p.) <- "list"
  
  if(verbose) {
    cat("Pearson correlation coefficients information:\n")
    cat(".............................................\n")
    cat("\np: p-value\n", "\n")
    cat("lowCI: lower confidence interval\n")
    cat("\nuppCI: upper confidence interval")
  }
  return(c.p.)
}