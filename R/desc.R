#' @name desc
#' @title Descriptive statistics
#' @author {
#' Ali Arminian <abeyran@gmail.com>
#' }
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' * `desc()` estimates the descriptive statistics such as 
#' `Min`(Minimum), `1st Qu.`(quartile), `Median`, `Mean`
#' (average), `3rd Qu.`(3rd quartile), `Max`(maximum), `var`
#' (variance), `std.dev`(standard deviation), `coef.var`
#' (CV or coefficient of variation) of the data set. 
#'
#' @details
#' The \code{desc()} function estimates the descriptive statistics, 
#' in tables for one or more independent (exogenous) variables on 
#' a dependent(endogenous) variable. It acts only on numerical
#' variables.
#' For example for the variable `x`:
#' 
#' \loadmathjax
#' 
#' * `1st. quartile:`
#' \mjsdeqn{Q_1 = (n + 1) x 1/4}
#' 
#' * `2nd. quartile or Median:`
#' \mjsdeqn{md =  (n + 1) x 2/4}
#' 
#' * `3rd Qu.:`
#' \mjsdeqn{Q_3 = (n + 1) x 3/4}
#' 
#' * `Arithmetic mean:`
#' \mjsdeqn{\bar{x}=\frac{1}{n} \sum_{i=i}^{n} x_{i}}
#' 
#' * `Range:`
#' \mjsdeqn{R_x = \max(x) - \min(x)}
#' 
#' * `Variance`:   
#' \mjsdeqn{\sigma_{x}^2 = \frac{\sum_{i=1}^n(x_i-\bar{x})^2}{n} 
#' }
#'    
#' * `Standard deviation:`
#' \mjsdeqn{sd_x = \sqrt{\frac{\sum_{i} (x_{i} - \mu)^2}{n}}}
#'                   
#' * `SEM or SE.mean`, the standard error of the mean is
#' calculated simply by taking the standard deviation and 
#' dividing it by the square root of the sample size: 
#' \mjsdeqn{SEM_x = \frac{sd(x)}{\sqrt{n}}}
#' 
#' * `coef.var or coefficient of variation:`
#' \mjsdeqn{CV = \frac{sd(x)}{\bar{x} }\times 100}
#' 
#' @param datap The data set
#' @param resp an integer value indicating the column 
#' in \code{datap} that corresponds to the response variable.
#' @importFrom stats cor lm pt sd setNames
#' @importFrom pastecs stat.desc
#' @importFrom grDevices dev.new
#'
#' @seealso \code{correlation},  \code{multiple linear regression},
#  lavaan and diagram packages for drawing path diagrams.
#' @return Returns a list of 3 objects:
#' \describe{
#' \item{desc1}{Descriptive statistics1 of input data}
#' \item{desc2}{Descriptive statistics2 of input data}
#' \item{corcf}{A table of correlation coefficients}
#' }
#' @usage desc(datap, resp)
#'
#' @examples
#' \donttest{
#' data(dtsimp)
#' desc(dtsimp, 1)
#' }
#' @examples
#' \donttest{
#' data(dtraw)
#' desc(dtraw[, -1], 1)
#' 
#' data(heart)
#' desc(heart, 2)
#' }
#' 
#' @references 
#' Bhattacharyya GK and Johnson RA 1997. Statistical Concepts 
#' and Methods, John Wiley and Sons, New York.
#' 
#' Draper N and Smith H 1981. Applied Regression Analysis, 
#' John Wiley & Sons, New York.
#' 
#' Neter, J, Whitmore, GA, Wasserman, W 1992. Applied Statistics.
#' Allyn & Bacon, Incorporated, ISBN 10: 0205134785 / ISBN 13:
#' 9780205134786. 
#' 
#' Snedecor, G.W., Cochran, W.G. 1980. Statistical Methods. 
#' Iowa State University Press.
#' 
#' @export

desc <- function(datap, resp)
{
  datap <- dataprep(datap)

   ni <- ncol(datap) - 1  
  lend <- nrow(datap)   
  
  if (ni == 1) {
    sd.x <- sd(datap[, -resp])
  } else {
    sd.x <- sapply((datap[, -resp]), sd, na.rm = T)
  }

  if (ni > 1)
  {
    corind <- cor(datap[, -resp])
  } else {
    corind = 1
    corind = as.matrix(corind)
  }
  cordep <- cor(datap[, -resp], datap[, resp]) 
  
  desc1 <- summary(datap)
  desc2 <- stat.desc(datap)
  corcf <- cor(datap)
  
  results <- list(desc1, desc2, corcf)
  results <- setNames(results, c("Descriptive statistics:", "Descriptive statistics:", "Correlation coefficients:"))
  
  class(results) <- "list"
  return(results)
}
