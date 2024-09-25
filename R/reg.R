#' @title Multiple Linear Regression
#' @name reg
#' @author {
#' Ali Arminian <abeyran@gmail.com>
#' }
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' * `reg()` performs a multiple linear regression analysis with extracting the attributed parameters
#'
#' @details
#'  The `reg` function fits a multiple linear regression analysis
#'  of one or more independent (exogenous) variables on a dependent(endogenous)
#'  variable in a linear pattern along with testing the significance of
#'  parameters. It is important that according to the type of data may produce some warning errors e.g., for dtsimp as:
#'  Warning message: In summary.lm(mlreg): essentially perfect fit: summary may be unreliable.
#'  This case is due to the intrinsic characteristics of data
#'
#' @param datap The data set
#' @param resp an integer value indicating the column in `datap` that
#' @param verbose If `verbose = TRUE` then some results are
#' printed in the console.
#'   corresponds to the response variable.
#' @importFrom stats cor lm pt sd
#' @importFrom pastecs stat.desc
#' @importFrom grDevices dev.new
#'
#' @seealso `multiple linear regression`
#'
#' @keywords multiple
#' @return An object of class list
#' @usage reg(datap, resp, verbose = FALSE)
#'
#' @examples
#' \donttest{
#' data(dtsimp)
#' reg(dtsimp, 1, verbose = FALSE)
#' }
#' @examples
#' \donttest{
#' data(heart)
#' reg(heart, 1, verbose = FALSE)
#' }
#' @export

reg <- function(datap, resp, verbose = FALSE)
{
  datap <- dataprep(datap)

  mlreg <- lm(datap[, resp] ~ ., data = datap)
  sumlreg <- summary(mlreg)
  mlsuml <- list(mlreg, sumlreg)
  class(mlsuml) <- "list"
  
  if (verbose) {
    cat("MUltiple linear regression output:\n")
    cat(".................................\n")
  } 
  return(mlsuml)
}