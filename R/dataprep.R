#' @name dataprep
#' @title Data preparation
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Prepares data for analyses
#' @param  datap dataset
#' @return Returns a data frame
#' @export


dataprep <- function(datap)
{
  datap <- data.frame(datap)

  for (i in seq_along(datap))
  {
    datap[, i][is.na(datap[, i])] <- mean(datap[, i], na.rm = TRUE)
    datap[, i] <- as.numeric(unlist(datap[, i]))
  }

  class(datap) <- "data.frame"
  return(datap)
  invisible(datap)
}
