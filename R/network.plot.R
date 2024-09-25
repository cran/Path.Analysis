#' @title Network plot
#' @name network.plot
#' @author {
#' Ali Arminian <abeyran@gmail.com>
#' }
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' * `network.plot()` draws the network plot of path coefficients analysis
#'
#' @details
#' The `network.plot()` draws a correlogram and a heatmap 
#' for data, if requested by user
#'
#' @param datap The data set
#' @importFrom grDevices dev.new
#' @importFrom graphics par
#' @importFrom corrr network_plot correlate
#'
#' @seealso `correlogram`, `diagram`, and `lavaan` packages 
#' for drawing path diagrams.
#' @return Returns an object of class `network_plot`.
#' @usage network.plot(datap)
#'
#' @examples
#' \donttest{
#' data(dtraw2)
#' network.plot(dtraw2)
#' }
#' 
#' @references 
#' Kuhn et al. 2022. corrr package. doi: 	<10.32614/CRAN.package.corrr>
#' https://github.com/tidymodels/corrr
#' @export

network.plot <- function(datap)
{
  datap <- dataprep(datap)

  old.par<- par(no.readonly = TRUE)
  par(mar=(rep(2, 4)))
  on.exit(par(old.par))

      p1 <- datap%>%
      correlate() %>%
      corrr::network_plot(min_cor = 0.2)
    
    return(p1)
}
