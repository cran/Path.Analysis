#' @title Creating the `Heatmap` chart
#' @name heat_map
#' @author {
#' Ali Arminian <abeyran@gmail.com>
#' }
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' * `heat_map()` draws a double-clustered `heatmap` 
#' for path coefficients analysis. Please be cautious
#' that this function acts only on numeric 
#' variables/columns (see example on `dtraw2` data set). 
#' Users for drawing other types of heatmaps may 
#' use `heatmap.3`, `ComplexHeatmap` and `pheatmap` R packages. 
#' Where an example is given in the vignette manual of this 
#' package (`Path.Analysis_manual.Rmd`)  
#'
#' @param datap The data set
#' @importFrom gplots heatmap.2
#' @importFrom grDevices dev.new
#' @importFrom graphics par
#'
#' @seealso `lavaan` and `diagram` packages for drawing path diagrams.
#' @return Returns an object of class `heatmap.2`.
#' @usage heat_map(datap)
#'
#' @examples
#' \donttest{
#' data(dtraw2)
#' dtraw2 <- scale(as.data.frame(dtraw2))
#' heat_map(dtraw2)
#' }
#' @export

heat_map <- function(datap)
{
  datap <- dataprep(datap)
  datap <- as.matrix(datap)
  
  old.par<- par(no.readonly = TRUE)
  par(mar=rep(1, 4))
  on.exit(par(old.par))
  
  p1 <- gplots::heatmap.2(x = datap, cexRow=0.5)
  
  class(p1) <- "heat_map"
  
  invisible(p1)
}