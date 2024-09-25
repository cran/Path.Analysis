#' @title Drawing the correlogram
#' @name cor_plot
#' @author {
#' Ali Arminian <abeyran@gmail.com>
#' }
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' * `corr_plot()` draws a correlogram for data
#' 
#' @param datap The data set
#' @importFrom grDevices dev.new
#' @importFrom graphics par
#' @importFrom metan corr_plot
#' 
#' @seealso `correlogram`, `diagram`, and `lavaan` package for drawing path diagrams.
#' @return Returns an object of class `gg, ggmatrix`.
#' @usage cor_plot(datap)
#'
#' @examples
#' \donttest{
#' data(dtsimp)
#' cor_plot(dtsimp)
#' }
#' 
#' @references 
#' Olivoto, T, and A Dal’Col Lúcio. 2020. “Metan: An r Package
#' for Multi‐environment Trial Analysis.” Methods in Ecology and
#' Evolution, 11(6): 783–89. https://doi.org/10.1111/2041-210
#' X.13384.
#' @export

cor_plot <- function(datap)
{
  datap <- dataprep(datap)
  
    old.par<- par(no.readonly = TRUE)
    par(mar=(rep(0.5, 4))) 
    on.exit(par(old.par))
    
    p1 <- metan::corr_plot(datap,
              upper = "corr",
              lower = "scatter",
              decimal.mark = ".",
              axis.labels = FALSE,
              show.labels.in = "show",
              size.axis.label = 12,
              size.varnames = 12,
              col.varnames = "black",
              diag = TRUE,
              diag.type = "histogram",
              bins = 20,
              col.diag = "gray",
              alpha.diag = 1,
              col.up.panel = "gray",
              col.lw.panel = "gray",
              col.dia.panel = "gray",
              prob = 0.05,
              col.sign = "green",
              alpha.sign = 0.15,
              lab.position = "tr",
              progress = NULL,
              smooth = FALSE,
              col.smooth = "red",
              confint = TRUE,
              size.point = 1,
              shape.point = 19,
              alpha.point = 0.7,
              size.line = 0.5,
              minsize = 2,
              maxsize = 3,
              pan.spacing = 0.15,
              digits = 2,
              export = FALSE,
              file.type = "pdf",
              file.name = NULL,
              width = 8,
              height = 7,
              resolution = 300)
      return(p1)
}
