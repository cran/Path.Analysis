#' @title Direct and Indirect Effects Matrices and Diagram
#' @name matdiag
#' @author {
#' Ali Arminian <abeyran@gmail.com>
#' }
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' * `matdiag()` extracts the direct effect and indirect effects
#'  matrices of data in path analysis along with the significance
#'  of direct effects where direct effects are shown as a
#'  vector (columnar matrix of 1*n dimensions and indirect
#'  effects are off-diagonal effects. Later, draws a diagram
#'  for path coefficient analysis based on the `DiagrammeR`
#'   package.
#'
#' @details
#' The `matdiag` function estimates the direct and indirect effects in path
#' coefficient analysis as tables along with drawing the diagram of path analysis.
#' This is apparently the only program testing the significance of direct effects
#' in a path analysis. Note: all variables must be numeric for matrix calculations
#' and the next plotting.
#' * In a path model, path coefficients or direct effects (Pi's) indicate
#' the direct effects of a variable on another, and are standardized partial
#' regression coefficients (in Wright's terminology) due they are estimated
#' from correlations or from the transformed (standardized) data as:
#'
#' \loadmathjax
#' \mjsdeqn{P_i = \beta_i\frac{\sigma_{X_i}}{\sigma_Y}
#' }
#' * The path equations are as follows:
#' * One dependent variable:
#' \mjsdeqn{P_1 + P_2r_{12} + P_3r_{13} + ... + P_nr_{1n} = rY_1}
#' \mjsdeqn{P_1r_{21} + P_2 + P_3r_{23} + ... + P_nr_{2n} = rY_2}
#' \mjsdeqn{...}
#' \mjsdeqn{P_1rn_1 + P_2r_{n2} + P_3r_{n3} + ... + P_n = rY_n}
#'
#' * Extension to more dependent variables:
#'   `Path.Analysis` is capable of performing this straightforward
#'   function through detailed explanations. The linear regression 
#'   model with a single response in its form is as follows (Johnson 
#'   and Wichern (2007):
#'   \mjseqn{Y = \beta_0 + \beta_1Z_1 + ... + \beta_rZ_r + \epsilon}
#'   
#'   where the multivariate multiple linear regression model is as follows:
#' \mjsdeqn{Y_1 = \beta_0 + \beta_1Z_{11} + \beta_2Z{12} + ... + \beta_rZ_{1r} + \epsilon_1}
#' \mjsdeqn{Y_2 = \beta_0 + \beta_1Z_{21} + \beta_2Z{22} + ... + \beta_rZ_{2r} + \epsilon_2}
#' \mjsdeqn{...}
#' \mjsdeqn{Y_n = \beta_0 + \beta_1Z_{n1} + \beta_2Z{n2} + ... + \beta_rZ_{nr} + \epsilon_n}
#'   
#'   As stated by Bondari (1990), for two dependent variables \mjseqn{Y_1} 
#'   and \mjseqn{Y_2}:
#' \mjsdeqn{ Y_1 = p_1X_1 + p_2X_2 + p_3X_3 + ... + p_nX_n }
#' \mjsdeqn{ Y_2 = p'_1X_1 + p'_2X_2 + p'_3X_3 + ... + p'_nX_n }
#' \mjsdeqn{ ... }
#'  
#' where:
#' \mjsdeqn{
#' r_{Y_1Y_2} = p_1p'_1 + p_2p'_2 + p_3p'_3 + ... +
#' p_np'_n + \sigma_{i=j}p_ip'_1r_{ij} = \sigma_{i,j}p_ip'_ir_{ij}
#' }
#' @param datap The data set
#' @param resp The response variable
#' @param verbose If `verbose = TRUE` then some results are printed
#'
#' @import corrplot DiagrammeR
#' @importFrom stats cor lm pt sd setNames
#' @importFrom corrplot cor.mtest
#'
#' @seealso `correlation`,  `multiple linear regression`,
#' and matrix notations in mathematics.
#' @seealso `lavaan` and `diagrammeR` packages for 
#' drawing path diagrams
#' @return Returns a list with three objects
#' \describe{
#' \item{direff}{a data frame of direct effects}
#' \item{matall}{a matrix of direct and indirect effects}
#' \item{Residual}{a constant of residuals}
#' }
#' @usage matdiag(datap, resp, verbose = FALSE)
#'
#' @examples
#' \donttest{
#' data(dtsimp)
#' matdiag(dtsimp, 1, verbose = FALSE)
#'
#' data(dtraw)
#' matdiag(dtraw[, -1], 1, verbose = FALSE)
#'
#' data(heart)
#' matdiag(heart, 2, verbose = FALSE)
#'  }
#'
#' @references
#' Arminian, A, MS Kang, M Kozak, S Houshmand, and P
#' Mathews. 2008. “MULTPATH: A Comprehensive Minitab
#' Program for Computing Path Coefficients and Multiple
#' Regression for Multivariate Analyses.” Journal of Crop
#' Improvement, 22(1): 82–120.
#'
#' Bondari, K. 1990. "PATH ANALYSIS IN AGRICULTURAL RESEARCH,"
#' Conference on Applied Statistics in Agriculture. https://do
#' i.org/10.4148/2475-7772.1439
#'
#' Cramer, C.S, TC Wehner, and SB Donaghy. 1999. “PATHSAS: A SAS
#' Computer Program for Path Coefficient Analysis of Quantitative
#' Data.” Journal of Heredity, 90(1): 260–62. https://doi.org/10
#' .1093/jhered/90.1.260.
#' 
#' Johnson, R.A., Wichern, D.W. 2007. Applied Multivariate Statistical 
#' Analysis. Prentice Hall, USA.  
#'
#' Li, C.C. 1975. Path Analysis: A Primer. Boxwood Pr. 346 p.
#' 
#' Olivoto, T, and A Dal’Col Lúcio. 2020. “Metan: An r Package
#' for Multi‐environment Trial Analysis.” Methods in Ecology and
#' Evolution, 11(6): 783–89. https://doi.org/10.1111/2041-210
#' X.13384.
#'
#' Wolfle, LM. 2003. “The Introduction of Path Analysis
#' to the Social Sciences, and Some Emergent Themes:
#' An Annotated Bibliography.” Structural Equation Modeling,
#' 10(1): 1–34.
#'
#' Wright, S. 1923. “The Theory of Path Coefficients
#' a Reply to Niles’s Criticism.” Genetics, 8(3): 239.
#'
#' ———. 1934. “The Method of Path Coefficients.” The Annals
#' of Mathematical Statistics, 5(3): 161–215.
#'
#' ———. 1960. “Path Coefficients and Path Regressions:
#' Alternative or Complementary Concepts?” Biometrics, 16(2):
#' 189–202.
#'
#' @export


matdiag <- function(datap, resp, verbose = FALSE)
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
  }
  corind = as.matrix(corind)

  cordep <- cor(datap[, -resp], datap[, resp])

  Ps <- solve(corind, cordep)
  matall <- corind

  for (i in 1:ni)
  {
    for (j in 1:ni)
    {
      matall[i, j] <- Ps[j] * corind[i, j]
    }
  }


  Ps2 <- as.vector(Ps)
  if (ni > 1)
  {
    diagps <- as.matrix(diag(Ps2))
    matall <- corind %*% diagps
  } else {
    diagps = Ps2
  }

  SEmean.x <- sd.x/(sqrt(lend))
  Ttest.sd <- Ps/sd.x
  TtestSEmean <- abs(Ps/SEmean.x)

  p.valSE <- as.numeric()

  for (i in 1:ni)
  {
    p.valSE[i] <- 1-pt(TtestSEmean[i], df = lend - 1)
  }

  direff <- as.table(cbind(Ps, TtestSEmean, p.valSE))
  direff <- round(direff, digits=4)

  sig <- c()
  for(i in 1:nrow(direff)){
    if(direff[i, 3] > 0.05) {
      sig[i] <- "ns"
    }
    else if(direff[i, 3] < 0.05 & direff[i, 3] > 0.01) {
      sig[i] <- "*"
    }
    else if (direff[i, 3] < 0.01) {
      sig[i] <- "**"
    }
    else if(direff[i, 3] < 0.01 & direff[i, 3] > 0.001) {
      sig[i] <- "***"
    }
  }

  direff <- data.frame(cbind(direff, sig))
  colnames(direff) <- c("Diret effect(s)", "t-test", "p(>|t|)", "sig.")
  rownames(direff) <- rownames(corind)
  rownames(matall) <- rownames(corind)
  colnames(matall) <- rownames(corind)

  psry <- t(Ps) %*% cordep
  Residual <- round(sqrt(1 - psry), 4)

  matall <- round(matall, digits=4)
  lres <- paste(Residual,":Residual")

  ldireff <- paste(direff[, 1], direff[, 4], sep=" ")

  results <- list(direff, matall, Residual)
  results <- setNames(results, c("Direct effect(s) matrix:", "Direct(Diagonal) and indirect(off-diagonal) effect(s) matrix:", "Residual effect:"))
  
  class(results) <- "list"
  
  if(verbose) {
    cat("Results of the path coefficient analysis:", "\n") 
    print(results) 
  }
  
  datap <- as.data.frame(datap)
  label <- c(colnames(datap[resp]), colnames(datap[-resp]))

  # create plot nodes
  nodf <- DiagrammeR::create_node_df(n = ncol(datap),
   type = c(colnames(datap[resp]),
   colnames(datap[-resp])),
   label <- label,
   shape = c("rectangle",
   rep("circle", (ncol(datap)-1))),
   style = "empty",
   fontsize = 12,
   fixedsize = TRUE,
   height = .75,
   width = 1.0,
   penwidth = 1.0,
   color = "blue"
  )

  # create plot edges
  edgf <- DiagrammeR::create_edge_df(
    from = c(1:ncol(datap)),
    to = rep(1, ncol(datap)),
    label = c(lres, ldireff),
    font = 2,
    fontsize = 10,
    minlen = 6,
    color = "red"
  )

  # Create graph with the ndf and edf
  
  graph <- DiagrammeR::create_graph(nodes_df = nodf, edges_df = edgf)
   graph %>%
   render_graph(output = "graph",
   layout = NULL,  as_svg = FALSE,
   title = "Diagram of the Path Coefficient Analysis",
   width = NULL, height = NULL)

}
