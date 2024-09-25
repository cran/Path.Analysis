#' @name Path.Analysis
#' @title Path Coefficient Analysis
#' @docType package
#' @author {
#' Ali Arminian <abeyran@gmail.com>
#' }
#' @description
#' \pkg{Path.Analysis} does descriptive statistics on dataset
#' and importantly graphical representation of data such as drawing
#' heatmaps, correlogram and path diagram.
NULL
#'
#' Dataset 1: a dependent (y) and 3 independent(x1 to x3) variables.
#'
#' @name dtsimp
#' @docType data
#' @usage data(dtsimp)
#' @keywords datasets
#' @format A `data.frame` with 105 observations of 4 variables.
#' \describe{
#' \item{` y`}{a numeric vector}
#' \item{`x1`}{a numeric vector}
#' \item{`x2`}{a numeric vector}
#' \item{`x3`}{a numeric vector}
#' }
#' @examples
#' \donttest{
#' library(Path.Analysis)
#' data(dtsimp)
#' }
NULL


#' Dataset 2: a number of 9 traits measured on 35 Camelina DH lines.
#'
#' @name dtraw
#' @docType data
#' @usage data(dtraw)
#' @keywords datasets
#' @format A `data.frame` with 35 observations of 9 variables.
#' \describe{
#' \item{`DH lines`}{a character vector}
#' \item{`y`}{a numeric vector}
#' \item{`X1`}{a numeric vector}
#' \item{`X2`}{a numeric vector}
#' \item{`X3`}{a numeric vector}
#' \item{`X4`}{a numeric vector}
#' \item{`X5`}{a numeric vector}
#' \item{`X6`}{a numeric vector}
#' \item{`X7`}{a numeric vector}
#' \item{`X8`}{a numeric vector}
#' }
#' @examples
#' \donttest{
#' library(Path.Analysis)
#' data(dtraw)
#' }
NULL

#' Dataset 3: a number of 9 traits measured on 35 Camelina DH lines.
#'
#' @name dtraw2
#' @docType data
#' @usage data(dtraw2)
#' @keywords datasets
#' @format A `data.frame` with 35 observations of 9 variables.
#' \describe{
#' \item{`DH lines`}{a character vector considered as rownames}
#' \item{`y`}{a numeric vector}
#' \item{`X1`}{a numeric vector}
#' \item{`X2`}{a numeric vector}
#' \item{`X3`}{a numeric vector}
#' \item{`X4`}{a numeric vector}
#' \item{`X5`}{a numeric vector}
#' \item{`X6`}{a numeric vector}
#' \item{`X7`}{a numeric vector}
#' \item{`X8`}{a numeric vector}
#' }
#' @examples
#' \donttest{
#' library(Path.Analysis)
#' data(dtraw2)
#' }
NULL


#' Dataset 4: a dataframe consisting of 7 variables measured on 8 observations.
#' @name dtseq
#' @docType data
#' @usage data(dtseq)
#' @keywords datasets
#' @format A `data.frame` with 8 observations of 7 variables.
#' \describe{
#' \item{`Genotypes`}{a character vector}
#' \item{`YLD`}{a numeric vector}
#' \item{`DFT`}{a numeric vector}
#' \item{`FS`}{a numeric vector}
#' \item{`FV`}{a numeric vector}
#' \item{`FW`}{a numeric vector}
#' \item{`DFL`}{a numeric vector}
#' \item{`FLP`}{a numeric vector}
#' }
#' @examples
#' \donttest{
#' library(Path.Analysis)
#' data(dtseq)
#' }
NULL


#' Dataset5
#'
#' @name dtseqr
#' @docType data
#' @usage data(dtseqr)
#' @keywords datasets
#' @format A `data.frame` with 24 observations of 7 variables.
#' \describe{
#' \item{`Genotypes`}{a character vector}
#' \item{`Rep`}{a numeric vector}
#' \item{`YLD`}{a numeric vector}
#' \item{`DFT`}{a numeric vector}
#' \item{`FS`}{a numeric vector}
#' \item{`FV`}{a numeric vector}
#' \item{`FW`}{a numeric vector}
#' \item{`DFL`}{a numeric vector}
#' \item{`FLP`}{a numeric vector}
#' }
#' @examples
#' \donttest{
#' library(Path.Analysis)
#' data(dtseqr)
#' }
NULL

#' Dataset 6: Heart Disease data set
#'
#' A mixed variable dataset containing 14 variables of 297 patients for
#' their heart disease diagnosis.
#'
#' @name heart
#' @docType data
#' @usage data(heart)
#' @keywords datasets
#' @format A `data.frame` including 297 rows and 14 variables:
#' \describe{
#' \item{age}{Age in years (numerical).}
#' \item{sex}{Sex: 1 = male, 0 = female (logical).}
#' \item{`heart.disease`}{a numeric vector as dependent.}
#' \item{`biking`}{a numeric vector as the first independent.}
#' \item{`smoking`}{a numeric vector as the 2nd independent.}
#' }
#' @source The data set is belong to machine learning repository of UCI.
#' The original data set includes 303 patients with 6 NA's. After removing
#' missing values, it reduced into 297 patients.
#'
#' <https://archive.ics.uci.edu/ml/datasets/Heart+Disease>
#'
#' @references Lichman, M. (2013). UCI machine learning repository.

#' @examples
#' \donttest{
#' library(Path.Analysis)
#' data(heart)
#' }
NULL
