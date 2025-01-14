% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor_plot.R
\name{cor_plot}
\alias{cor_plot}
\title{Drawing the correlogram}
\usage{
cor_plot(datap)
}
\arguments{
\item{datap}{The data set}
}
\value{
Returns an object of class \verb{gg, ggmatrix}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
\itemize{
\item \code{corr_plot()} draws a correlogram for data
}
}
\examples{
\donttest{
data(dtsimp)
cor_plot(dtsimp)
}

}
\references{
Olivoto, T, and A Dal’Col Lúcio. 2020. “Metan: An r Package
for Multi‐environment Trial Analysis.” Methods in Ecology and
Evolution, 11(6): 783–89. https://doi.org/10.1111/2041-210
X.13384.
}
\seealso{
\code{correlogram}, \code{diagram}, and \code{lavaan} package for drawing path diagrams.
}
\author{
{
Ali Arminian \href{mailto:abeyran@gmail.com}{abeyran@gmail.com}
}
}
