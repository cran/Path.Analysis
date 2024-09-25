## Summary

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```
R package **Path.Analysis** provides a comprehensive textual and illustrative analysis on raw data or a correlation matrix
to extract correlation coefficients, path direct and indirect effect along with testing direct effects.
Later, it draws 3 kinds of correlation plot, diagram and a Heatmap. 

'Path.Analysis' is very easy to use and provides a good plotting options in 
visualization method, graphic layout, color, legend, text labels, etc. 
It also provides p-values of direct effects to help users determine the 
statistical significance of the correlations and direct effects.

For examples, see its
#[online vignette](https://github.com/abeyran/Path.Analysis).


This package is licensed under the MIT license, and available on CRAN:
<https://cran.r-project.org/package=Path.Analysis>.



## Basic examples

```r
library(Path.Analysis)
data(dtsimp)
Path.Analysis(dtsimp, 1, rplot = FALSE, rdend = FALSE)
```

```r
library(Path.Analysis)
data(dtraw)
Path.Analysis(dtraw, 1, rplot = TRUE, rdend = FALSE)
```

## Download and Install

To download the release version of the package on CRAN, type the following at the R command line:

```r
install.packages('Path.Analysis')
```

To download the development version of the package, type the following at the R command line:

```r
devtools::install_github('abeyran/Path.Analysis', build_vignettes = TRUE)
```

## How to cite

To cite `corrplot` properly, call the R built-in command
`citation('Path.Analysis')` as follows:

```r
citation('Rapth')
```

## Reporting bugs and other issues

If you encounter a clear bug, please file a minimal reproducible example on 
[github](https://github.com/abeyran/Path.Analysis/issues).

