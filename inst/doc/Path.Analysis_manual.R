## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = '70%'
)

## ----install Path.Analysis package--------------------------------------------
if(!require('Path.Analysis')){
    install.packages('Path.Analysis')
}
library('Path.Analysis')

## ----setup,warning=FALSE,message=FALSE----------------------------------------
library(car)
library(stats)
library(Hmisc)
library(pastecs)
library(devtools)
library(usethis)
library(testthat)
library(knitr)
library(rmarkdown)
    
## For graphical displays
library(metan)
library(ComplexHeatmap)
library(grDevices)
library(DiagrammeR)

## ----Prepare and show the dtsimp data and perform the 1st path diagram, echo = FALSE, fig.height=4, fig.width=5----

data(dtsimp)
corr(dtsimp, verbose = FALSE)
reg(dtsimp, 1, verbose = FALSE)
matdiag(dtsimp, 1)

## ----Prepare and Showe the dtraw dataset, message=FALSE, warning=FALSE--------
data(dtraw)
dtraw <- as.data.frame(dtraw)
rownames(dtraw) <- dtraw[, 1]
dtraw[, 1] <- NULL
head(dtraw[1:4, ])

## ----Correlogram of dtraw dataset, excluding the first column on the left-----
data(dtsimp)
cor_plot(dtsimp)

## ----include=FALSE------------------------------------------------------------

old.par <- par(no.readonly = TRUE)
on.exit(par(old.par))
par(mar=rep(1, 4))
knitr::opts_chunk$set(par(mar=rep(2, 4)))


## ----Perform the  path diagram, echo = FALSE, fig.height=4, fig.width=5-------

old.par <- par(no.readonly = TRUE)
on.exit(par(old.par))
par(mar=rep(1, 4))
matdiag(dtraw, 1)


## ----response is between independents-----------------------------------------
data(heart)
desc(heart, 2)
# matdiag(heart, 2)

## ----Corrplot diagram of dtseq1 data, echo = FALSE, fig.height=4, fig.width=5----
# YLD v.s FS, DFT, FW, FV
data(dtseq)
dtseq1 <- dtseq[, c(2, 4, 3, 6, 5)]  
head(dtseq1)
matdiag(dtseq1, 1)

## ----Network plot of the dtseq1 data, echo = FALSE, fig.height=4, fig.width=5----
network.plot(dtraw2)

## ----A Heatmap plot of the dtraw data, echo = FALSE, fig.height=4, fig.width=5----

 data(dtraw2)
 heat_map(dtraw2)

## ----echo = FALSE-------------------------------------------------------------

old.par <- par(no.readonly = TRUE)
on.exit(par(old.par))
par(mar=rep(1, 4))

data(dtraw2)
dtraw2 <- scale(as.data.frame(dtraw2))
dtraw2 <- as.matrix(dtraw2)

# Define some graphics to display the distribution of columns
.hist = anno_histogram(dtraw2, gp = gpar(fill = "lightblue"))

.density = anno_density(dtraw2, type = "line", gp = gpar(col = "blue"))

ha_mix_top = HeatmapAnnotation(
  hist = .hist, density = .density,
  height = unit(3.8, "cm")
)
# Define some graphics to display the distribution of ows
.violin = anno_density(dtraw2, type = "violin", 
 gp = gpar(fill = "lightblue"), which = "row")

.boxplot = anno_boxplot(dtraw2, which = "row")

ha_mix_right = HeatmapAnnotation(violin = .violin, bxplt = .boxplot,
 which = "row", width = unit(4, "cm"))

# Combine annotation with heatmap

Heatmap(dtraw2, name = "dtraw2", 
        column_names_gp = gpar(fontsize = 8),
        top_annotation = ha_mix_top) + ha_mix_right


## ----Prepare and show the dtseq2 data and perform the  path diagram, echo = FALSE, fig.height=4, fig.width=5----
dtseq2 <- dtseq[, c(4, 8, 7)]  
head(dtseq2)
matdiag(dtseq2, 1)

## ----Prepare and show the dtseq3 data and perform the path diagram, echo = FALSE, fig.height=4, fig.width=5----
dtseq3 <- dtseq[, c(3, 8, 7)]   
head(dtseq3)
matdiag(dtseq3, 1)

## ----Prepare and show the dtseq4 data and perform the  path diagram, echo = FALSE, fig.height=4, fig.width=5----
dtseq4 <- dtseq[, c(6, 8, 7)]  
head(dtseq4)
matdiag(dtseq4, 1)

## ----corrplot of the dtseq5 data, echo = FALSE, fig.height=4, fig.width=5-----
dtseq5 <- dtseq[, c(5, 8, 7)]  
head(dtseq5)
matdiag(dtseq5, 1)

## ----Network plot of the dtseq6 data, echo = FALSE, fig.height=4, fig.width=5----
dtseq6 <- dtseq[, c(7, 8)]  
head(dtseq6)
matdiag(dtseq6, 1)

## -----------------------------------------------------------------------------
data(dtseqr)
dtseqr <- as.data.frame(dtseqr)
dtseqr[, 1] <- as.factor(dtseqr[, 1])  # Rep
dtseqr[, 2] <- as.factor(dtseqr[, 2])  # Genotypes

f <- lm(cbind(YLD, DFT, FS, FV, FW, DFL, FLP) ~ Rep + Genotypes, dtseqr)

summary(Anova(f))  # all results for MANOVA

# Anova(f)$SSPE    # individual printing SSCP matrix of error

Anova(f)$SSPE[4:5, 4:5] # SSCP matrix of error for two dependent variables i.e Fv and FW.

## -----------------------------------------------------------------------------
ru1u2 <- Anova(f)$SSPE[4, 5]/(  sqrt(Anova(f)$SSPE[4, 4])*sqrt(Anova(f)$SSPE[5, 5]))
cat("\nCorrelation coefficient between residuals is:\n", ru1u2)

## ----echo = FALSE-------------------------------------------------------------

grViz("
      digraph SEM {
      
graph [layout = dot,
overlap = true,
outputorder = edgesfirst]
node [shape = rectangle]
      
a [pos = '0, 0!', label = 'FLP', shape = ellipse, style = filled, color = orange]
b [pos = '0, 1!', label = 'DFL', shape = ellipse, style = filled, color = orange]
c [pos = '3, 1!', label = 'DFT', shape = ellipse, style = filled, color = green]
d [pos = '3, 2!', label = 'FS', shape = ellipse, style = filled, color = green]
e [pos = '3, -1!', label = 'FV', shape = ellipse, style = filled, color = green]
f [pos = '3, -2!', label = 'FW', shape = ellipse, style = filled, color = green]
g [pos = '5, 0!', label = 'YLD', shape = rectangle, style = filled, color = cyan]
h [pos = '6.5, 0!', label = 'Residual', shape = plaintext]

a->b [label = '0.36ns', color=red, penwidth = 4]   
a->c [label = '-0.11ns', color=blue, penwidth = 2]
a->d [label = '-0.12ns', color=blue, penwidth = 2]
a->e [label = '0.19ns', color=blue, penwidth = 3]
a->f [label = '0.37ns', color=blue, penwidth = 4]

b->c [label = '0.35ns', color=red, penwidth = 4]
b->d [label = '0.49ns', color=red, penwidth = 4.5]
b->e [label = '0.04ns', color=red, penwidth = 1]
b->f [label = '0.19ns', color=red, penwidth = 2]

c->g [label = '0.21ns', color=black, penwidth = 3]
d->g [label = '-0.39ns', color=black, penwidth = 4]
e->g [label = '0.37ns', color=black, penwidth = 4]
f->g [label = '0.08ns', color=black, penwidth = 1]

h->g [label = '0.77', color=blueviolet, penwidth = 5] # residual effect
      }
      ")

## ----echo = FALSE-------------------------------------------------------------

grViz("
      digraph SEM {
      
graph [layout = dot,
overlap = true,
outputorder = edgesfirst]
      
node [shape = rectangle]
      
 a [pos = '0, 0!', label = 'X1', shape = ellipse, style = filled, color = orange]
 b [pos = '0, 1!', label = 'X2', shape = ellipse, style = filled, color = orange]

 e [pos = '1, 0!', label = 'Y2', shape = rectangle, style = filled, color = green]
 f [pos = '1, 1!', label = 'Y1', shape = rectangle, style = filled, color = green]
 i [pos = '2, 0!', label = 'Residual1', shape = plaintext]
 j [pos = '2, 1!', label = 'Residual2', shape = plaintext]

a -> b [label = '0.36ns', color=blue, penwidth = 3.0, dir=both]
a -> e [label = '0.19ns', color=blue, penwidth = 1.0]
a -> f [label = '0.37ns', color=blue, penwidth = 4.0]

b -> e [label = '0.04ns', color=red, penwidth = 1.0]
b -> f [label = '0.19ns', color=red, penwidth = 2.0]

i -> e [label = '0.98', color=blueviolet, penwidth = 4.5] # residual effect
j -> f [label = '0.91', color=blueviolet, penwidth = 4.0] # residual effect

i -> j [label = '0.26', color=black, penwidth = 3.0, dir=both]
  }
      ")

