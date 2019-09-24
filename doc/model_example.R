## ----setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.align = 'center'
)
library(structToolbox)
library(gridExtra)

## --------------------------------------------------------------------------
D = iris_dataset()
head(D$data)

## --------------------------------------------------------------------------
P = PCA(number_components=15)
P$number_components=5
P$number_components

## --------------------------------------------------------------------------
param.ids(P)

## --------------------------------------------------------------------------
M = mean_centre() + PCA(number_components = 4)

## --------------------------------------------------------------------------
M[2]$number_components

## --------------------------------------------------------------------------
M = model.train(M,D)

## --------------------------------------------------------------------------
M = model.predict(M,D)

## --------------------------------------------------------------------------
output.ids(M[2])
M[2]$scores

## --------------------------------------------------------------------------
chart.names(M[2])

## --------------------------------------------------------------------------
C = pca_scores_plot(groups=D$sample_meta$Species,factor_name='Species') # colour by Species
chart.plot(C,M[2])

## --------------------------------------------------------------------------
# add petal width to emta data of pca scores
M[2]$scores$sample_meta$Petal.Width=D$data$Petal.Width
# update plot
C$factor_name='Petal.Width'
chart.plot(C,M[2])

## ----fig.width=10----------------------------------------------------------
C1 = pca_scores_plot(groups=D$sample_meta$Species,factor_name='Species') # colour by Species
g1 = chart.plot(C1,M[2])
C2 = PCA.scree()
g2 = chart.plot(C2,M[2])
grid.arrange(grobs=list(g1,g2),nrow=1)

## --------------------------------------------------------------------------
is(PCA(),'stato')

## --------------------------------------------------------------------------
# this is the stato id for PCA
stato.id(P)

# this is the stato name
stato.name(P)

# this is the stato definition
stato.definition(P)

## --------------------------------------------------------------------------
stato.summary(P)

