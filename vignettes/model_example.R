## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
    dpi=72
)
library(structToolbox)
library(gridExtra)

## -----------------------------------------------------------------------------
D = iris_DatasetExperiment()
head(D$data)

## -----------------------------------------------------------------------------
P = PCA(number_components=15)
P$number_components=5
P$number_components

## -----------------------------------------------------------------------------
param_ids(P)

## -----------------------------------------------------------------------------
M = mean_centre() + PCA(number_components = 4)

## -----------------------------------------------------------------------------
M[2]$number_components

## -----------------------------------------------------------------------------
M = model_train(M,D)

## -----------------------------------------------------------------------------
M = model_predict(M,D)

## -----------------------------------------------------------------------------
output_ids(M[2])
M[2]$scores

## -----------------------------------------------------------------------------
chart_names(M[2])

## -----------------------------------------------------------------------------
C = pca_scores_plot(groups=D$sample_meta$Species,factor_name='Species') # colour by Species
chart_plot(C,M[2])

## -----------------------------------------------------------------------------
# add petal width to emta data of pca scores
M[2]$scores$sample_meta$Petal.Width=D$data$Petal.Width
# update plot
C$factor_name='Petal.Width'
chart_plot(C,M[2])

## ----fig.width=10-------------------------------------------------------------
C1 = pca_scores_plot(groups=D$sample_meta$Species,factor_name='Species') # colour by Species
g1 = chart_plot(C1,M[2])
C2 = pca_scree()
g2 = chart_plot(C2,M[2])
grid.arrange(grobs=list(g1,g2),nrow=1)

## -----------------------------------------------------------------------------
is(PCA(),'stato')

## -----------------------------------------------------------------------------
# this is the stato id for PCA
stato_id(P)

# this is the stato name
stato_name(P)

# this is the stato definition
stato_definition(P)

## -----------------------------------------------------------------------------
stato_summary(P)

## -----------------------------------------------------------------------------
D = iris_DatasetExperiment()
D

## -----------------------------------------------------------------------------
M = mean_centre() + PLSDA(number_components=2,factor_name='Species')
M

## -----------------------------------------------------------------------------
XCV = kfold_xval(folds=5,factor_name='Species')
# change the number of folds
XCV$folds=10
XCV$folds

## -----------------------------------------------------------------------------
models(XCV)=M
models(XCV)

## -----------------------------------------------------------------------------
XCV = kfold_xval(folds=5,method='venetian',factor_name='Species') * 
      (mean_centre()+PLSDA(number_components = 2,factor_name='Species'))

## -----------------------------------------------------------------------------
XCV = run(XCV,D,balanced_accuracy())
XCV$metric

## -----------------------------------------------------------------------------
chart_names(XCV)

## ----warning=FALSE------------------------------------------------------------
C = kfoldxcv_grid()
chart_plot(C,XCV)[[2]] # produces multiple figures. only plot second one.

## -----------------------------------------------------------------------------
P = permute_sample_order(number_of_permutations = 10) * 
    kfold_xval(folds=5,factor_name='Species')*
    (mean_centre() + PLSDA(factor_name='Species',number_components=2))
P = run(P,D,balanced_accuracy())
P$metric

