## ----setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(
    dpi=72
)
library(structToolbox)
library(gridExtra)

## --------------------------------------------------------------------------
D = iris_dataset()
summary(D)

## --------------------------------------------------------------------------
M = mean_centre() + PLSDA(number_components=2,factor_name='Species')
M

## --------------------------------------------------------------------------
XCV = kfold_xval(folds=5,factor_name='Species')
# change the number of folds
XCV$folds=10
XCV$folds

## --------------------------------------------------------------------------
models(XCV)=M
models(XCV)

## --------------------------------------------------------------------------
XCV = kfold_xval(folds=5,method='venetian',factor_name='Species') * 
      (mean_centre()+PLSDA(number_components = 2,factor_name='Species'))

## --------------------------------------------------------------------------
XCV = run(XCV,D,balanced_accuracy())
XCV$metric

## --------------------------------------------------------------------------
chart.names(XCV)

## ----warning=FALSE---------------------------------------------------------
C = kfoldxcv_grid()
chart.plot(C,XCV)

## --------------------------------------------------------------------------
P = permute_sample_order(number_of_permutations = 10) * 
    kfold_xval(folds=5,factor_name='Species')*
    (mean_centre() + PLSDA(factor_name='Species',number_components=2))
P = run(P,D,balanced_accuracy())
P$metric

