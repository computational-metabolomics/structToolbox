% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kfold_xval_charts.R
\name{kfoldxcv_metric}
\alias{kfoldxcv_metric}
\title{kfoldxcv metric plot}
\usage{
kfoldxcv_metric(...)
}
\arguments{
\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{
kfoldxcv_metric
} object. This object has no \code{output} slots.
See \code{\link[struct]{chart_plot}} in the \code{struct} package to plot this chart object.
}
\description{
A boxplot of the performance metric computed for each fold of a k-fold cross-validation.
}
\section{Inheritance}{

A \code{kfoldxcv_metric} object inherits the following \code{struct} classes: \cr\cr
\verb{[kfoldxcv_metric]} >> \verb{[chart]} >> \verb{[struct_class]}
}

\examples{
M = kfoldxcv_metric()

C = kfoldxcv_metric()
}
