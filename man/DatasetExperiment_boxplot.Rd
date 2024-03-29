% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_chart_classes.R
\name{DatasetExperiment_boxplot}
\alias{DatasetExperiment_boxplot}
\title{Feature distribution histogram}
\usage{
DatasetExperiment_boxplot(
  factor_name,
  by_sample = TRUE,
  per_class = TRUE,
  number = 50,
  ...
)
}
\arguments{
\item{factor_name}{(character) The name of a sample-meta column to use.}

\item{by_sample}{(logical) Plot by sample. Allowed values are limited to the following: \itemize{ \item{\code{"TRUE"}: The data is plotted across features for a subset of samples.}\item{\code{"FALSE"}: The data is plotted across samples for a subset of features.}} The default is \code{TRUE}.\cr}

\item{per_class}{(logical) Plot per class. Allowed values are limited to the following: \itemize{ \item{\code{"TRUE"}: The data is plotted for each class.}\item{\code{"FALSE"}: The data is plotted for all samples.}} The default is \code{TRUE}.\cr}

\item{number}{(numeric, integer) The number of features/samples plotted. The default is \code{50}.\cr}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{
DatasetExperiment_boxplot
} object. This object has no \code{output} slots.
See \code{\link[struct]{chart_plot}} in the \code{struct} package to plot this chart object.

struct object
}
\description{
A boxplot to visualise the distribution of values within a subset of features.
}
\section{Inheritance}{

A \code{DatasetExperiment_boxplot} object inherits the following \code{struct} classes: \cr\cr
\verb{[DatasetExperiment_boxplot]} >> \verb{[chart]} >> \verb{[struct_class]}
}

\examples{
M = DatasetExperiment_boxplot(
      factor_name = "V1",
      by_sample = FALSE,
      per_class = FALSE,
      number = 50)

D = MTBLS79_DatasetExperiment()
C = DatasetExperiment_boxplot(factor_name='Class',number=10,per_class=FALSE)
chart_plot(C,D)
}
