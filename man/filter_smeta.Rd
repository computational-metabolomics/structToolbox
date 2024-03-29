% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_smeta_class.R
\name{filter_smeta}
\alias{filter_smeta}
\title{Filter by sample meta data}
\usage{
filter_smeta(mode = "include", levels, factor_name, ...)
}
\arguments{
\item{mode}{(character) Mode of action. Allowed values are limited to the following: \itemize{ \item{\code{"include"}: Samples in the specified levels are retained.}\item{\code{"exclude"}: Samples in the specified levels are excluded.}} The default is \code{"include"}.}

\item{levels}{(character) The level name(s) for filtering.}

\item{factor_name}{(character) The name of a sample-meta column to use.}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{filter_smeta} object with the following \code{output} slots:
\tabular{ll}{
\code{filtered} \tab          (DatasetExperiment)  \cr
}
}
\description{
The data is filtered by so that the named levels of a factor are included/excluded from the dataset.
}
\section{Inheritance}{

A \code{filter_smeta} object inherits the following \code{struct} classes: \cr\cr
\verb{[filter_smeta]} >> \verb{[model]} >> \verb{[struct_class]}
}

\examples{
M = filter_smeta(
      mode = "include",
      levels = character(0),
      factor_name = "V1")

D = MTBLS79_DatasetExperiment()
M = filter_smeta(mode='exclude',levels='QC',factor_name='QC')
M = model_apply(M,D)
}
