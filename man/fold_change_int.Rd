% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fold_change_int_class.R
\name{fold_change_int}
\alias{fold_change_int}
\title{Fold change for interactions between factors}
\usage{
fold_change_int(
  factor_name,
  threshold = 2,
  control_group = character(0),
  method = "geometric",
  conf_level = 0.95,
  ...
)
}
\arguments{
\item{factor_name}{(character) The name of a sample-meta column to use.}

\item{threshold}{(numeric) The fold change threshold for labelling features as significant. The default is \code{2}.\cr}

\item{control_group}{(character) The level names of the groups used in the denominator (where possible) when computing fold change. One level for each factor, assumed to be in the same order as factor_name. The default is \code{character(0)}.}

\item{method}{(character) Fold change method. Allowed values are limited to the following: \itemize{ \item{\code{"geometric"}: A log transform is applied before using group means to calculate fold change. In the non-tranformedspace this is equivalent to using geometric group means. Confidence intervals for independant and paired sampling are estimated using standard error of the mean in log transformed space before being transformed back to the original space.}\item{\code{"median"}: The group medians and the method described by Price and Bonett is used to estimate confidence intervals. For paired data standard error of the median is used to estimate confidence intervals from the median fold change of all pairs.}\item{\code{"mean"}: The group means and the method described by Price and Bonnet is used to estimate confidence intervals. For paired data standard error of the mean is used to estimate confidence intervals from the mean fold change of all pairs.}} The default is \code{"geometric"}.}

\item{conf_level}{(numeric) The confidence level of the interval. The default is \code{0.95}.\cr}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{fold_change_int} object with the following \code{output} slots:
\tabular{ll}{
\code{fold_change} \tab          (data.frame) The fold change between groups. \cr
\code{lower_ci} \tab          (data.frame) Lower confidence interval for fold change. \cr
\code{upper_ci} \tab          (data.frame) Upper confidence interval for fold change. \cr
\code{significant} \tab          (data.frame) A logical indictor of whether the calculated fold change including the estimated confidence limits is greater than the selected threshold. \cr
}
}
\description{
For more than one factor the fold change calculation is extended to include all combinations of levels (interactions) of all factors. Paired fold changes are not possible for this computation.
}
\section{Inheritance}{

A \code{fold_change_int} object inherits the following \code{struct} classes: \cr\cr
\verb{[fold_change_int]} >> \verb{[fold_change]} >> \verb{[model]} >> \verb{[struct_class]}
}

\examples{
M = fold_change_int(
      factor_name = "V1",
      sample_name = character(0),
      threshold = 2,
      control_group = character(0),
      method = "geometric",
      paired = FALSE,
      conf_level = 0.95)

D = MTBLS79_DatasetExperiment()
D=D[,1:10,drop=FALSE]
M = filter_smeta(mode='exclude',levels='QC',factor_name='Class') +
    fold_change_int(factor_name=c('Class','Batch'))
M = model_apply(M,D)
}
\references{
Lloyd GR, Jankevics A, Weber RJM (2020). "struct: an
R/Bioconductor-based framework for standardized metabolomics data
analysis and beyond." \emph{Bioinformatics}, \emph{36}(22-23), 5551-5552.
\url{https://doi.org/10.1093/bioinformatics/btaa1031}.
}
