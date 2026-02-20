# structToolbox

An extensive set of data (pre-)processing and analysis methods and tools
for metabolomics and other omics, with a strong emphasis on statistics
and machine learning.

This toolbox allows the user to build extensive and standardised
workflows for data analysis. The methods and tools have been implemented
using class-based templates provided by the `struct` (*St*atistics in
*R* *U*sing *C*lass-based *T*emplates) package. The toolbox includes
pre-processing methods (e.g. signal drift and batch correction,
normalisation, missing value imputation and scaling), univariate
(e.g. ttest, various forms of ANOVA, Kruskal–Wallis test and more) and
multivariate statistical methods (e.g. PCA and PLS, including
cross-validation and permutation testing) as well as machine learning
methods (e.g. Support Vector Machines). The STATistics Ontology (STATO)
has been integrated and implemented to provide standardised definitions
for the different methods, inputs and outputs.

## Installation

To install this package:

    if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

    BiocManager::install("structToolbox")

To install the development version:

    if (!require("remotes", quietly = TRUE))
        install.packages("remotes")

    remotes::install_github("computational-metabolomics/structToolbox")
