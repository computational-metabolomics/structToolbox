# structToolbox

An extensive set of data (pre-)processing and analysis methods and 
tools for metabolomics and other omics, with a strong emphasis on statistics and 
machine learning. 

This toolbox allows the user to build extensive and 
standardised workflows for data analysis. The methods and tools have been 
implemented using class-based templates provided by the `struct` (<u>St</u>atistics 
in <u>R</u> <u>U</u>sing <u>C</u>lass-based <u>T</u>emplates) package. 
The toolbox includes pre-processing 
methods (e.g. signal drift and batch correction, normalisation, missing value 
imputation and scaling), univariate (e.g. ttest, various forms of ANOVA, 
Kruskal–Wallis test and more) and multivariate statistical methods (e.g. PCA and
PLS, including cross-validation and permutation testing) as well as machine 
learning methods (e.g. Support Vector Machines). The 
STATistics Ontology (STATO) has been integrated and implemented to provide 
standardised definitions for the different methods, inputs and outputs.

## Installation

To install this package:
    
```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("structToolbox")
```

To install the development version:
    
```
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("computational-metabolomics/structToolbox")
```

<!-- badges: start -->
[![BioC version](https://img.shields.io/badge/dynamic/yaml?url=https%3A%2F%2Fbioconductor.org%2Fconfig.yaml&query=%24.release_version&label=Bioconductor)](http://www.bioconductor.org)
[![BioC status](http://www.bioconductor.org/shields/build/release/bioc/structToolbox.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/structToolbox)
<!-- badges: end -->
    