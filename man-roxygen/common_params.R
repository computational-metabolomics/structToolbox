<%
# helper function to evaluate stringified R-code
evalString <- function(str) {
    eval.parent(parse(text=str))
}

# list of commonly used params and descriptions
L=list(
    'alpha'       = 'p-value threshold for determining significance. Default alpha = 0.05.',
    'mtc'         = 'multiple test correction method to apply. Can be: holm, hochberg, hommel, bonferroni, BH, BY, fdr or none',
    'formula'     = 'the formula to use in the form y~...',
    'blank_label' = "the label used to identify 'blank' samples",
    'qc_label'    = "the label used to identify 'QC' samples. If set then the median of the QC samples is used instead of all samples.",
    'factor_name' = "the column name of sample_meta to use",
    'factor_names'= "the column name(s) of sample_meta to use"
)

paramNames=evalString(paramNames)

# be sure to use `cat` since we are in a <\% and not a <\%= brew-chunk
out=lapply(paramNames,
    function(param) {
        cat(paste("#' @param", param, L[[param]]), fill = TRUE)
    })
%>
    # sometimes you need to end a template with NULL
