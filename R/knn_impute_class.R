#' knn missing value imputation
#'
#' applies a k-nearest neighbour approach to impute missing values
#' @param ... slots and values for the new object 
#' @export knn_impute
#' @import pmp
#' @examples
#' M = knn_impute()
knn_impute = function(...) {
    out=.knn_impute()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.knn_impute<-setClass(
    "knn_impute",
    contains = c('model'),
    slots=c(params_neighbours='entity',
        params_sample_max='entity',
        params_feature_max='entity',
        outputs_imputed='entity'
    ),

    prototype=list(name = 'kNN missing value imputation',
        description = 'k-nearest neighbour missing value imputation.',
        type = 'normalisation',
        predicted = 'imputed',

        params_neighbours=entity(name = 'Number of neighbours',
            description = 'The number of neighbours (k) to use ofr imputation of missing values.',
            value = 5,
            type='numeric'),

        params_feature_max=entity(name = 'Maximum percent per feature',
            description = 'The maximum percent missing values per sample',
            value = 50,
            type='numeric'),

        params_sample_max=entity(name = 'Maximum percent per sample',
            description = 'The maximum percent missing values per sample',
            value = 50,
            type='numeric'),

        outputs_imputed=entity(name = 'Imputed DatasetExperiment',
            description = 'A DatasetExperiment object containing the data where missing values have been imputed.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        )
    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("knn_impute","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)

        smeta=D$sample_meta
        x=D$data

        imputed = mv_imputation(t(as.matrix(x)),method='knn',k = opt$neighbours,rowmax=opt$feature_max/100,colmax=opt$sample_max/100,maxp = NULL,FALSE)
        D$data = as.data.frame(t(imputed))

        output_value(M,'imputed') = D

        return(M)
    }
)
