#' knn missing value imputation
#'
#' Applies a k-nearest neighbour approach to impute missing values.
#' @param neighbours The number of neighbours to use for imputation.
#' @param sample_max Maximum proportion of missing values in any sample.
#' @param feature_max Maximum proportion of missing values in any feature.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export knn_impute
#' @examples
#' M = knn_impute()
knn_impute = function(neighbours=5,sample_max=0.5,feature_max=0.5,...) {
    out=struct::new_struct('knn_impute',
        neighbours=neighbours,
        sample_max=sample_max,
        feature_max=feature_max,
        ...)
    return(out)
}


.knn_impute<-setClass(
    "knn_impute",
    contains = c('model'),
    slots=c(neighbours='entity',
        sample_max='entity',
        feature_max='entity',
        imputed='entity'
    ),

    prototype=list(name = 'kNN missing value imputation',
        description = 'k-nearest neighbour missing value imputation.',
        type = 'normalisation',
        predicted = 'imputed',
        libraries='pmp',
        .params=c('neighbours','feature_max','sample_max'),
        .outputs=c('imputed'),

        neighbours=entity(name = 'Number of neighbours',
            description = 'The number of neighbours (k) to use ofr imputation of missing values.',
            value = 5,
            type='numeric'),

        feature_max=entity(name = 'Maximum percent per feature',
            description = 'The maximum percent missing values per sample',
            value = 50,
            type='numeric'),

        sample_max=entity(name = 'Maximum percent per sample',
            description = 'The maximum percent missing values per sample',
            value = 50,
            type='numeric'),

        imputed=entity(name = 'Imputed DatasetExperiment',
            description = 'A DatasetExperiment object containing the data where missing values have been imputed.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        )
    )
)


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
