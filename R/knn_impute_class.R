#' @eval get_description('knn_impute')
#' @export knn_impute
#' @examples
#' M = knn_impute()
knn_impute = function(neighbours=5,sample_max=50,feature_max=50,by='features',...) {
    out=struct::new_struct('knn_impute',
        neighbours=neighbours,
        sample_max=sample_max,
        feature_max=feature_max,
        by=by,
        ...)
    return(out)
}


.knn_impute<-setClass(
    "knn_impute",
    contains = c('model','stato'),
    slots=c(neighbours='entity',
        sample_max='entity',
        feature_max='entity',
        imputed='entity',
        by='enum'
    ),

    prototype=list(name = 'kNN missing value imputation',
        description = paste0('k-nearest neighbour missing value imputation ',
        'replaces missing values in the data with the average of a predefined ',
        'number of the most similar neighbours for which the value is present'),
        type = 'normalisation',
        predicted = 'imputed',
        libraries='pmp',
        stato_id='STATO:0000523',
        .params=c('neighbours','feature_max','sample_max','by'),
        .outputs=c('imputed'),

        neighbours=entity(name = 'Number of neighbours',
            description = 'The number of neighbours (k) to use for imputation.',
            value = 5,
            type='numeric'),

        feature_max=entity(name = 'Maximum percent per feature',
            description = 'The maximum percent missing values per feature.',
            value = 50,
            type='numeric'),

        sample_max=entity(name = 'Maximum percent per sample',
            description = 'The maximum percent missing values per sample.',
            value = 50,
            type='numeric'),

        imputed=entity(name = 'Imputed DatasetExperiment',
            description = 'A DatasetExperiment object containing the data where missing values have been imputed.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        
        by=enum(name = 'By sample or by feature',
            description = 'Impute using similar "samples" or "features". Default features.',
            type='character',
            value="features",
            allowed=c('samples','features')
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
        x=as.matrix(D$data)
        
        if (M$by == 'features') {
            x=t(x)
        }
        

        imputed = pmp::mv_imputation(x,
            method='knn',
            k = opt$neighbours,
            rowmax=opt$feature_max/100,
            colmax=opt$sample_max/100,
            maxp = NULL,
            check_df = FALSE)
        
        if (M$by =='features') {
            imputed=t(imputed)   
        }

        rownames(imputed)=rownames(D$data)
        colnames(imputed)=colnames(D$data)
        
        D = DatasetExperiment(data=imputed,sample_meta=D$sample_meta,variable_meta=D$variable_meta)
        output_value(M,'imputed') = D

        return(M)
    }
)
