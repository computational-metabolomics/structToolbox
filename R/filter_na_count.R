#' @eval get_description('filter_na_count')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = filter_na_count(threshold=3,factor_name='class')
#' M = model_apply(M,D)
#' @export filter_na_count
filter_na_count = function(threshold,factor_name,...) {
    out=struct::new_struct('filter_na_count',
        threshold=threshold,
        factor_name=factor_name,
        ...)
    return(out)
}

.filter_na_count<-setClass(
    "filter_na_count",
    contains = c('model'),
    slots=c(threshold='entity',
        factor_name='entity',
        filtered='entity',
        count='entity',
        na_count='entity',
        flags='entity'
    ),
    prototype=list(name = 'Minimum number of measured values filter',
        description = paste0('The number of measured values is counted for ',
            'each feature, and any feature with less than a predefined minimum ',
            'number of values in each group is removed. If there are several factors, then ',
            'the threshold is applied so that the minimum number of samples ',
            'is present for all combinations (interactions) of groups.'),
        type = 'filter',
        predicted = 'filtered',
        .params=c('threshold','factor_name'),
        .outputs=c('filtered','count','na_count','flags'),

        factor_name=ents$factor_name,

        threshold=entity(name = 'Missing value threshold',
            description = 'The minimum number of samples in each group/interaction.',
            value = 2,
            type='numeric'),


        filtered=entity(name = 'Filtered DatasetExperiment',
            description = 'A DatasetExperiment object containing the filtered data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        count=entity(name = 'Count per class',
            description = 'The number of measured values in each group/interaction.',
            type='data.frame',
            value=data.frame()
        ),
        na_count=entity(name = 'Missing value count',
            description = 'The number of missing values in each group/interaction.',
            type='data.frame',
            value=data.frame()
        ),
        flags=entity(name = 'Flags',
            description = 'Flags to indicate which features were removed.',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("filter_na_count","DatasetExperiment"),
    definition=function(M,D)
    {

        # create interaction factor from input factors
        IF=interaction(D$sample_meta[,M$factor_name])

        # count na per class for each feature
        L=levels(IF)
        count=na_count=matrix(0,nrow=ncol(D$data),ncol=length(L))

        for (k in 1:length(L)) {
            count[,k]=apply(D$data,2,function(x) sum(!is.na(x[IF==L[k]])))
            na_count[,k]=apply(D$data,2,function(x) sum(is.na(x[IF==L[k]])))
        }
        colnames(na_count)=L
        colnames(count)=L
        rownames(na_count)=colnames(D$data)
        rownames(count)=colnames(D$data)

        flags=apply(count,1,function(x) any(x<M$threshold))


        M$flags=data.frame(flags=flags)
        M$count=as.data.frame(count)
        M$na_count=as.data.frame(na_count)

        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("filter_na_count","DatasetExperiment"),
    definition=function(M,D)
    {
        flags=M$flags$flags

        # delete the columns
        D=D[,!flags,drop=FALSE]

        M$filtered=D
        return(M)
    }
)


#' @export
#' @template as_data_frame
setMethod(f="as_data_frame",
    signature=c("filter_na_count"),
    definition=function(M) {
        out=cbind(M$flags,M$na_count)
    }
)
