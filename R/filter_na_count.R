#' filter_na_count class
#'
#' Filters features by the number of NA per class
#'
#' @param threshold the maximum number of NA allowed per level of factor_name
#' @param factor_name the sample_meta column name to use
#'
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = filter_na_count(threshold=3,factor_name='class')
#' M = model_apply(M,D)
#'
#' @param ... additional slots and values passed to struct_class
#' @return struct object
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
    prototype=list(name = 'filters features by the number of NA per class',
        description = 'Filters by removing features where the number of features in any class exceeds the threshold',
        type = 'filter',
        predicted = 'filtered',
        .params=c('threshold','factor_name'),
        .outputs=c('filtered','count','na_count','flags'),

        factor_name=entity(name='Factor name',
            type='character',
            description='Name of sample_meta column to use'
        ),

        threshold=entity(name = 'Count threshold (%)',
            description = 'Features with less than THRESHOLD missing values in any class are excluded.',
            value = 2,
            type='numeric'),


        filtered=entity(name = 'Filtered DatasetExperiment',
            description = 'A DatasetExperiment object containing the filtered data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        count=entity(name = 'Count per class',
            description = 'Number of non-NA per class',
            type='data.frame',
            value=data.frame()
        ),
        na_count=entity(name = 'NA count per class',
            description = 'Number of NA per class',
            type='data.frame',
            value=data.frame()
        ),
        flags=entity(name = 'Flags',
            description = 'a flag indicating whether the sample was rejected.',
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
            na_count[,k]=apply(D$data,2,function(x) sum(!is.na(x[IF==L[k]])))
            count[,k]=apply(D$data,2,function(x) sum(is.na(x[IF==L[k]])))
        }
        colnames(na_count)=L
        colnames(count)=L
        rownames(na_count)=colnames(D$data)
        rownames(count)=colnames(D$data)

        flags=apply(na_count,1,function(x) any(x<M$threshold))


        M$flags=data.frame(flags=flags)
        M$count=as.data.frame(na_count)
        M$na_count=as.data.frame(count)

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
