#' missing value filter (samples)
#'
#' filters samples based on the percent number of missing values
#' @param ... slots and values for the new object
#' @return struct object
#' @export mv_sample_filter
#' @import pmp
#' @examples
#' C = mv_sample_filter()
mv_sample_filter = function(...) {
    out=.mv_sample_filter()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.mv_sample_filter<-setClass(
    "mv_sample_filter",
    contains = c('model'),
    slots=c(params_mv_threshold='entity',
        outputs_filtered='entity',
        outputs_flags='entity'
    ),
    prototype=list(name = 'Missing value sample filter',
        description = 'Filters by removing samples where the percent number of missing values exceeds the threshold.',
        type = 'filter',
        predicted = 'filtered',
        params_mv_threshold=entity(name = 'Missing value threshold (%)',
            description = 'Samples with greather than THRESHOLD% missing values are excluded.',
            value = 20,
            type='numeric'),
        outputs_filtered=entity(name = 'Filtered DatasetExperiment',
            description = 'A DatasetExperiment object containing the filtered data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        outputs_flags=entity(name = 'Flags',
            description = '% missing values and a flag indicating whether the sample was rejected.',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @param ... slots and values for the new object
#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("mv_sample_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)

        smeta=D$sample_meta
        x=D$data

        filtered = filter_samples_by_mv(x,max_perc_mv=opt$mv_threshold/100,D$sample_meta[,1])

        flags<-data.frame(filtered$flags)

        D=D[flags$flags==1,,drop=FALSE]

        output_value(M,'filtered') = D
        output_value(M,'flags') = flags

        return(M)
    }
)

#' @param ... slots and values for the new object
#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("mv_sample_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        M=model_apply(M,D)
        return(M)
    }
)

#' @param ... slots and values for the new object
#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("mv_sample_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        M=model_apply(M,D)
        return(M)
    }
)

##### plots
#' plot for missing value sample filter
#'
#' plots a histogram of % missing values per sample
#' @import struct
#' @param ... slots and values for the new object
#' @return struct object
#' @export mv_sample_filter_hist
#' @examples
#' C = mv_sample_filter_hist()
mv_sample_filter_hist = function(...) {
    out=.mv_sample_filter_hist()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.mv_sample_filter_hist<-setClass(
    "mv_sample_filter_hist",
    contains='chart',
    prototype = list(name='Histogram of missing values per sample',
        description='A histogram of the % missing values per sample',
        type="histogram"
    )
)

#' @param ... slots and values for the new object
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("mv_sample_filter_hist",'mv_sample_filter'),
    definition=function(obj,dobj)
    {
        t=param_value(dobj,'mv_threshold')
        A=output_value(dobj,'flags')
        A$perc_mv=(A$perc_mv)*100
        A$features=factor(A$flags,levels=c(1,0),labels=c('accepted','rejected'))
        out=ggplot(data=A, aes_(x=~perc_mv,fill=~features)) +
            geom_histogram(boundary=(t),color='white') +
            xlab('% missing values (per sample)') +
            ylab('Count') +
            scale_fill_Publication()+
            theme_Publication(base_size = 12) +
            ggtitle('Missing values')

        return(out)
    }
)

