#' @eval get_description('mv_sample_filter')
#' @export mv_sample_filter
#' @examples
#' C = mv_sample_filter()
mv_sample_filter = function(mv_threshold=20,...) {
    out=struct::new_struct('mv_sample_filter',
        mv_threshold=mv_threshold,
        ...)
    return(out)
}


.mv_sample_filter<-setClass(
    "mv_sample_filter",
    contains = c('model'),
    slots=c(mv_threshold='entity',
        filtered='entity',
        flags='entity',
        percent_missing='entity'
    ),
    prototype=list(name = 'Missing value sample filter',
        description = paste0(
            'Removes samples where the ',
        'percent number of missing values exceeds a threshold.'),
        type = 'filter',
        predicted = 'filtered',
        libraries='pmp',
        .params=c('mv_threshold'),
        .outputs=c('filtered','flags','percent_missing'),

        mv_threshold=entity(name = 'Missing value threshold (%)',
            description = paste0(
                'The maximum percentage of features with missing ',
                'values in a sample.'),
            value = 20,
            type='numeric'),
        filtered=entity(name = 'Filtered DatasetExperiment',
            description = 
                'A DatasetExperiment object containing the filtered data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        flags=entity(name = 'Flags',
            description = paste0(
                'A flag indicating whether the sample was rejected. ',
                '0 = rejected.'),
            type='data.frame',
            value=data.frame()
        ),
        percent_missing=entity(name = 'Percent missing',
            description = '% missing values for each sample.',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("mv_sample_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        # inputs
        opt=param_list(M)
        # meta data
        smeta=D$sample_meta
        # data
        x=D$data
        # apply filter
        filtered = pmp::filter_samples_by_mv(x,max_perc_mv=opt$mv_threshold/100,D$sample_meta[,1],remove_samples = FALSE)
        flags<-data.frame(attributes(filtered)$flags)
        output_value(M,'flags') = data.frame('flags'=flags[,2],row.names = rownames(x))
        output_value(M,'percent_missing')=data.frame('precent_missing'=flags[,1],row.names = rownames(x))
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("mv_sample_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        flags=M$flags
        # remove samples
        RM=filter_by_name(
            mode='exclude',
            dimension='sample',
            names=rownames(flags)[flags[,1]==0]
        )
        RM=model_apply(RM,D)
        # fill output slots
        output_value(M,'filtered') = predicted(RM)
        return(M)
    }
)

##### plots
#' @eval get_description('mv_sample_filter_hist')
#' @import struct
#' @export mv_sample_filter_hist
#' @examples
#' C = mv_sample_filter_hist()
mv_sample_filter_hist = function(...) {
    out=struct::new_struct('mv_sample_filter_hist',...)
    return(out)
}


.mv_sample_filter_hist<-setClass(
    "mv_sample_filter_hist",
    contains='chart',
    prototype = list(name='Histogram of missing values per sample',
        description='A histogram of the the proportion of missing values per sample',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("mv_sample_filter_hist",'mv_sample_filter'),
    definition=function(obj,dobj)
    {
        t=param_value(dobj,'mv_threshold')
        A=output_value(dobj,'flags')
        A$perc_mv=(dobj$percent_missing[,1])*100
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

