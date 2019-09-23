#' missing value filter (samples)
#'
#' filters samples based on the percent number of missing values
#' @export mv_sample_filter
#' @import pmp
#' @examples
#' C = mv_sample_filter()
mv_sample_filter<-setClass(
    "mv_sample_filter",
    contains = c('method'),
    slots=c(params.mv_threshold='entity',
        outputs.filtered='entity',
        outputs.flags='entity'
    ),
    prototype=list(name = 'Missing value sample filter',
        description = 'Filters by removing samples where the percent number of missing values exceeds the threshold.',
        type = 'filter',
        predicted = 'filtered',
        params.mv_threshold=entity(name = 'Missing value threshold (%)',
            description = 'Samples with greather than THRESHOLD% missing values are excluded.',
            value = 20,
            type='numeric'),
        outputs.filtered=entity(name = 'Filtered dataset',
            description = 'A dataset object containing the filtered data.',
            type='dataset',
            value=dataset()
        ),
        outputs.flags=entity(name = 'Flags',
            description = '% missing values and a flag indicating whether the sample was rejected.',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @export
#' @template method_apply
setMethod(f="method.apply",
    signature=c("mv_sample_filter","dataset"),
    definition=function(M,D)
    {
        opt=param.list(M)

        smeta=dataset.sample_meta(D)
        x=dataset.data(D)

        filtered = filter_samples_by_mv(x,max_perc_mv=opt$mv_threshold/100,D$sample_meta[,1])
        dataset.data(D) = as.data.frame(t(filtered$df))

        flags<-data.frame(filtered$flags)
        smeta<-smeta[flags$flags==1,,drop=FALSE]
        dataset.sample_meta(D)=smeta

        output.value(M,'filtered') = D
        output.value(M,'flags') = flags

        return(M)
    }
)


##### plots
#' plot for missing value sample filter
#'
#' plots a histogram of % missing values per sample
#' @import struct
#' @export mv_sample_filter.hist
#' @examples
#' C = mv_sample_filter.hist()
mv_sample_filter.hist<-setClass(
    "mv_sample_filter.hist",
    contains='chart',
    prototype = list(name='Histogram of missing values per sample',
        description='A histogram of the % missing values per sample',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("mv_sample_filter.hist",'mv_sample_filter'),
    definition=function(obj,dobj)
    {
        t=param.value(dobj,'mv_threshold')
        A=output.value(dobj,'flags')
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

