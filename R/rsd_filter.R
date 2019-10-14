#' rsd filter
#'
#' filters features based on the relative standard deviation (RSD) for the QC samples
#' @export rsd_filter
#' @import pmp
#' @examples
#' M = rsd_filter()
#'
rsd_filter<-setClass(
    "rsd_filter",
    contains = c('model'),
    slots=c(params.rsd_threshold='entity',
        params.qc_label='entity',
        params.factor_name='entity',
        outputs.filtered='entity',
        outputs.flags='entity'
    ),
    prototype=list(name = 'RSD filter',
        description = 'Filters features by calculating the relative standard deviation (RSD) for the QC samples and removing features with RSD greater than the threshold.',
        type = 'filter',
        predicted = 'filtered',

        params.rsd_threshold=entity(name = 'RSD threhsold',
            description = 'Features with RSD greather than the threshold are removed.',
            value = 20,
            type='numeric'),

        params.qc_label=entity(name = 'QC label',
            description = 'Label used to identify QC samples.',
            value = 'QC',
            type='character'),

        params.factor_name=entity(name='Factor name',
            description='Name of sample meta column to use',
            type='character',
            value='V1'),

        outputs.filtered=entity(name = 'RSD filtered dataset',
            description = 'A dataset object containing the filtered data.',
            type='dataset',
            value=dataset()
        ),
        outputs.flags=entity(name = 'Flags',
            description = 'RSD and a flag indicating whether the feature was rejected by the filter or not.',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model.apply",
    signature=c("rsd_filter","dataset"),
    definition=function(M,D)
    {
        opt=param.list(M)
        smeta=dataset.sample_meta(D)
        x=dataset.data(D)
        rsd_filtered = filter_peaks_by_rsd(t(x), max_rsd = opt$rsd_threshold, classes=smeta[[opt$factor_name]], qc_label=opt$qc_label)
        dataset.data(D) = as.data.frame(t(rsd_filtered$df))

        flags<-data.frame(rsd_filtered$flags)
        vmeta=dataset.variable_meta(D)
        vmeta=vmeta[flags[,2]==1,,drop=FALSE]
        dataset.variable_meta(D)=vmeta

        output.value(M,'filtered') = D
        output.value(M,'flags') = data.frame(rsd_filtered$flags,stringsAsFactors = F)
        return(M)
    }
)


##### plots
#' plot for rsd filter
#'
#' plots a histogram of the calculated RSD for the RSD filter
#' @import struct
#' @export rsd_filter.hist
#' @examples
#' C = rsd_filter.hist()
#'
rsd_filter.hist<-setClass(
    "rsd_filter.hist",
    contains='chart',
    prototype = list(name='Histogram of RSD for the QC samples',
        description='A histogram of the calculated RSD for QC samples.',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("rsd_filter.hist",'rsd_filter'),
    definition=function(obj,dobj)
    {
        t=param.value(dobj,'rsd_threshold')
        A=output.value(dobj,'flags')
        A$rsd_QC=log2(A$rsd_QC)
        A$features=factor(A$rsd_flags,levels=c(1,0),labels=c('accepted','rejected'))

        out=ggplot(data=A, aes_(x=~rsd_QC,fill=~features)) +
            geom_histogram(boundary=log2(t),color='white') +
            xlab('log2(RSD), QC samples') +
            ylab('Count') +
            scale_fill_Publication()+
            theme_Publication(base_size = 12) +
            ggtitle('RSD filter')+
            theme(panel.border = element_rect(linetype = "solid", fill = NA))

        # get the breaks
        po=ggplot_build(out)
        breaks=po$layout$panel_scales_x[[1]]$get_breaks()

        # add second axis with labels
        out=out+scale_x_continuous(breaks=breaks,sec.axis=dup_axis(labels=2^breaks,name='RSD, QC samples'))

        return(out)
    }
)

