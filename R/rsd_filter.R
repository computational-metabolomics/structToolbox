#' rsd filter
#'
#' Filters features based on the relative standard deviation (RSD) for the QC samples.
#' @param rsd_threshold Features with RSD greater than the threshold are removed.
#' @param qc_label The label used to identify QC samples in the chosen sample_meta column.
#' @param factor_name The name of the sample_meta column containing QC labels.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export rsd_filter
#' @examples
#' M = rsd_filter(factor_name='class')
#'
rsd_filter = function(rsd_threshold=20,qc_label='QC',factor_name,...) {
    out=struct::new_struct('rsd_filter',
        rsd_threshold=rsd_threshold,
        qc_label=qc_label,
        factor_name=factor_name,
        ...)
    return(out)
}

.rsd_filter<-setClass(
    "rsd_filter",
    contains = c('model'),
    slots=c(rsd_threshold='entity',
        qc_label='entity',
        factor_name='entity',
        filtered='entity',
        flags='entity',
        rsd_qc='entity'
    ),
    prototype=list(name = 'RSD filter',
        description = 'Filters features by calculating the relative standard deviation (RSD) for the QC samples and removing features with RSD greater than the threshold.',
        type = 'filter',
        predicted = 'filtered',
        libraries='pmp',
        .params=c('rsd_threshold','qc_label','factor_name'),
        .outputs=c('filtered','flags','rsd_qc'),

        rsd_threshold=entity(name = 'RSD threhsold',
            description = 'Features with RSD greater than the threshold are removed.',
            value = 20,
            type='numeric'),

        qc_label=entity(name = 'QC label',
            description = 'Label used to identify QC samples.',
            value = 'QC',
            type='character'),

        factor_name=entity(name='Factor name',
            description='Name of sample meta column to use',
            type='character',
            value='V1'),

        filtered=entity(name = 'RSD filtered DatasetExperiment',
            description = 'A DatasetExperiment object containing the filtered data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        flags=entity(name = 'Flags',
            description = 'RSD and a flag indicating whether the feature was rejected by the filter or not.',
            type='data.frame',
            value=data.frame()
        ),
        rsd_qc=entity(name = 'RSD',
            description = 'The calculated RSD of the QC class',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("rsd_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)
        smeta=D$sample_meta
        x=D$data
        rsd_filtered = pmp::filter_peaks_by_rsd(t(x), max_rsd = opt$rsd_threshold, classes=smeta[[opt$factor_name]], qc_label=opt$qc_label,remove_peaks=FALSE)

        flags<-attributes(rsd_filtered)$flags
        D=D[,flags[,2]==1]

        output_value(M,'filtered') = D
        output_value(M,'flags') = data.frame('rsd_flags'=flags[,2])
        output_value(M,'rsd_qc') = data.frame('rsd_qc'=flags[,1])
        return(M)
    }
)


##### plots
#' plot for rsd filter
#'
#' plots a histogram of the calculated RSD for the RSD filter
#' @import struct
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export rsd_filter_hist
#' @examples
#' C = rsd_filter_hist()
#'
rsd_filter_hist = function(...) {
    out=struct::new_struct('rsd_filter_hist',...)
    return(out)
}


.rsd_filter_hist<-setClass(
    "rsd_filter_hist",
    contains='chart',
    prototype = list(name='Histogram of RSD for the QC samples',
        description='A histogram of the calculated RSD for QC samples.',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("rsd_filter_hist",'rsd_filter'),
    definition=function(obj,dobj)
    {
        t=param_value(dobj,'rsd_threshold')
        A=output_value(dobj,'flags')
        A$rsd_qc=log2(dobj$rsd_qc[,1])
        A$features=factor(A$rsd_flags,levels=c(1,0),labels=c('accepted','rejected'))

        out=ggplot(data=A, aes_(x=~rsd_qc,fill=~features)) +
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

