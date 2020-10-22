#' @eval get_description('ANOVA')
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
    contains = c('model','stato'),
    slots=c(rsd_threshold='entity',
        qc_label='entity',
        factor_name='entity',
        filtered='entity',
        flags='entity',
        rsd_qc='entity'
    ),
    prototype=list(name = 'RSD filter',
        description = paste0(
            'An RSD filter calculates the relative standard deviation (the ',
            'ratio of the mean to the standard deviation) for all features. ',
            'Any feature with an RSD lower than a predefined threshold is ',
            'excluded.'),
        stato_id='STATO:0000236',
        type = 'filter',
        predicted = 'filtered',
        libraries='pmp',
        .params=c('rsd_threshold','qc_label','factor_name'),
        .outputs=c('filtered','flags','rsd_qc'),

        rsd_threshold=entity(name = 'RSD threhsold',
            description = 'The RSD threshold below which features are removed.',
            value = 20,
            type='numeric'),

        qc_label=entity(name = 'QC label',
            description = 'The label used to identify QC samples.',
            value = 'QC',
            type='character'),

        factor_name=ents$factor_name,

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
#' @eval get_description('rsd_filter_hist')
#' @import struct
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
    prototype = list(
        name='RSD histogram',
        description='A histogram of the calculated RSD values.',
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

