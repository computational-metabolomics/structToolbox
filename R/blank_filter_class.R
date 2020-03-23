#' Blank filter
#'
#' Filters features based on the features present in blank samples. The median
#' intensity of the samples is compared to the median intensity of the blank
#' samples. Any sample not sufficiently more intense than the blank is removed.
#' This is a wrapper for the blank filter in the PMP package.
#'
#' @templateVar paramNames c('factor_name')
#' @template common_params
#'
#' @return A struct model object for applying a blank filter
#'
#' @examples
#' D = iris_DatasetExperiment()
#' M = blank_filter(fold_change=2,
#'                  factor_name='Species',
#'                  blank_label='setosa',
#'                  qc_label='versicolor')
#' M = model_apply(M,D)
#' @inheritParams pmp::filter_peaks_by_blank
#' @param ... additional slots and values passed to struct_class
#' @export blank_filter
blank_filter = function(fold_change=20,blank_label='blank',qc_label='QC',factor_name,fraction_in_blank=0,...) {
   out=struct::new_struct('blank_filter',
        fold_change=fold_change,
        blank_label=blank_label,
        qc_label=qc_label,
        factor_name=factor_name,
        fraction_in_blank=fraction_in_blank,
        ...)
    return(out)
}


.blank_filter<-setClass(
    "blank_filter",
    contains = c('model'),
    slots=c(fold_change='entity',
        blank_label='entity',
        qc_label='entity',
        factor_name='entity',
        fraction_in_blank='entity',
        filtered='entity',
        flags='entity'
    ),
    prototype=list(name = 'Blank filter',
        description = 'Filters features by comparing the median intensity of blank samples to the median intensity of samples. Features where the intensity is not large compared to the blank are removed.',
        type = 'filter',
        predicted = 'filtered',
        libraries='pmp',
        .params=c('fold_change','blank_label','qc_label','factor_name','fraction_in_blank'),
        .outputs=c('filtered','flags'),

        blank_label=ents$blank_label,
        qc_label=ents$qc_label,
        factor_name=ents$factor_name,

        fold_change=entity(name = 'Fold change threhsold',
            description = 'Features with median intensity less than FOLD_CHANGE times the median intensity of blanks are removed.',
            value = 20,
            type='numeric'),
        fraction_in_blank=entity(name='Fraction in blank',
            description='Remove features only if they occur in a sufficient proportion of the blanks',
            type='numeric',
            value=0),

        filtered=ents$filtered,
        flags=ents$flags
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("blank_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        smeta=D$sample_meta
        x=D$data
        # remove = NULL does not remove blanks. ANY VALUE removes blanks.
        blank_filtered = pmp::filter_peaks_by_blank(t(x), fold_change=M$fold_change,
            classes=smeta[,M$factor_name],
            blank_label=M$blank_label,
            qc_label=M$qc_label,
            remove_samples=FALSE,
            remove_peaks=FALSE,
            fraction_in_blank=M$fraction_in_blank
        )

        # store the flags
        flags=data.frame(attributes(blank_filtered)$flags)
        output_value(M,'flags') = data.frame(flags,stringsAsFactors = F)
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",signature=c("blank_filter","DatasetExperiment"),
    definition=function(M,D) {
        # remove the blanks. do it this way because pmp doesnt remove from class labels.
        RB = filter_smeta(mode='exclude',levels=M$blank_label,factor_name=M$factor_name)
        RB=model_apply(RB,D)
        D=predicted(RB)

        # get the flags
        flags=M$flags

        # filter
        D=D[,flags$blank_flags==1,drop=FALSE]

        # store
        M$filtered=D

        return(M)
    }
)

##### plots
#' plot for blank filter
#'
#' plots a histogram of the calculated fold change for the blank filter (median blank / median sample)
#' @import struct
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export blank_filter_hist
#' @examples
#' C = blank_filter_hist()
blank_filter_hist = function(...) {
    out=.blank_filter_hist()
    out=struct::new_struct('blank_filter_hist',...)
    return(out)
}


.blank_filter_hist<-setClass(
    "blank_filter_hist",
    contains='chart',
    prototype = list(name='Histogram of blank filter fold changes',
        description='A histogram of the calculated fold change for the blank filter (median blank / median sample)',
        type="histogram"
    )
)


#' @export
#' @template chart_plot
setMethod(f="chart_plot",

    signature=c("blank_filter_hist",'blank_filter'),
    definition=function(obj,dobj)
    {
        t=param_value(dobj,'fold_change')
        A=output_value(dobj,'flags')
        A$fold_change=log2(A$fold_change)
        A$features=factor(A$blank_flags,levels=c(1,0),labels=c('accepted','rejected'))

        out=ggplot(data=A, aes_(x=~fold_change,fill=~features)) +
            geom_histogram(boundary=log2(t),color='white') +
            xlab('log2(fold change)') +
            ylab('Count') +
            scale_fill_Publication()+
            theme_Publication(base_size = 12) +
            ggtitle('Blank filter')
        theme(panel.border = element_rect(linetype = "solid", fill = NA))

        # get the breaks
        po=ggplot_build(out)
        breaks=po$layout$panel_scales_x[[1]]$get_breaks()

        # add second axis with labels
        out=out+scale_x_continuous(breaks=breaks,sec.axis=dup_axis(labels=2^breaks,name='Fold change'))

        return(out)
    }
)

