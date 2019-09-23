#' Blank filter
#'
#' Filters features based on the features present in blank samples. The median
#' intensity of the samples is compared to the median intensity of the blank
#' samples. Any sample not sufficiently more intense than the blank is removed.
#' This is a wrapper for the blank filter in the PMP package.
#' @import pmp
#'
#' @param fold_change the threshold for filtering samples
#' @param fraction max proportion of blanks a feature can be present in
#'
#' @templateVar paramNames c('blank_label','qc_label','factor_name')
#' @template common_params
#'
#' @return A STRUCT method object with functions for applying a blank filter
#'
#' @examples
#' D = iris_dataset()
#' M = blank_filter(fold_change=2,
#'                  factor_name='Species',
#'                  blank_label='setosa',
#'                  qc_label='versicolor')
#' M = method.apply(M,D)
#'
#' @export blank_filter
blank_filter<-setClass(
    "blank_filter",
    contains = c('method'),
    slots=c(params.fold_change='entity',
        params.blank_label='entity',
        params.qc_label='entity',
        params.factor_name='entity',
        params.fraction='entity',
        outputs.filtered='entity',
        outputs.flags='entity'
    ),
    prototype=list(name = 'Blank filter',
        description = 'Filters features by comparing the median intensity of blank samples to the median intensity of samples. Features where the intensity is not large compared to the blank are removed.',
        type = 'filter',
        predicted = 'filtered',

        params.blank_label=ents$blank_label,
        params.qc_label=ents$qc_label,
        params.factor_name=ents$factor_name,

        params.fold_change=entity(name = 'Fold change threhsold',
            description = 'Features with median intensity less than FOLD_CHANGE times the median intensity of blanks are removed.',
            value = 20,
            type='numeric'),
        params.fraction=entity(name='Fraction in blank',
            description='Remove features only if they occur in a sufficient proportion of the blanks',
            type='numeric',
            value=0),

        outputs.filtered=ents$filtered,
        outputs.flags=ents$flags
    )
)

#' @export
#' @template method_apply
setMethod(f="method.apply",
    signature=c("blank_filter","dataset"),
    definition=function(M,D)
    {
        opt=param.list(M)
        smeta=dataset.sample_meta(D)
        x=dataset.data(D)
        # remove = NULL does not remove blanks. ANY VALUE removes blanks.
        blank_filtered = pmp::filter_peaks_by_blank(t(x), fold_change=opt$fold_change,
            classes=smeta[,opt$factor_name],
            blank_label=opt$blank_label,
            qc_label=opt$qc_label,
            remove=FALSE,
            fraction_in_blank=opt$fraction
        )
        dataset.data(D) = as.data.frame(t(blank_filtered$df))

        # remove the blanks. do it this way because pmp doesnt remove from class labels.
        RB = filter_smeta(mode='exclude',levels=opt$blank_label,factor_name=opt$factor_name)
        RB=method.apply(RB,D)
        D=predicted(RB)

        flags=data.frame(blank_filtered$flags)
        vmeta=dataset.variable_meta(D)
        vmeta=vmeta[flags$blank_flags==1,,drop=FALSE]
        dataset.variable_meta(D)=vmeta

        output.value(M,'filtered') = D
        output.value(M,'flags') = data.frame(blank_filtered$flags,stringsAsFactors = F)
        return(M)
    }
)


##### plots
#' plot for blank filter
#'
#' plots a histogram of the calculated fold change for the blank filter (median blank / median sample)
#' @import struct
#' @export blank_filter.hist
#' @examples
#' C = blank_filter.hist()
blank_filter.hist<-setClass(
    "blank_filter.hist",
    contains='chart',
    prototype = list(name='Histogram of blank filter fold changes',
        description='A histogram of the calculated fold change for the blank filter (median blank / median sample)',
        type="histogram"
    )
)

#' @export
setMethod(f="chart.plot",

    signature=c("blank_filter.hist",'blank_filter'),
    definition=function(obj,dobj)
    {
        t=param.value(dobj,'fold_change')
        A=output.value(dobj,'flags')
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

