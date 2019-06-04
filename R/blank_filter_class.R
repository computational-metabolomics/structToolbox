#' blank filter
#'
#' filters features base on the features present in blank samples
#' @export blank_filter
#' @import pmp
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
        params.fold_change=entity(name = 'Fold change threhsold',
            description = 'Features with median intensity less than FOLD_CHANGE times the median intensity of blanks are removed.',
            value = 20,
            type='numeric'),
        params.blank_label=entity(name = 'Blank label',
            description = 'Label used to identify blank samples.',
            value = 'Blank',
            type='character'),
        params.qc_label=entity(name = 'QC label',
            description = 'Label used to identify QC samples. If not set to null then median of the QCs is used instead of all samples.',
            value = 'QC',
            type='character'),
        outputs.filtered=entity(name = 'Blank filtered dataset',
            description = 'A dataset object containing the filtered data.',
            type='dataset',
            value=dataset()
        ),
        outputs.flags=entity(name = 'Flags',
            description = 'Fold change and a flag indicating whether the feature was rejected by the filter or not.',
            type='data.frame',
            value=data.frame()
        ),
        params.factor_name=entity(name='Factor name',
            description='Name of sample meta column to use',
            type='character',
            value='V1'),
        params.fraction=entity(name='Fraction in blank',
            description='Remove features only if they occur in a sufficient proportion of the blanks',
            type='character',
            value=0)
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("blank_filter","dataset"),
    definition=function(M,D)
    {
        opt=param.list(M)
        smeta=dataset.sample_meta(D)
        x=dataset.data(D)
        # remove = NULL does not remove blanks. ANY VALUE removes blanks.
        blank_filtered = filter_peaks_by_blank(t(x), fold_change=opt$fold_change,
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
        idx=colnames(flags$blank_flags)
        vmeta=vmeta[idx[flags$blank_flags==1],,drop=FALSE]
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

