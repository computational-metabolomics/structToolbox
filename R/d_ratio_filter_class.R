#' @eval get_description('dratio_filter')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = dratio_filter(threshold=20,qc_label='QC',factor_name='class')
#' M = model_apply(M,D)
#' @export dratio_filter
dratio_filter = function(threshold=20, qc_label='QC', factor_name, ...) {
    out=struct::new_struct('dratio_filter',
        threshold=threshold,
        qc_label=qc_label,
        factor_name=factor_name,
        ...)
    return(out)
}

.dratio_filter<-setClass(
    "dratio_filter",
    contains = c('model'),
    slots=c(threshold='entity',
        qc_label='entity',
        factor_name='entity',
        filtered='entity',
        flags='entity',
        d_ratio='data.frame'
    ),
    prototype=list(name = 'Dispersion ratio filter',
        description = paste0('The dispersion ratio (d-ratio) compares the ',
        'standard deviation (or non-parametric equivalent) of the Quality ',
        'Control (QC) samples relative to the standard deviation (or ',
        'non-parametric equivalent) of the samples for each feature. ',
        'If the d-ratio is greater than a predefined threshold then the ',
        'observed sample variance could be due to technical variance and ',
        'the feature is removed.'),
        type = 'filter',
        predicted = 'filtered',
        .params=c('threshold','qc_label','factor_name'),
        .outputs=c('filtered','flags','d_ratio'),
        citations=list(
            bibentry(
                bibtype='Article',
                year = '2018',
                month = 'May',
                volume = 14,
                number = 6,
                author = as.person('David Broadhurst, Royston Goodacre, Stacey N. Reinke, Julia Kuligowski, Ian D. Wilson, Matthew R. Lewis, Warwick B. Dunn'),
                title = paste0('Guidelines and considerations for the use of ',
                    'system suitability and quality control samples in mass ',
                    'spectrometry assays applied in untargeted clinical ',
                    'metabolomic studies'),
                journal = 'Metabolomics'
            )
        ),

        threshold=entity(name = 'Dispersion ratio threshold',
            description = 'The threshold below which features are removed.',
            value = 20,
            type='numeric'),

        qc_label=entity(name = 'QC label',
            description = 'The label used to identify QC samples.',
            value = 'QC',
            type='character'),

        factor_name=ents$factor_name,

        filtered=entity(name = 'Filtered data',
            description = 'A DatasetExperiment object containing the filtered data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        flags=entity(name = 'Flags',
            description = 'Flag indicating whether the feature was rejected by the filter or not.',
            type='data.frame',
            value=data.frame()
        )

    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("dratio_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        # median QC samples
        QC=filter_smeta(mode='include',levels=M$qc_label,factor_name=M$factor_name)
        QC = model_apply(QC,D)
        QC = predicted(QC)$data
        QC=apply(QC,2,mad,na.rm=TRUE)

        # median samples
        S=filter_smeta(mode='exclude',levels=M$qc_label,factor_name=M$factor_name)
        S = model_apply(S,D)
        S = predicted(S)$data
        S=apply(S,2,mad,na.rm=TRUE)

        d_ratio=(QC/(QC+S))*100

        OUT=d_ratio>M$threshold

        M$d_ratio=data.frame(d_ratio=d_ratio,row.names=colnames(D$data))
        M$flags=data.frame(rejected=OUT,row.names = colnames(D$data))

        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("dratio_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        # get flags
        OUT=M$flags$rejected
        # remove flagged
        D=D[,-OUT]
        # store
        M$filtered=D

        return(M)
    }
)
