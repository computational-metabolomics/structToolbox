#' D ratio filter
#'
#' Filters features based on their D ratio, which is the ratio of technical to
#' sample variance.
#'
#'
#' @slot threshold D ratio threshold. Features with a d-ratio larger than this
#' value are removed.
#' @slot qc_label the label used to identify QC labels
#' @slot factor_name the the sample_meta data column containing the QC labels
#'
#' @return A struct method object with functions for filtering using the d-ratio.
#'
#' @examples
#' D = sbcms_DatasetExperiment()
#' M = dratio_filter(threshold=20,qc_label='QC',factor_name='class')
#' M = model_apply(M,D)
#'
#' @param ... slots and values for the new object
#' @return struct object
#' @export dratio_filter
dratio_filter = function(...) {
    out=.dratio_filter()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.dratio_filter<-setClass(
    "dratio_filter",
    contains = c('model'),
    slots=c(params_threshold='entity',
        params_qc_label='entity',
        params_factor_name='entity',
        outputs_filtered='entity',
        outputs_flags='entity',
        outputs_d_ratio='data.frame'
    ),
    prototype=list(name = 'd_ratio filter',
        description = 'Filters features by calculating the d_ratio and removing features below the threshold.',
        type = 'filter',
        predicted = 'filtered',

        params_threshold=entity(name = 'd_ratio filter',
            description = 'Features with d_ratio less than the threshold are removed.',
            value = 20,
            type='numeric'),

        params_qc_label=entity(name = 'QC label',
            description = 'Label used to identify QC samples.',
            value = 'QC',
            type='character'),

        params_factor_name=entity(name='Factor name',
            description='Name of sample meta column to use',
            type='character',
            value='V1'),

        outputs_filtered=entity(name = 'd_ratio filtered DatasetExperiment',
            description = 'A DatasetExperiment object containing the filtered data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        outputs_flags=entity(name = 'Flags',
            description = 'flag indicating whether the feature was rejected by the filter or not.',
            type='data.frame',
            value=data.frame()
        )

    )
)

#' @param ... slots and values for the new object
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

#' @param ... slots and values for the new object
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
