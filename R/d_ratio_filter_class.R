#' D ratio filter
#'
#' Filters features based on their D ratio, which is the ratio of technical to
#' sample variance.
#'
#'
#' @param threshold D ratio threshold. Features with a d-ratio larger than this
#' value are removed.
#' @param qc_label the label used to identify QC labels
#' @param factor_name the the sample_meta data column containing the QC labels
#'
#' @return A struct method object with functions for filtering using the d-ratio.
#'
#' @examples
#' D = sbcms_dataset()
#' M = dratio_filter(threshold=20,qc_label='QC',factor_name='class')
#' M = model.apply(M,D)
#'
#' @export dratio_filter
dratio_filter<-setClass(
    "dratio_filter",
    contains = c('model'),
    slots=c(params.threshold='entity',
        params.qc_label='entity',
        params.factor_name='entity',
        outputs.filtered='entity',
        outputs.flags='entity',
        outputs.d_ratio='data.frame'
    ),
    prototype=list(name = 'd_ratio filter',
        description = 'Filters features by calculating the d_ratio and removing features below the threshold.',
        type = 'filter',
        predicted = 'filtered',

        params.threshold=entity(name = 'd_ratio filter',
            description = 'Features with d_ratio less than the threshold are removed.',
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

        outputs.filtered=entity(name = 'd_ratio filtered dataset',
            description = 'A dataset object containing the filtered data.',
            type='dataset',
            value=dataset()
        ),
        outputs.flags=entity(name = 'Flags',
            description = 'flag indicating whether the feature was rejected by the filter or not.',
            type='data.frame',
            value=data.frame()
        )

    )
)

#' @export
#' @template model_train
setMethod(f="model.train",
    signature=c("dratio_filter","dataset"),
    definition=function(M,D)
    {
        # median QC samples
        QC=filter_smeta(mode='include',levels=M$qc_label,factor_name=M$factor_name)
        QC = model.apply(QC,D)
        QC = predicted(QC)$data
        QC=apply(QC,2,mad,na.rm=TRUE)

        # median samples
        S=filter_smeta(mode='exclude',levels=M$qc_label,factor_name=M$factor_name)
        S = model.apply(S,D)
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
setMethod(f="model.predict",
    signature=c("dratio_filter","dataset"),
    definition=function(M,D)
    {
        # get flags
        OUT=M$flags$rejected
        # remove flagged
        D$data=D$data[,-OUT]
        D$variable_meta=D$variable_meta[,-OUT]
        # store
        M$filtered=D

        return(M)
    }
)
