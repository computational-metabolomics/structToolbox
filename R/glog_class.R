#' glog transform
#'
#' applies a glog transform to the input data
#' @export glog_transform
#' @import pmp
glog_transform<-setClass(
    "glog_transform",
    contains = c('method'),
    slots=c(params.qc_label='entity',
        params.factor_name='entity',
        outputs.transformed='entity'
    ),

    prototype=list(name = 'generalised logarithm transform',
        description = 'applies a glog tranform using using QC samples as reference samples.',
        type = 'normalisation',
        predicted = 'transformed',

        params.qc_label=entity(name = 'QC label',
            description = 'Label used to identify QC samples.',
            value = 'QC',
            type='character'),

        outputs.transformed=entity(name = 'glog transformed dataset',
            description = 'A dataset object containing the glog transformed data.',
            type='dataset',
            value=dataset()
        )
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("glog_transform","dataset"),
    definition=function(M,D)
    {
        opt=param.list(M)

        smeta=dataset.sample_meta(D)
        x=dataset.data(D)

        out = glog_transformation(t(x),classes = smeta[,M$factor_name],qc_label=opt$qc_label)
        dataset.data(D) = as.data.frame(t(out))

        output.value(M,'transformed') = D

        return(M)
    }
)
