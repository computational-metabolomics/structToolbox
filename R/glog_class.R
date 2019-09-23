#' glog transform
#'
#' applies a glog transform to the input data
#' @export glog_transform
#' @import pmp
#' @examples
#' M = glog_transform()
glog_transform<-setClass(
    "glog_transform",
    contains = c('method'),
    slots=c(params.qc_label='entity',
        params.factor_name='entity',
        outputs.transformed='entity',
        outputs.lambda='entity',
        outputs.error_flag='entity',
        outputs.lambda_opt='numeric'
    ),

    prototype=list(name = 'generalised logarithm transform',
        description = 'applies a glog tranform using using QC samples as reference samples.',
        type = 'normalisation',
        predicted = 'transformed',

        params.factor_name=entity(name = 'factor_name',
            description = 'Column name of sample_meta containing QC labels',
            value = 'V1',
            type='character'),

        params.qc_label=entity(name = 'QC label',
            description = 'Label used to identify QC samples.',
            value = 'QC',
            type='character'),

        outputs.transformed=entity(name = 'glog transformed dataset',
            description = 'A dataset object containing the glog transformed data.',
            type='dataset',
            value=dataset()
        ),
        outputs.lambda=entity(name = 'lambda',
            description = 'The value of lambda used, as determined by PMP package.',
            type='numeric',
            value=0
        ),
        outputs.error_flag=entity(name = 'Optimisation error',
            description = 'A logical indicating whether the glog optimisation for lambda was successful.',
            type='logical',
            value=FALSE
        )
    )
)

#' @export
#' @template method_apply
setMethod(f="method.apply",
    signature=c("glog_transform","dataset"),
    definition=function(M,D)
    {
        opt=param.list(M)

        smeta=dataset.sample_meta(D)
        x=dataset.data(D)

        out = pmp::glog_transformation(t(x),classes = smeta[,M$factor_name],qc_label=opt$qc_label,store_lambda = TRUE)
        dataset.data(D) = as.data.frame(t(out[[1]]))

        output.value(M,'transformed') = D
        M$lambda = out[[2]]
        M$lambda_opt=out[[3]]
        M$error_flag = out[[4]]

        return(M)
    }
)
