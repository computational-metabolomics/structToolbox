#' glog transform
#'
#' applies a glog transform to the input data
#' @export glog_transform
#' @import pmp
#' @examples
#' M = glog_transform()
glog_transform = function(...) {
    out=.glog_transform()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.glog_transform<-setClass(
    "glog_transform",
    contains = c('model'),
    slots=c(params_qc_label='entity',
        params_factor_name='entity',
        outputs_transformed='entity',
        outputs_lambda='entity',
        outputs_error_flag='entity',
        outputs_lambda_opt='numeric'
    ),

    prototype=list(name = 'generalised logarithm transform',
        description = 'applies a glog tranform using using QC samples as reference samples.',
        type = 'normalisation',
        predicted = 'transformed',

        params_factor_name=entity(name = 'factor_name',
            description = 'Column name of sample_meta containing QC labels',
            value = 'V1',
            type='character'),

        params_qc_label=entity(name = 'QC label',
            description = 'Label used to identify QC samples.',
            value = 'QC',
            type='character'),

        outputs_transformed=entity(name = 'glog transformed DatasetExperiment',
            description = 'A DatasetExperiment object containing the glog transformed data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        outputs_lambda=entity(name = 'lambda',
            description = 'The value of lambda used, as determined by PMP package.',
            type='numeric',
            value=0
        ),
        outputs_error_flag=entity(name = 'Optimisation error',
            description = 'A logical indicating whether the glog optimisation for lambda was successful.',
            type='logical',
            value=FALSE
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("glog_transform","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)

        smeta=D$sample_meta
        x=D$data

        out = pmp::glog_transformation(t(x),classes = smeta[,M$factor_name],qc_label=opt$qc_label,store_lambda = TRUE)
        D$data = as.data.frame(t(out[[1]]))

        output_value(M,'transformed') = D
        M$lambda = out[[2]]
        M$lambda_opt=out[[3]]
        M$error_flag = out[[4]]

        return(M)
    }
)
