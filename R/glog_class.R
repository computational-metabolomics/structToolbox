#' glog transform
#'
#' applies a glog transform to the input data
#' @param ... slots and values for the new object
#' @return struct object
#' @export glog_transform
#' @import pmp
#' @examples
#' M = glog_transform()
glog_transform = function(qc_label='QC',factor_name,...) {
    out=.glog_transform()
    out=struct::.initialize_struct_class(out,
        qc_label=qc_label,
        factor_name=factor_name,
        ...)
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
        libraries = 'pmp',

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
#' @template model_train
setMethod(f="model_train",
    signature=c("glog_transform","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)

        smeta=D$sample_meta
        x=D$data

        out = pmp::glog_transformation(t(x),classes = smeta[,M$factor_name],qc_label=opt$qc_label)

        output_value(M,'transformed') = D
        output_value(M,'lambda') = attributes(out)$processing_history$glog_transformation$lambda
        output_value(M,'lambda_opt')=attributes(out)$processing_history$glog_transformation$lambda_opt
        output_value(M,'error_flag') = attributes(out)$processing_history$glog_transformation$error_flag

        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("glog_transform","DatasetExperiment"),
    definition=function(M,D)
    {
        # get data
        x=D$data
        # get meta data
        smeta=D$sample_meta
        # apply transform using provided
        out = pmp::glog_transformation(t(x),classes = smeta[,M$factor_name],lambda = M$lambda_opt,qc_label=M$qc_label)
        # put tranformed data into dataset object
        D$data = as.data.frame(t(out))
        # assign transformed data to output slot
        output_value(M,'transformed') = D

        return(M)
    }
)


#' glog transform optimisation plot
#'
#' plots the SSE error vs lambda for glog tranform
#' @param ... slots and values for the new object
#' @return struct object
#' @export
#' @examples
#' M = glog_opt_plot()
glog_opt_plot = function(plot_grid=100,...) {
    out=.glog_opt_plot()
    out=struct::.initialize_struct_class(out,
        plot_grid=plot_grid,
        ...)
    return(out)
}

.glog_opt_plot<-setClass(
    "glog_opt_plot",
    contains='chart',
    slots=c(
        params_plot_grid='numeric'
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("glog_opt_plot",'glog_transform'),
    definition=function(obj,dobj,gobj)
    {
        smeta=gobj$sample_meta
        x=gobj$data

        out = pmp::glog_plot_optimised_lambda(
            df=t(x),
            classes=smeta[[dobj$factor_name]],
            qc_label=dobj$qc_label,
            optimised_lambda=dobj$lambda_opt,
            plot_grid=obj$plot_grid)

        return(out)
    }
)






