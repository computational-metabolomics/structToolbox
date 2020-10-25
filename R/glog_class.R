#' @eval get_description('glog_transform')
#' @export glog_transform
#' @examples
#' D = iris_DatasetExperiment()
#' M = glog_transform(qc_label='versicolor',factor_name='Species')
#' M = model_apply(M,D)
glog_transform = function(qc_label='QC',factor_name,lambda=NULL,...) {
    out=struct::new_struct('glog_transform',
        qc_label=qc_label,
        factor_name=factor_name,
        lambda=lambda,
        ...)
    return(out)
}


.glog_transform<-setClass(
    "glog_transform",
    contains = c('model'),
    slots=c(qc_label='entity',
        factor_name='entity',
        transformed='entity',
        lambda='entity',
        error_flag='entity',
        lambda_opt='entity'
    ),
    
    prototype=list(name = 'Generalised logarithmic transform',
        description = paste0('The generalised logarithm (glog) transformation ',
            'applies a log transformation while applying an offset to account for ',
            'technical variation.'),
        type = 'transformation',
        predicted = 'transformed',
        libraries = 'pmp',
        citations=list(
            bibentry(
                bibtype='Article',
                year = 2002,
                volume = 18,
                number = "Suppl 1",
                pages = "S105-S110",
                author = as.person("B.P. Durbin and J.S. Hardin and D.M. Hawkins and D.M. Rocke"),
                title = "A variance-stabilizing transformation for gene-expression microarray data",
                journal = "Bioinformatics"
            ),
            bibentry(
                bibtype='Article',
                year = 2007,
                volume = 8,
                number = 1,
                pages = 234,
                author = as.person("Helen M Parsons, Christian Ludwig, Ulrich L Gunther and Mark R Viant"),
                title = paste0("Improved classification accuracy in 1- and ',
                    '2-dimensional {NMR} metabolomics data using the variance ',
                    'stabilising generalised logarithm transformation"),
                journal = "Bioinformatics"
            )
        ),
        .params=c('qc_label','factor_name','lambda'),
        .outputs=c('transformed','error_flag','lambda_opt'),
        
        factor_name=ents$factor_name,
        
        qc_label=entity(name = 'QC label',
            description = 'The label used to identify QC samples.',
            value = 'QC',
            type='character'),
        
        transformed=entity(name = 'glog transformed DatasetExperiment',
            description = 'A DatasetExperiment object containing the glog transformed data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        lambda=entity(name = 'lambda',
            description = paste0('The value of lambda to use. If NULL then ',
                'the pmp package will be used to determine an "optimal" value for lambda.'),
            type=c('numeric','NULL'),
            value=NULL
        ),
        lambda_opt=entity(name = 'lambda_opt',
            description = 'The optimal value for lambda returned by the pmp package, if used.',
            type=c('numeric','NULL'),
            value=NULL
        ),
        error_flag=entity(name = 'Optimisation error',
            description = 'A logical indicating whether the glog optimisation for lambda was successful. If not then PMP returns a default value for lambda.',
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
        
        out = pmp::glog_transformation(t(x),classes = smeta[,M$factor_name],qc_label=opt$qc_label,lambda = M$lambda)
        
        M$lambda = attributes(out)$processing_history$glog_transformation$lambda
        M$lambda_opt=attributes(out)$processing_history$glog_transformation$lambda_opt
        M$error_flag = attributes(out)$processing_history$glog_transformation$error_flag
        
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
        # use lambda if provided
        lambda=M$lambda
        if (is.null(M$lambda)) {
            # use optimised value if requested
            lambda = M$lambda_opt
        }
        
        # apply transform using provided
        out = pmp::glog_transformation(t(x),classes = smeta[,M$factor_name],lambda = lambda,qc_label=M$qc_label)
        # put transformed data into dataset object
        D$data = as.data.frame(t(out))
        # assign transformed data to output slot
        output_value(M,'transformed') = D
        
        return(M)
    }
)


#' @eval get_description('glog_opt_plot')
#' @export
#' @examples
#' D = iris_DatasetExperiment()
#' M = glog_transform(qc_label='versicolor',factor_name='Species')
#' M = model_apply(M,D)
#' C = glog_opt_plot()
#' chart_plot(C,M,D)
glog_opt_plot = function(plot_grid=100,...) {
    out=struct::new_struct('glog_opt_plot',
        plot_grid=plot_grid,
        ...)
    return(out)
}

.glog_opt_plot<-setClass(
    "glog_opt_plot",
    contains='chart',
    slots=c(plot_grid='numeric'),
    prototype=list(
        name='Glog optimisation',
        description=paste0('A plot of the sum of squares error (SSE) vs ',
        'different values of lambda for the glog transform. The indicated ',
        'optimum value for lambda minimises the SSE.'),
        .params=c('plot_grid'),
        libraries='pmp'
    )
)

#' @export
#' @template chart_plot
#' @param gobj The DatasetExperiment object used with glog_transform
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






