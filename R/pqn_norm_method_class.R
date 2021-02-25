#' @eval get_description('pqn_norm')
#' @export pqn_norm
#' @examples
#' D = iris_DatasetExperiment()
#' M = pqn_norm(factor_name='Species',qc_label='all')
#' M = model_apply(M,D)
#'
pqn_norm = function(
    qc_label='QC',
    factor_name,
    qc_frac=0,
    sample_frac=0,
    ref_method="mean",
    ref_mean=NULL,
    ...) {
    
    out=struct::new_struct('pqn_norm',
        qc_label=qc_label,
        factor_name=factor_name,
        qc_frac=qc_frac,
        sample_frac=sample_frac,
        ref_method=ref_method,
        ref_mean=ref_mean,
        ...)
    return(out)
}


.pqn_norm<-setClass(
    "pqn_norm",
    contains = c('model'),
    slots=c(
        qc_label='entity',
        factor_name='entity',
        qc_frac='entity',
        sample_frac='entity',
        ref_method='enum',
        normalised='entity',
        coeff='entity',
        ref_mean='entity', 
        computed_ref='entity', 
        ref_count='entity', 
        sample_count='entity', 
        coef_count='entity', 
        coef_idx='entity', 
        coef_name='entity'
    ),
    prototype=list(name = 'Probabilistic Quotient Normalisation (PQN)',
        description = paste0(
            'PQN is used to normalise for differences in concentration ',
            'between samples. It makes use of Quality Control (QC) samples as ',
            'a reference. PQN scales by the median change relative to the ',
            'reference in order to be more robust against changes caused by ',
            'response to perturbation.'),
        type = 'normalisation',
        predicted = 'normalised',
        libraries='pmp',
        .params=c('qc_label','factor_name','qc_frac','sample_frac','ref_mean',
            'ref_method'),
        .outputs=c('normalised','coeff','computed_ref','ref_count','sample_count',
            'coef_count','coef_idx','coef_name'),
        
        qc_label=entity(name = 'QC label',
            description = 'The label used to identify QC samples.',
            value = 'QC',
            type='character',
            max_length = 1
        ),
        
        qc_frac=entity(name = 'QC fraction',
            description=paste0(
                "A value between 0 and 1 to indicate the minimum proportion ",
                "of QC samples a feature must be present in for it to be ",
                "included when computing the reference. Default qc_frac = 0. "
            ),
            type='numeric',
            value=0,
            max_length = 1
        ),
        
        sample_frac=entity(name = 'Sample fraction',
            description=paste0(
                "A value between 0 and 1 to indicate the minimum proportion ",
                "of samples a feature must be present in for it to be ",
                "considered when computing the normalisation coefficients. "
            ),
            type='numeric',
            value=0,
            max_length = 1
        ),
        ref_mean=entity(name = 'PQN Reference sample',
            description=paste0(
                "A single sample to use as the reference for normalisation. If ",
                "set to NULL then the reference will be computed based on ",
                "the other input parameters (ref_mean, qc_label etc). "
            ),
            type=c('numeric','NULL'),
            value=NULL
        ),
        ref_method=enum(name = 'Reference computation method',
            description=c(
                'mean' = paste0('The reference is computed as the mean of the ',
                    'samples matching the qc_label input.'),
                'median' = paste0('The reference is computed as the median ',
                    "of the samples matching the qc_label_input")
            ),
            type=c('character'),
            value='mean',
            allowed=c('mean','median'),
            max_length = 1
        ),   
        normalised=entity(name = 'Normalised DatasetExperiment',
            description = 'A DatasetExperiment object containing the normalised data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        coeff=entity(name = 'PQN coefficients',
            description = 'The normalisation coefficients calculated by PQN',
            type='data.frame',
            value=data.frame()
        ),
        
        factor_name=ents$factor_name,
        
        computed_ref = entity(name = 'Calculated PQN reference',
            description = paste0('The reference used for normalisation. If ',
                'ref_mean = NULL then this is the computed reference using the ',
                'ref_mean method. If ref_mean is provided then it is duplicated ',
                'here.'
            ),
            type=c('NULL','numeric'),
            value = NULL
        ),
        ref_count = entity(name = 'Reference feature count',
            description=paste0('The number of features present in the ',
                'reference. '
            ),
            type=c('NULL','numeric'),
            value = NULL
        ),
        ref_count = entity(name = 'Reference feature count',
            description=paste0('The number of features present in the ',
                'reference meeting th qc_frac criteria. '
            ),
            type=c('NULL','numeric'),
            value = NULL
        ),
        sample_count = entity(name = 'Sample feature count',
            description=paste0('The number of features present in the ',
                'samples meeting the sample_frac criteria. '
            ),
            type=c('NULL','numeric'),
            value = NULL
        ),
        coef_count = entity(name = 'Coefficient feature count',
            description=paste0('The number of coefficients calculated for ',
                'each sample. '
            ),
            type=c('NULL','numeric'),
            value = NULL
        ),
        coef_idx = entity(name = 'Coefficient index',
            description=paste0('The index of the feature selected as the ',
                'normalisation coefficient for each sample. '
            ),
            type=c('NULL','numeric'),
            value = NULL
        ),
        coef_name = entity(name = 'Coefficient feature name',
            description=paste0('The name of the feature selected as the ',
                'normalisation coefficient for each sample. '
            ),
            type=c('NULL','numeric','character'),
            value = NULL
        )
        
        
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("pqn_norm","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)
        
        smeta=D$sample_meta
        x=D$data
        
        normalised = pmp::pqn_normalisation(
            t(x), # operates on transpose of x
            classes=smeta[,M$factor_name],
            qc_label=opt$qc_label,
            qc_frac=opt$qc_frac,
            sample_frac=opt$qc_frac,
            ref_mean=opt$ref_mean
        ) 
        D$data = as.data.frame(t(normalised))
        
        output_value(M,'normalised') = D
        output_value(M,'coeff') = data.frame('coeff'=attributes(normalised)$flags,row.names = rownames(x))
        
        A=attributes(normalised)$processing_history$pqn_normalisation
        M$computed_ref=A$computed_ref
        M$ref_count=A$reference_feature_count
        M$sample_count=A$sample_feature_count
        M$coef_count=A$coef_feature_count
        M$coef_idx=A$coef_idx
        M$coef_name=A$coef_idx_name
        
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("pqn_norm","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)
        
        smeta=D$sample_meta
        x=D$data
        
        normalised = pmp::pqn_normalisation(
            t(x), # operates on transpose of x
            classes=smeta[,M$factor_name],
            qc_label=opt$qc_label,
            qc_frac=opt$qc_frac,
            sample_frac=opt$qc_frac,
            ref_mean=opt$computed_ref # use the computed reference from training
        ) 
        D$data = as.data.frame(t(normalised))
        
        output_value(M,'normalised') = D
        output_value(M,'coeff') = data.frame('coeff'=attributes(normalised)$flags,row.names = rownames(x))
        
        A=attributes(normalised)$processing_history$pqn_normalisation
        M$computed_ref=A$computed_ref
        M$ref_count=A$reference_feature_count
        M$sample_count=A$sample_feature_count
        M$coef_count=A$coef_feature_count
        M$coef_idx=A$coef_idx
        M$coef_name=A$coef_idx_name
        
        return(M)
    }
)


##### plots
#' @eval get_description('pqn_norm_hist')
#' @import struct
#' @export pqn_norm_hist
#' @examples
#' C = pqn_norm_hist()
pqn_norm_hist = function(...) {
    out=struct::new_struct('pqn_norm_hist',...)
    return(out)
}

.pqn_norm_hist<-setClass(
    "pqn_norm_hist",
    contains='chart',
    prototype = list(name='PQN coefficient histogram',
        description='A histogram of the PQN coefficients for all features',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pqn_norm_hist",'pqn_norm'),
    definition=function(obj,dobj)
    {
        A=output_value(dobj,'coeff')
        
        out=ggplot(data=A, aes_(x=~pqn_coef)) +
            geom_histogram(color='white') +
            xlab('PQN coefficient') +
            ylab('Count') +
            scale_fill_Publication()+
            theme_Publication(base_size = 12)
        
        return(out)
    }
)
