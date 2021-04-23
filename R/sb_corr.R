#' @eval get_description('sb_corr')
#' @return struct object
#' @export sb_corr
#' @examples
#' M = sb_corr(order_col='run_order',batch_col='batch_no',qc_col='class')
sb_corr = function(
    order_col,
    batch_col,
    qc_col,
    smooth=0,
    use_log=TRUE,
    min_qc=4,
    qc_label='QC',
    spar_lim=c(-1.5,1.5),
    ...) {
    out=struct::new_struct('sb_corr',
        order_col=order_col,
        batch_col=batch_col,
        qc_col=qc_col,
        smooth=smooth,
        use_log=use_log,
        min_qc=min_qc,
        qc_label=qc_label,
        spar_lim=spar_lim,
        ...)
    return(out)
}

.sb_corr<-setClass(
    "sb_corr",
    contains = c('model','stato'),
    slots=c(
        order_col='entity',
        batch_col='entity',
        qc_col='entity',
        smooth='entity',
        use_log='entity',
        min_qc='entity',
        qc_label='entity',
        corrected='entity',
        spar_lim='entity',
        fitted='entity'
    ),

    prototype=list(
        name = 'Signal/batch correction for mass spectrometry data',
        description = paste0('Applies Quality Control Robust Spline (QC-RSC) ',
            'method to correct for signal drift and batch differences in mass ',
            'spectrometry data.'),
        type = 'correction',
        predicted = 'corrected',
        libraries='pmp',
        citations=list(
            bibentry(
                bibtype = 'Article',
                year = '2013',
                volume = '405',
                number = '15',
                pages = '5147-5157',
                author = as.person('J. A. Kirwan and D. I. Broadhurst and R. L. Davidson and M. R. Viant'),
                title = paste0('Characterising and correcting batch variation ',
                'in an automated direct infusion mass spectrometry (DIMS) metabolomics workflow'),
                journal = 'Analytical and Bioanalytical Chemistry'
            )
        ),
        stato_id='STATO:0000236',
        .params=c('order_col','batch_col','qc_col','smooth','use_log','min_qc',
            'qc_label','spar_lim'),
        .outputs=c('corrected','fitted'),

        order_col=entity(
            name = 'Sample run order column',
            description = 'The column name of sample_meta indicating the run order of the samples.',
            value = character(0),
            type='character'),

        batch_col=entity(
            name = 'Batch ID column',
            description = 'The column name of sample_meta indicating the batch each sample was measured in.',
            value = character(0),
            type='character'),

        qc_col=entity(
            name = 'Class ID column',
            description = 'The column name of sample_meta indicating the group each sample is a member of.',
            value = character(0),
            type='character'),

        smooth=entity(
            name = 'Spline smoothing parameter',
            description = 'The amount of smoothing applied (0 to 1). If set to 0 the smoothing parameter will be estimated using
            leave-one-out cross-validation.',
            value = 0,
            type='numeric'),

        use_log=entity(
            name = 'Log tranformation',
            description = c(
                'TRUE' = 'The data is log transformed prior to performing signal correction.',
                'FALSE' = 'Signal correction is applied to the input data'),
            value = TRUE,
            type='logical'),

        min_qc=entity(
            name = 'Minimum number of QCs',
            description = 'The minimum number of QC samples required for signal correction.',
            value = 4,
            type='numeric'),

        qc_label=entity(
            name = 'QC label',
            description = 'The label used to identify QC samples.',
            value = 'QC',
            type='character'),

        corrected=entity(name = 'Signal/batch corrected DatasetExperiment',
            description = 'The DatasetExperiment after signal/batch correction has been applied.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        spar_lim=entity(name = 'Smoothing parameter limits',
            description = paste0('A two element vector specifying the upper ',
                'and lower limits when `spar = 0`. Allows the value of `spar` ',
                'to be constrained within these limits to prevent overfitting.'),
            type='numeric',
            value=c(-1.5,1.5),
            max_length = 2
        ),
        fitted=entity(name = 'Fitted splines',
            description = paste0('The fitted splines for each feature.'),
            type='data.frame'
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("sb_corr","DatasetExperiment"),
    definition=function(M,D)
    {
        rn=rownames(D$data)
        cn=colnames(D$data)
        # uses transposed data
        X=as.data.frame(t(D$data))

        # apply correction
        corrected_data <- pmp::QCRSC(
            df=X,
            order=D$sample_meta[[M$order_col]],
            batch=D$sample_meta[[M$batch_col]],
            classes=D$sample_meta[[M$qc_col]],
            spar=M$smooth,
            minQC=M$min_qc,
            log=M$use_log,
            qc_label=M$qc_label,
            spar_lim = M$spar_lim
        )

        D$data=as.data.frame(t(corrected_data))
        rownames(D$data)=rn
        colnames(D$data)=cn
        
        # get the fits
        A=attr(corrected_data,'processing_history')$QCRSC$QC_fit
        A=as.data.frame(t(A))
        colnames(A)=colnames(D$data)
        rownames(A)=rownames(D$data)
        
        M$corrected=D
        M$fitted=A
        
        return(M)
    }
)





#' @export
#' @template chart_plot
#' @param gobj The DatasetExperiment object before signal correction was applied.
setMethod(f="chart_plot",
    signature=c("feature_profile",'sb_corr'),
    definition=function(obj,dobj,gobj) {
        
        # usual plot object
        C = do.call('feature_profile', param_list(obj))
        g = chart_plot(C,gobj)
        
        # add fitted curve for selected feature
        df=data.frame(
            x = gobj$sample_meta[[obj$run_order]],
            y = dobj$fitted[,obj$feature_to_plot],
            batch=factor(gobj$sample_meta[[dobj$batch_col]]))
        g = g +
            geom_line(data=df,mapping=aes_string(x='x',y='y',group='batch'),
                color='#9E9998',size=0.8)
        
        return(g)
    }
    
)

