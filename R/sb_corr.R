#' sbcms
#'
#' Signal/batch correction using SMCBMS package
#' @param order_col The sample-meta column containing the order of measurement.
#' @param batch_col The sample_meta column containing the batch labels.
#' @param qc_col The sample_meta column containing QC labels.
#' @param qc_label The label used in \code{qc_col} to identify QC samples.
#' @param smooth Spline smoothing parameter. Should be in the range 0 to 1.
#' If set to 0 it will be estimated using leave-one-out cross-validation.
#' @param use_log Perform the signal correction fit on the log scaled data. Default is TRUE.
#' @param min_qc Minimum number of measured quality control (QC) samples required
#' for signal correction within feature per batch. Default 4.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export sb_corr
#' @examples
#' M = sb_corr()
sb_corr = function(order_col,batch_col,qc_col,smooth=0,use_log=TRUE,min_qc=4,qc_label,...) {
    out=struct::new_struct(sb_corr,...)
    return(out)
}

.sb_corr<-setClass(
    "sb_corr",
    contains = c('model'),
    slots=c(
        order_col='entity',
        batch_col='entity',
        qc_col='entity',
        smooth='entity',
        use_log='entity',
        min_qc='entity',
        qc_label='entity',
        corrected='entity'
    ),

    prototype=list(
        name = 'Signal/batch correction for mass spectrometry data',
        description = 'Applies Quality Control Robust Spline (QC-RSC) method to
        correct for signal drift and batch differences in mass spectrometry data.',
        type = 'correction',
        predicted = 'corrected',
        libraries='pmp',
        .params=c('order_col','batch_col','qc_col','smooth','use_log','min_qc','qc_label'),
        .outputs=c('corrected'),

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
            description = 'Should be in the range 0 to 1. If set to 0 (default) it will be estimated using
            leave-one-out cross-validation.',
            value = 0,
            type='numeric'),

        use_log=entity(
            name = 'Use log transformed data',
            description = 'TRUE or FALSE to perform the signal correction fit on the log scaled data. Default is TRUE.',
            value = TRUE,
            type='logical'),

        min_qc=entity(
            name = 'Minimum number of QCs',
            description = 'Minimum number of QC samples required for signal correction.',
            value = 4,
            type='numeric'),

        qc_label=entity(
            name = 'QC label',
            description = 'The label used to identify QC samples.',
            value = 'QC',
            type='character'),

        corrected=entity(name = 'Signal/batch corrected DatasetExperiment',
            description = 'THe DatasetExperiment after signal/batch correction has been applied.',
            type='DatasetExperiment',
            value=DatasetExperiment()
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
            qc_label=M$qc_label
        )

        D$data=as.data.frame(t(corrected_data))
        rownames(D$data)=rn
        colnames(D$data)=cn

        M$corrected=D
        return(M)
    }
)
