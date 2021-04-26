#' MTBLS79: Direct infusion mass spectrometry metabolomics dataset: a benchmark for data processing and quality control
#'
#' Direct-infusion mass spectrometry (DIMS) metabolomics is an important approach 
#' for characterising molecular responses of organisms to disease, drugs and the 
#' environment. Increasingly large-scale metabolomics studies are being conducted, 
#' necessitating improvements in both bioanalytical and computational workflows 
#' to maintain data quality. This dataset represents a systematic evaluation of 
#' the reproducibility of a multi-batch DIMS metabolomics study of cardiac tissue 
#' extracts. It comprises of twenty biological samples (cow vs. sheep) that were 
#' analysed repeatedly, in 8 batches across 7 days, together with a concurrent set 
#' of quality control (QC) samples. Data are presented from each step of the workflow 
#' and are available in MetaboLights (https://www.ebi.ac.uk/metabolights/MTBLS79)
#' 
#' @param filtered TRUE to load data with quality control filters already applied, 
#' or FALSE to load the unfiltered data. Default is FALSE. The raw data is available 
#' from (https://www.ebi.ac.uk/metabolights/MTBLS79) and as an R dataset in the 
#' \code{pmp} package, available on Bioconductor.
#' 
#' @export MTBLS79_DatasetExperiment
#' @return DatasetExperiment object
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' summary(D)
MTBLS79_DatasetExperiment=function(filtered=FALSE) {

    if (filtered) {
        M = filter_by_name(mode='exclude',dimension='variable',names=to_filter)
        M = model_apply(M,MTBLS79_corrected)
        return(predicted(M))
    } else {
        return(MTBLS79_corrected)
    }
}

# internal function to generate corrected data from pmp
prep_from_pmp = function() {
    # library(pmp)
    # the pmp SE object
    SE = pmp::MTBLS79
    
    # convert to DE
    DE = as.DatasetExperiment(SE)
    DE$name = 'MTBLS79'
    DE$description = 'Converted from SE provided by the pmp package'
    
    # add a column indicating the order the samples were measured in
    DE$sample_meta$run_order = 1:nrow(DE)
    
    # add a column indicating if the sample is biological or a QC
    Type=as.character(DE$sample_meta$Class)
    Type[Type != 'QC'] = 'Sample'
    DE$sample_meta$Type = factor(Type)
    
    # add a column for plotting batches
    DE$sample_meta$batch_qc = DE$sample_meta$Batch
    DE$sample_meta$batch_qc[DE$sample_meta$Type=='QC']='QC'
    
    # convert to factors
    DE$sample_meta$Batch = factor(DE$sample_meta$Batch)
    DE$sample_meta$Type = factor(DE$sample_meta$Type)
    DE$sample_meta$Class = factor(DE$sample_meta$Class)
    DE$sample_meta$batch_qc = factor(DE$sample_meta$batch_qc)
    
    M = # batch correction
        sb_corr(
            order_col='run_order',
            batch_col='Batch', 
            qc_col='Type', 
            qc_label='QC',
            spar_lim = c(0.6,0.8) 
        ) +
        filter_na_count(
            threshold=3,
            factor_name='Batch'
        )
    
    M = model_apply(M,DE)
    
    # get the signal/batch corrected data
    MTBLS79_corrected = predicted(M)
    
    # extra filtering
    M3 = kw_rank_sum(
        alpha=0.0001,
        mtc='none',
        factor_names='Batch',
        predicted='significant'
        ) +
        filter_by_name(
            mode='exclude',
            dimension = 'variable',
            seq_in = 'names', 
            names='seq_fcn', # this is a placeholder and will be replaced by seq_fcn
            seq_fcn=function(x){return(x[,1])}
        ) +
        wilcox_test(
            alpha=1e-14,
            factor_names='Type', 
            mtc='none', 
            predicted = 'significant'
        ) +
        filter_by_name(
            mode='exclude',
            dimension='variable',
            seq_in='names', 
            names='place_holder'
        )+
        rsd_filter(
            rsd_threshold=20,
            factor_name='Type'
        )
    
    M3 = model_apply(M3, MTBLS79_corrected)
    
    # store the colnames of the filtered data
    to_filter=colnames(MTBLS79_corrected) %in% colnames(predicted(M3))
    to_filter=colnames(MTBLS79_corrected)[!to_filter] # names of features to remove
    
    # write the data
    #usethis::use_data(MTBLS79_corrected,to_filter,internal=TRUE,overwrite=TRUE)
}