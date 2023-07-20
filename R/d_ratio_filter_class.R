#' @eval get_description('dratio_filter')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = dratio_filter(threshold=20,qc_label='QC',factor_name='Class')
#' M = model_apply(M,D)
#' @export dratio_filter
dratio_filter = function(
        threshold=20, 
        qc_label='QC', 
        factor_name, 
        method='ratio',
        dispersion='sd',
        ...) {
    out=struct::new_struct('dratio_filter',
                           threshold=threshold,
                           qc_label=qc_label,
                           factor_name=factor_name,
                           method=method,
                           dispersion=dispersion,
                           ...)
    return(out)
}

.dratio_filter<-setClass(
    "dratio_filter",
    contains = c('model'),
    slots=c(threshold='entity',
            qc_label='entity',
            factor_name='entity',
            filtered='entity',
            flags='entity',
            d_ratio='data.frame',
            method='enum',
            dispersion='enum'
    ),
    prototype=list(name = 'Dispersion ratio filter',
                   description = paste0('The dispersion ratio (d-ratio) compares the ',
                                        'standard deviation (or non-parametric equivalent) of the Quality ',
                                        'Control (QC) samples relative to the standard deviation (or ',
                                        'non-parametric equivalent) of the samples for each feature. ',
                                        'If the d-ratio is greater than a predefined threshold then the ',
                                        'observed sample variance could be due to technical variance and ',
                                        'the feature is removed.'),
                   type = 'filter',
                   predicted = 'filtered',
                   .params=c('threshold','qc_label','factor_name','method','dispersion'),
                   .outputs=c('filtered','flags','d_ratio'),
                   citations=list(
                       bibentry(
                           bibtype='Article',
                           year = '2018',
                           month = 'May',
                           volume = 14,
                           number = 6,
                           author = as.person(
                               paste0('David Broadhurst, Royston Goodacre, ',
                                      'Stacey N. Reinke, Julia Kuligowski, Ian D. Wilson, ',
                                      'Matthew R. Lewis, Warwick B. Dunn')),
                           title = paste0('Guidelines and considerations for the use of ',
                                          'system suitability and quality control samples in mass ',
                                          'spectrometry assays applied in untargeted clinical ',
                                          'metabolomic studies'),
                           journal = 'Metabolomics'
                       )
                   ),
                   
                   threshold=entity(name = 'Dispersion ratio threshold',
                                    description = 'The threshold above which features are removed.',
                                    value = 20,
                                    type='numeric'),
                   
                   qc_label=entity(name = 'QC label',
                                   description = 'The label used to identify QC samples.',
                                   value = 'QC',
                                   type='character'),
                   
                   factor_name=ents$factor_name,
                   
                   filtered=entity(name = 'Filtered data',
                                   description = 'A DatasetExperiment object containing the filtered data.',
                                   type='DatasetExperiment',
                                   value=DatasetExperiment()
                   ),
                   flags=entity(name = 'Flags',
                                description = 'Flag indicating whether the feature was rejected by the filter or not.',
                                type='data.frame',
                                value=data.frame()
                   ),
                   method=enum(
                       name='dratio method',
                       description = c(
                           'ratio' = paste0('Dispersion of the QCs divided by the ',
                                            'dispersion of the samples. Corresponds to Eq 4 in ',
                                            ' Broadhurst et al (2018).'),
                           'euclidean' = paste0('Dispersion of the QCs divided by the ',
                                                'euclidean length of the total dispersion. Total dispersion ',
                                                'is estimated from the QC and Sample dispersion by assuming ',
                                                'that they are orthogonal. Corresponds to Eq 5 in ',
                                                'Broadhurst et al (2018)')),
                       allowed=c('ratio','euclidean'),
                       value='ratio',
                       type='character',
                       max_length = 1
                   ),
                   dispersion=enum(
                       name='Dispersion method',
                       description = c(
                           'sd' = paste0('Dispersion is estimated using the ',
                                         'standard deviation.'),
                           'mad' = paste0('Dispersion is estimated using the median ',
                                          'absolute deviation.')),
                       allowed=c('sd','mad'),
                       value='sd',
                       type='character',
                       max_length = 1
                   )
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
          signature=c("dratio_filter","DatasetExperiment"),
          definition=function(M,D)
          {
              # dispersion QC samples
              QC = filter_smeta(
                  mode='include',
                  levels=M$qc_label,
                  factor_name=M$factor_name)
              QC = model_apply(QC,D)
              QC = predicted(QC)$data
              
              if (M$dispersion=='mad') {
                  QC = apply(QC,2,mad,na.rm=TRUE)
              } else {
                  QC = apply(QC,2,sd,na.rm=TRUE)
              }
              
              # dispersion (not QC) samples
              S = filter_smeta(
                  mode='exclude',
                  levels=M$qc_label,
                  factor_name=M$factor_name)
              S = model_apply(S,D)
              S = predicted(S)$data
              
              if (M$dispersion=='mad') {
                  S = apply(S,2,mad,na.rm=TRUE) # constant = 1.4826 default
              } else {
                  S = apply(S,2,sd,na.rm=TRUE)
              }
              
              # dispersion ratio
              if (M$method=='ratio') {
                  # eq 4
                  d_ratio=(QC/S)*100
              } else {
                  # eq 5
                  d_ratio= (QC / sqrt((QC^2) + (S^2))) * 100
              }
              
              OUT=d_ratio>M$threshold
              
              M$d_ratio=data.frame(d_ratio=d_ratio,row.names=colnames(D$data))
              M$flags=data.frame(rejected=OUT,row.names = colnames(D$data))
              
              return(M)
          }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
          signature=c("dratio_filter","DatasetExperiment"),
          definition=function(M,D)
          {
              # get flags
              OUT=M$flags$rejected
              # remove flagged
              D=D[,!OUT]
              # store
              M$filtered=D
              
              return(M)
          }
)
