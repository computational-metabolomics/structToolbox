# entity objects used in struct, to reduce duplicity
ents=list()

ents$alpha=entity_stato(name='Confidence level',
    stato_id='STATO:0000053',
    value=0.05,
    type='numeric',
    description='The p-value cutoff for determining significance.'
)

ents$mtc=enum_stato(name='Multiple test correction method',
    value='fdr',
    type='character',
    description=c(
        'bonferroni' = 'Bonferroni correction in which the p-values are multiplied by the number of comparisons.',
        'fdr' = 'Benjamini and Hochberg False Discovery Rate correction.',
        'none' = 'No correction.'
    ),
    allowed=c("bonferroni","fdr", "none"),
    max_length = 1,
    stato_id='OBI:0200089'
)

ents$formula=entity(name='Formula',
    value=y~x,
    type='formula',
    description='A symbolic description of the model to be fitted.'
)

ents$f_statistic=entity_stato(name='F-statistic',
    stato_id='STATO:0000176',
    type='data.frame',
    description=paste0('The value of the calculated statistic.'),
    value=data.frame()
)

ents$p_value=entity_stato(name='p value',
    stato_id='STATO:0000175',
    type='data.frame',
    description=paste0('The probability of observing the calculated statistic ',
        'if the null hypothesis is true.'),
    value=data.frame()
)

ents$significant=entity(name='Significant features',
    #stato_id='STATO:0000069',
    type='data.frame',
    description=paste0('True/False indicating whether the p-value computed ',
    'for each variable is less than the threshold.'),
    value=data.frame()
)

ents$blank_label=entity(name = 'Blank label',
    description = 'The label used to identify blank samples.',
    value = 'Blank',
    type='character')

ents$qc_label=entity(name = 'QC label',
    description = paste0('The label used to identify QC samples. If set to NULL ',
        'then the median of the samples is used.'),
    value = 'QC',
    type=c('character','NULL'))

ents$filtered=entity(name = 'Filtered DatasetExperiment',
    description = 'A DatasetExperiment object containing the filtered data.',
    type='DatasetExperiment',
    value=DatasetExperiment()
)

ents$flags=entity(name = 'Flags',
    description = 'A flag indicating whether the feature was rejected  or not.',
    type='data.frame',
    value=data.frame()
)

ents$factor_name=entity(name='Factor name',
    description='The name of a sample-meta column to use.',
    type='character',
    value='V1')

ents$factor_names=entity(name='Factor name(s)',
    description='The name of sample meta column(s) to use.',
    type='character',
    value='V1')

ents$by_sample=entity(name='Plot by sample or by feature',
    value=TRUE,
    type='logical',
    description=c(
        'TRUE' =  'Missing values are plotted per sample.',
        'FALSE' = 'Missing values are plotted per feature.'
    )
)
ents$label_outliers=entity(name='Label outliers',
    value=FALSE,
    type='logical',
    description=c(
        'TRUE' = 'Sample labels for potential outliers are displayed on the plot', 
        'FALSE' = 'Sample labels are not included on the plot.'
    )
)
ents$show_counts=entity(name='Show counts',
    value=TRUE,
    type='logical',
    description=c(
        'TRUE' = 'The number of samples for each box is displayed.',
        'FALSE' = 'The number of samples for each box is not displayed.'
    )
)

