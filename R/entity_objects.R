# entity objects used in struct, to reduce duplicity
ents=list()

ents$alpha=entity.stato(name='Confidence level',
    stato.id='STATO:0000053',
    value=0.05,
    type='numeric',
    description='the p-value cutoff for determining significance.'
)

ents$mtc=entity.stato(name='Multiple Test Correction method',
    stato.id='OBI:0200089',
    value='fdr',
    type='character',
    description='The method used to adjust for multiple comparisons.'
)

ents$formula=entity(name='Formula',
    value=y~x,
    type='formula',
    description='The formula to use'
)

ents.f_statistic=entity.stato(name='F-statistic',
    stato.id='STATO:0000176',
    type='data.frame',
    description='the value of the calculated statistic which is converted to a p-value when compared to an F-distribution.'
)

ents$p_value=entity.stato(name='p value',
    stato.id='STATO:0000175',
    type='data.frame',
    description='The probability of observing the calculated statistic.'
)

ents$significant=entity(name='Significant features',
    #stato.id='STATO:0000069',
    type='data.frame',
    description='TRUE if a feature is considered "significant", FALSE if not.',
    value=data.frame()
)

ents$blank_label=entity(name = 'Blank label',
    description = 'Label used to identify blank samples.',
    value = 'Blank',
    type='character')

ents$qc_label=entity(name = 'QC label',
    description = 'Label used to identify QC samples. If not set to null then median of the QCs is used instead of all samples.',
    value = 'QC',
    type=c('character','NULL'))

ents$filtered=entity(name = 'Filtered dataset',
    description = 'A dataset object containing the filtered data.',
    type='dataset',
    value=dataset()
)

ents$flags=entity(name = 'Flags',
    description = 'Flag indicating whether the feature was rejected by the filter or not.',
    type='data.frame',
    value=data.frame()
)

ents$factor_name=entity(name='Factor name',
    description='Name of sample meta column to use',
    type='character',
    value='V1')

ents$factor_names=entity(name='Factor name(s)',
    description='Name of sample meta column(s) to use',
    type='character',
    value='V1')




