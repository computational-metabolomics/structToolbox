#' t-test model class
#'
#' t-test model class. Calculate t-test for all features in a dataset
#'
#' @import struct
#' @import stats
#' @export ttest
ttest<-setClass(
  "ttest",
  contains=c('method','stato'),
  slots=c(
    # INPUTS
    params.alpha='entity.stato',
    params.mtc='entity.stato',
    params.factor_names='entity',
    # OUTPUTS
    outputs.t_statistic='entity.stato',
    outputs.p_value='entity.stato',
    outputs.dof='entity.stato',
    outputs.significant='entity'

    # CHARTS
    # none
  ),
  prototype = list(name='t-test',
    description='Applies the t-test to each feature to indicate significance, with (optional)
                                multiple-testing correction.',
    type="univariate",
    predicted='p_value',
    stato.id="STATO:0000304",

    params.factor_names=entity(name='Factor names',
      type='character',
      description='Names of sample_meta columns to use'
    ),

    params.alpha=entity.stato(name='Confidence level',
      stato.id='STATO:0000053',
      value=0.05,
      type='numeric',
      description='the p-value cutoff for determining significance.'
    ),
    params.mtc=entity.stato(name='Multiple Test Correction method',
      stato.id='OBI:0200089',
      value='fdr',
      type='numeric',
      description='The method used to adjust for multiple comparisons.'
    ),
    outputs.t_statistic=entity.stato(name='t-statistic',
      stato.id='STATO:0000176',
      type='numeric',
      description='the value of the calculate statistics which is converted to a p-value when compared to a t-distribution.'
    ),
    outputs.p_value=entity.stato(name='p value',
      stato.id='STATO:0000175',
      type='numeric',
      description='the probability of observing the calculated t-statistic.'
    ),
    outputs.dof=entity.stato(name='degrees of freedom',
      stato.id='STATO:0000069',
      type='numeric',
      description='the number of degrees of freedom used to calculate the test statistic'
    ),
    outputs.significant=entity(name='Significant features',
      #stato.id='STATO:0000069',
      type='logical',
      description='TRUE if the calculated p-value is less than the supplied threhold (alpha)'
    )
  )
)

#' @export
setMethod(f="method.apply",
  signature=c("ttest",'dataset'),
  definition=function(M,D)
  {
    X=dataset.data(D)
    y=dataset.sample_meta(D)[[M$factor_names]]
    L=levels(y)
    if (length(L)!=2) {
      stop('must have exactly two levels for this implmentation of t-statistic')
    }
    output=apply(X,2,function(x) {a=unlist(t.test(x[y==L[1]],x[y==L[2]])[c("statistic","p.value","parameter")])})
    output=as.data.frame(t(output),stringsAsFactors = FALSE)
    output$p.value=p.adjust(output$p.value,method = param.value(M,'mtc'))
    output.value(M,'t_statistic')=output$statistic.t
    output.value(M,'p_value')=output$p.value
    output.value(M,'dof')=output$parameter.df
    output.value(M,'significant')=output$p.value<param.value(M,'alpha')

    return(M)
  }
)









