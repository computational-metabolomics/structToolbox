#' filter features by number of NA per class
#'
#' filters features by the number of NA per class
#' @export filter_na_count
filter_na_count<-setClass(
  "filter_na_count",
  contains = c('method'),
  slots=c(params.threshold='entity',
    params.factor_name='entity',
    outputs.filtered='entity',
    outputs.count='entity',
    outputs.flags='entity'
  ),
  prototype=list(name = 'filters features by the number of NA per class',
    description = 'Filters by removing features where the number of features in any class exceeds the threshold',
    type = 'filter',
    predicted = 'filtered',

    params.factor_name=entity(name='Factor name',
      type='character',
      description='Name of sample_meta column to use'
    ),

    params.threshold=entity(name = 'Count threshold (%)',
      description = 'Features with less than THRESHOLD missing values in any class are excluded.',
      value = 2,
      type='numeric'),


    outputs.filtered=entity(name = 'Filtered dataset',
      description = 'A dataset object containing the filtered data.',
      type='dataset',
      value=dataset()
    ),
    outputs.count=entity(name = 'NA count per class',
      description = 'Number of NA per class',
      type='data.frame',
      value=data.frame()
    ),
    outputs.flags=entity(name = 'Flags',
      description = 'a flag indicating whether the sample was rejected.',
      type='data.frame',
      value=data.frame()
    )
  )
)

#' @export
setMethod(f="method.apply",
  signature=c("filter_na_count","dataset"),
  definition=function(M,D)
  {

    # count na per class for each feature
    L=levels(D$sample_meta[[M$factor_name]])
    na_count=matrix(0,nrow=ncol(D$data),ncol=length(L))
    for (k in 1:length(L)) {
      na_count[,k]=apply(D$data,2,function(x) sum(!is.na(x[D$sample_meta[[M$factor_name]]==L[k]])))
    }
    colnames(na_count)=L

    flags=apply(na_count,1,function(x) any(x<M$threshold))

    # delete the columns
    D$data=D$data[,!flags,drop=FALSE]
    D$variable_meta=D$variable_meta[!flags,,drop=FALSE]

    M$filtered=D
    M$flags=data.frame(flags=flags)
    M$count=as.data.frame(na_count)

    return(M)
  }
)
