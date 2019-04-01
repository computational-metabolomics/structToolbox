#' filter by sample meta class
#'
#' a Filter to reduce a dataset object based on sample meta data labels.
#' @export filter_smeta

filter_smeta<-setClass(
  "filter_smeta",
  contains = c('method'),
  slots=c(params.mode='entity',
          params.levels='entity',
          params.factor_name='entity',
          outputs.filtered='dataset'
  ),
  prototype=list(type = 'filter',
                 predicted = 'filtered'
  )
)

#' @export
setMethod(f="method.apply",
          signature=c("filter_smeta","dataset"),
          definition=function(M,D)
          {
            opt=param.list(M)
            smeta=dataset.sample_meta(D)
            x=dataset.data(D)
            if (opt$mode=='exclude') {
              out=smeta[[opt$factor_name]] %in% opt$levels
            } else if (opt$mode=='include') {
              out=!(smeta[[opt$factor_name]] %in% opt$levels)
            } else {
              stop('mode must be "include" or "exclude"')
            }
            x=x[!out,,drop=FALSE]
            smeta=smeta[!out,,drop=FALSE]
            smeta=droplevels(smeta)
            dataset.data(D)=x
            dataset.sample_meta(D)=smeta
            output.value(M,'filtered')=D
            return(M)
          }
)


