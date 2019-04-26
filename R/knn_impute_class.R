#' knn missing value imputation
#'
#' applies a k-nearest neighbour approach to impute missing values
#' @export knn_impute
#' @import pmp
knn_impute<-setClass(
  "knn_impute",
  contains = c('method'),
  slots=c(params.neighbours='entity',
    params.row_max='entity',
    params.col_max='entity',
    outputs.imputed='entity'
  ),

  prototype=list(name = 'kNN missing value imputation',
    description = 'k-nearest neighbour missing value imputation.',
    type = 'normalisation',
    predicted = 'imputed',

    params.neighbours=entity(name = 'Number of neighbours',
      description = 'The number of neighbours (k) to use ofr imputation of missing values.',
      value = 5,
      type='numeric'),

    params.neighbours=entity(name = 'Maximum percent per sample',
      description = 'The maximum percent missing values per sample',
      value = 50,
      type='numeric'),

    outputs.imputed=entity(name = 'Imputed dataset',
      description = 'A dataset object containing the data where missing values have been imputed.',
      type='dataset',
      value=dataset()
    )
  )
)

#' @export
setMethod(f="method.apply",
  signature=c("knn_impute","dataset"),
  definition=function(M,D)
  {
    opt=param.list(M)

    smeta=dataset.sample_meta(D)
    x=dataset.data(D)

    imputed = mv_imputation(t(as.matrix(x)),method='knn',k = opt$neighbours,rowmax=opt$row_max/100,colmax=opt$col_max/100,maxp = NULL,FALSE)
    dataset.data(D) = as.data.frame(t(imputed))

    output.value(M,'imputed') = D

    return(M)
  }
)
