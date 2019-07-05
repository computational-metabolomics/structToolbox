#' log transform
#'
#' applies a log transform to the input data
#' @export log_transform
#' @examples
#' M = log_transform()
log_transform<-setClass(
    "log_transform",
    contains = c('method'),
    slots=c(params.base='entity',
        outputs.transformed='entity'
    ),

    prototype=list(name = 'logarithm transform',
        description = 'applies a log tranform to the data.',
        type = 'transform',
        predicted = 'transformed',

        params.base=entity(name = 'logarithm base',
            description = 'The base of the logarithm used for the tranform.',
            value = 10,
            type='numeric'),

        outputs.transformed=entity(name = 'log transformed dataset',
            description = 'A dataset object containing the log transformed data.',
            type='dataset',
            value=dataset()
        )
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("log_transform","dataset"),
    definition=function(M,D)
    {
        opt=param.list(M)

        smeta=dataset.sample_meta(D)
        x=dataset.data(D)

        out = log(x,base = opt$base)
        dataset.data(D) = as.data.frame(out)

        output.value(M,'transformed') = D

        return(M)
    }
)
