#' filter by name
#'
#' a filter to subsample a dataset object based on sample or feature labels.
#'
#' @param mode "include" or ["exclude"] to subsample a a dataset by including or
#' excluding samples/features based on the provided labels
#' @param dimension ["sample"] or "variable" to filter by sample or feature
#' labels
#' @param names the sample/feature identifiers to filter by. Can provide column
#' names, column indices or logical
#'
#' @examples
#' D = sbcms_dataset()
#' M = filter_by_name(mode='exclude',dimension='variable',names=c(1,2,3))
#' M = method.apply(M,D)
#'
#' @export filter_by_name
filter_by_name<-setClass(
    "filter_by_name",
    contains = c('method'),
    slots=c(params.mode='entity',
        params.dimension='enum',
        params.names='entity',
        outputs.filtered='dataset'
    ),
    prototype=list(type = 'filter',
        predicted = 'filtered',
        params.mode=entity(value='exclude',
            name='Filter mode',
            description = 'The filtering mode controls whether samples/features are mode="included" or mode="excluded" based on their name',
            type='character'),
        params.dimension=enum(value='sample',
            name='Filter dimension',
            description = 'The filtering dimensions controls whether dimension="sample" or dimension="variable" are filtered based on their name',
            type='character',
            list=c('sample','variable')
        ),

        params.names=entity(name='Names',
            description = 'The name of features/samples to be filtered. Must be an exact match. Can also provide indexes (numeric) or logical.',
            type='character')
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("filter_by_name","dataset"),
    definition=function(M,D)
    {
        opt=param.list(M)
        x=dataset.data(D)

        if (opt$dimension=='sample') {
            smeta=dataset.sample_meta(D)

            if (is.logical(opt$names)) {

                    IN = opt$names

            } else if (is.numeric(opt$names)) {
                IN = (1:nrow(D$data)) %in% opt$names
            } else {
                IN=rownames(smeta) %in% opt$names
            }

            if (opt$mode=='include') {
                smeta=smeta[IN,,drop=FALSE]
                x=x[IN,,drop=FALSE]
            } else if (opt$mode=='exclude') {
                smeta=smeta[!IN,,drop=FALSE]
                x=x[!IN,,drop=FALSE]
            }
            dataset.data(D)=x
            dataset.sample_meta(D)=smeta
        } else if (opt$dimension=='variable') {
            vmeta=dataset.variable_meta(D)

            if (is.logical(opt$names)) {

                IN = opt$names
                INx = opt$names

            } else if (is.numeric(opt$names)) {
                IN = (1:ncol(D$data)) %in% opt$names
                INx = (1:ncol(D$data)) %in% opt$names
            } else {
                IN=rownames(vmeta) %in% opt$names
                INx=colnames(x) %in% opt$names
            }


            if (opt$mode=='include') {
                vmeta=vmeta[IN,,drop=FALSE]
                x=x[ ,INx,drop=FALSE]
            } else if (opt$mode=='exclude') {
                vmeta=vmeta[!IN,,drop=FALSE]
                x=x[,!INx,drop=FALSE]
            }
            dataset.data(D)=x
            dataset.variable_meta(D)=vmeta
        }
        output.value(M,'filtered')=D
        return(M)
    }
)

