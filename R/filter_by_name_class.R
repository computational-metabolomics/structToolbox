#' Filter by name
#'
#' A filter to subsample a DatasetExperiment object based on sample or feature name,
#' id, row/column index or using a vector of TRUE/FALSE.
#'
#' @param mode "include" or ["exclude"] to subsample a DatasetExperiment by including or
#' excluding samples/features based on the provided labels
#' @param dimension ["sample"] or "variable" to filter by sample or feature
#' labels
#' @param names the sample/feature identifiers to filter by. Can provide column
#' names, column indices or logical.
#'
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = filter_by_name(mode='exclude',dimension='variable',names=c(1,2,3))
#' M = model_apply(M,D)
#'
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export filter_by_name
filter_by_name = function(mode='exclude',dimension='sample',names,...) {
    out=struct::new_struct('filter_by_name',
        mode=mode,
        dimension=dimension,
        names=names,
        ...)
    return(out)
}


.filter_by_name<-setClass(
    "filter_by_name",
    contains = c('model'),
    slots=c(mode='entity',
        dimension='enum',
        names='entity',
        filtered='DatasetExperiment'
    ),
    prototype=list(type = 'filter',
        predicted = 'filtered',
        .params=c('mode','dimension','names'),
        .outputs=c('filtered'),

        # inputs
        mode=entity(value='exclude',
            name='Filter mode',
            description = 'The filtering mode controls whether samples/features are mode="included" or mode="excluded" based on their name',
            type='character'),
        dimension=enum(value='sample',
            name='Filter dimension',
            description = 'The filtering dimensions controls whether dimension="sample" or dimension="variable" are filtered based on their name',
            type='character',
            allowed=c('sample','variable')
        ),
        names=entity(name='Names',
            description = 'The name of features/samples to be filtered. Must be an exact match. Can also provide indexes (numeric) or logical.',
            type=c('character','numeric','logical'))
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("filter_by_name","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)
        x=D$data

        if (opt$dimension=='sample') {
            smeta=D$sample_meta

            if (is.logical(opt$names)) {

                IN = opt$names

            } else if (is.numeric(opt$names)) {
                IN = (1:nrow(D$data)) %in% opt$names
            } else {
                IN=rownames(smeta) %in% opt$names
            }

            if (opt$mode=='include') {
                smeta=smeta[IN,,drop=FALSE]
                D=D[IN,,drop=FALSE]
            } else if (opt$mode=='exclude') {
                smeta=smeta[!IN,,drop=FALSE]
                D=D[!IN,,drop=FALSE]
            }

        } else if (opt$dimension=='variable') {
            vmeta=D$variable_meta

            if (is.logical(opt$names)) {
                # TRUE or FALSE provided so use as is
                IN = opt$names
            } else if (is.numeric(opt$names)) {
                # numeric provided to assume column indices
                IN = (1:ncol(D$data)) %in% opt$names
            } else {
                # character so assume column names
                IN=rownames(vmeta) %in% opt$names
            }


            if (opt$mode=='include') {
                vmeta=vmeta[IN,,drop=FALSE]
                D=D[ ,IN,drop=FALSE]
            } else if (opt$mode=='exclude') {
                vmeta=vmeta[!IN,,drop=FALSE]
                D=D[,!IN,drop=FALSE]
            }

        }
        output_value(M,'filtered')=D
        return(M)
    }
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("filter_by_name","DatasetExperiment"),
    definition=function(M,D) {
        M=model_apply(M,D)
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("filter_by_name","DatasetExperiment"),
    definition=function(M,D) {
        M=model_apply(M,D)
        return(M)
    }
)

