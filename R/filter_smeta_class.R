#' filter_smeta class
#'
#' A filter to subset a DatasetExperiment object based on sample meta data.
#'
#' @param mode = ['include'] or 'exclude' to include or exclude samples based on
#' the provided labels
#' @param levels a list of level names to include/exclude
#' @param factor_name the sample_meta column name to use
#'
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = filter_smeta(mode='exclude',levels='QC',factor_name='QC')
#' M = model_apply(M,D)
#'
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export filter_smeta
filter_smeta = function(mode='include',levels,factor_name,...) {
    out=struct::new_struct('filter_smeta',
        mode=mode,
        levels=levels,
        factor_name=factor_name,
        ...)
    return(out)
}

.filter_smeta<-setClass(
    "filter_smeta",
    contains = c('model'),
    slots=c(mode='enum',
        levels='entity',
        factor_name='entity',
        filtered='DatasetExperiment'
    ),
    prototype=list(type = 'filter',
        name='Filter by sample_meta data',
        description='Filter data to include or exlude samples based on their meta data.',
        predicted = 'filtered',
        .params=c('mode','levels','factor_name'),
        .outputs=c('filtered'),

        mode=enum(name='Mode of action',
            description='"include" or "exclude" samples based on the sample_meta data',
            type='character',
            allowed=c('include','exclude'),
            value='include'
        ),

        levels=entity(name='list of level names to filter by',
            description='The levels of factor_name to filter by',
            type='character',
            value='NA'
        ),

        factor_name=entity(name='Factor name',
            description='The sample_meta column name to filter by',
            type='character',
            value='NA')
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("filter_smeta","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)
        smeta=D$sample_meta
        x=D$data
        if (opt$mode=='exclude') {
            out=smeta[[opt$factor_name]] %in% opt$levels
        } else if (opt$mode=='include') {
            out=!(smeta[[opt$factor_name]] %in% opt$levels)
        } else {
            stop('mode must be "include" or "exclude"')
        }
        D=D[!out,]
        # drop excluded levels from factors
        D$sample_meta=droplevels(D$sample_meta)
        output_value(M,'filtered')=D
        return(M)
    }
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("filter_smeta","DatasetExperiment"),
    definition=function(M,D) {
        M=model_apply(M,D)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("filter_smeta","DatasetExperiment"),
    definition=function(M,D) {
        M=model_apply(M,D)
    }
)
