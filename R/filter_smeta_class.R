#' @eval get_description('filter_smeta')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = filter_smeta(mode='exclude',levels='QC',factor_name='QC')
#' M = model_apply(M,D)
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
        name='Filter by sample meta data',
        description=paste0('The data is filtered by so that the named levels ',
        'of a factor are included/excluded from the dataset. '),
        predicted = 'filtered',
        .params=c('mode','levels','factor_name'),
        .outputs=c('filtered'),

        mode=enum(name='Mode of action',
            description=c(
                "include" = 'Samples in the specified levels are retained.' ,
                "exclude" = 'Samples in the specified levels are excluded.'
                ),
            type='character',
            allowed=c('include','exclude'),
            value='include',
            max_length=1
        ),

        levels=entity(name='Levels to filter by',
            description='The level name(s) for filtering.',
            type='character',
            value=character(0)
        ),

        factor_name=ents$factor_name
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
