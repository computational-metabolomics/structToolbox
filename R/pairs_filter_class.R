#' Pairs filter
#'
#' Filters samples for paired analysis, ensuring each sample id is present in all groups.
#'
#' @slot factor_name (character) the column name of sample_meta containing the
#' labels
#' @slot sample_id (character) the column name of sample_meta containing the
#' sample ids
#'
#' @return A STRUCT method object with functions for applying a pairs filter
#' @examples
#' M=pairs_filter()
#'
#' @param ... slots and values for the new object 
#' @return struct object
#' @export pairs_filter
pairs_filter = function(...) {
    out=.pairs_filter()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.pairs_filter<-setClass(
    "pairs_filter",
    contains = c('model'),
    slots=c(params_factor_name='entity',
        params_fraction='entity',
        params_sample_id='entity',
        outputs_filtered='entity',
        outputs_flags='entity'
    ),
    prototype=list(name = 'Blank filter',
        description = 'Filters features by comparing the median intensity of blank samples to the median intensity of samples. Features where the intensity is not large compared to the blank are removed.',
        type = 'filter',
        predicted = 'filtered',

        params_factor_name=entity(name='Factor name',
            description='Name of sample meta column to use',
            type='character',
            value='V1'),

        params_sample_id=entity(name='Sample id',
            description='Name of sample meta column containing the sample id',
            type='character',
            value='V1'),

        outputs_filtered=entity(name='Filtered DatasetExperiment',
            description='A DatasetExperiment object after the filter has been applied',
            type='DatasetExperiment',
            value=DatasetExperiment()),

        outputs_flags=entity(name='Filter flags',
            description='A data.frame indicating whether features were filtered from the DatasetExperiment.',
            type='data.frame',
            value=data.frame())
    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("pairs_filter","DatasetExperiment"),
    definition=function(M,D){
        M=model_apply(M,D)
        return(M)
    })

#' @param ... slots and values for the new object 
#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("pairs_filter","DatasetExperiment"),
    definition=function(M,D){
        M=model_apply(M,D)
        return(M)
    })

#' @param ... slots and values for the new object 
#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("pairs_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        # get levels of factor
        L=levels(D$sample_meta[[M$factor_name]])
        # make the sample id a factor
        D$sample_meta[[M$sample_id]]=factor(D$sample_meta[[M$sample_id]])

        idx=numeric(0)

        # for each sample id
        for (k in levels(D$sample_meta[[M$sample_id]])) {

            OK=TRUE
            w=which(D$sample_meta[[M$sample_id]]==k)
            l=levels(D$sample_meta[w,M$factor_name])

            # make sure we have all factors represented
            if (!(all(L %in% l))){
                OK=FALSE
            }

            # make sure all factors represented equally
            if (length(w) %% length(L) != 0) {
                OK=FALSE
            }

            # if not ok then flag for removal
            if (!OK) {
                idx=c(idx,w)
            }

            flags=matrix(0,nrow=nrow(D$data),ncol=1)
            flags[idx,1]=1
            M$flags=data.frame(flags=flags)

            FF=filter_by_name(mode='exclude',dimension='sample',names=as.logical(flags[,1]))
            FF=model_apply(FF,D)
            M$filtered=predicted(FF)


        }
        return(M)
    }
)
