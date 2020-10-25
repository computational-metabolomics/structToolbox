#' @eval get_description('pairs_filter')
#' @examples
#' M=pairs_filter(factor_name='Class',sample_id='ids')
#' @return struct object
#' @export pairs_filter
pairs_filter = function(factor_name,sample_id,...) {
    out=struct::new_struct('pairs_filter',
        factor_name=factor_name,
        sample_id=sample_id,
        ...)
    return(out)
}


.pairs_filter<-setClass(
    "pairs_filter",
    contains = c('model'),
    slots=c(factor_name='entity',
        sample_id='entity',
        filtered='entity',
        flags='entity'
    ),
    prototype=list(name = 'Pairs filter',
        description = paste0('This filter is used for study designs with ',
        'paired sampling to ensure that  measurements from the same source ',
        '(e.g. patient) are represented in all factor levels and interactions. '),
        type = 'filter',
        predicted = 'filtered',
        .params=c('factor_name','sample_id'),
        .outputs=c('filtered','flags'),

        factor_name=ents$factor_name,

        sample_id=entity(name='Sample id',
            description='Name of sample meta column containing sample identifiers',
            type='character',
            value='V1'),

        filtered=entity(name='Filtered DatasetExperiment',
            description='A DatasetExperiment object after the filter has been applied',
            type='DatasetExperiment',
            value=DatasetExperiment()),

        flags=entity(name='Filter flags',
            description='A data.frame indicating whether features were filtered from the DatasetExperiment.',
            type='data.frame',
            value=data.frame())
    )
)

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
