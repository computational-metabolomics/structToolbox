#' Pairs filter
#'
#' Filters samples for paired analysis, ensuring each sample id is present in all groups.
#'
#' @param factor_name (character) the column name of sample_meta containing the
#' labels
#' @param sample_id (character) the column name of sample_meta containing the
#' sample ids
#'
#' @return A STRUCT method object with functions for applying a pairs filter
#'
#' @examples
#' D = iris_dataset()
#' M = pairs_filter(factor_name='Species',
#'                  sample_id='setosa')
#' M = method.apply(M,D)
#'
#' @export pairs_filter
pairs_filter<-setClass(
    "pairs_filter",
    contains = c('method'),
    slots=c(params.factor_name='entity',
        params.fraction='entity',
        params.sample_id='entity',
        outputs.filtered='entity',
        outputs.flags='entity'
    ),
    prototype=list(name = 'Blank filter',
        description = 'Filters features by comparing the median intensity of blank samples to the median intensity of samples. Features where the intensity is not large compared to the blank are removed.',
        type = 'filter',
        predicted = 'filtered',

        params.factor_name=entity(name='Factor name',
            description='Name of sample meta column to use',
            type='character',
            value='V1'),

        params.sample_id=entity(name='Sample id',
            description='Name of sample meta column containing the sample id',
            type='character',
            value='V1')
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("pairs_filter","dataset"),
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
            FF=method.apply(FF,D)
            M$filtered=predicted(FF)


        }
        return(M)
    }
)
