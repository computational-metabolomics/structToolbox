#' fold change for interactions class
#'
#' calculates fold change between groups for all factors and interactions
#'
#' @import struct
#' @import stats
#' @export fold_change_int
#' @examples
#' M = fold_change_int()
fold_change_int<-setClass(
    "fold_change_int",
    contains=c('method','fold_change'),
)

#' @export
setMethod(f="method.apply",
    signature=c("fold_change_int",'dataset'),
    definition=function(M,D)
    {

        ## apply fold change between all pairwise combinations of levels of all factors
        # combinations of factors
        FF=structToolbox:::full_fact(M$factor_name)
        FF=apply(FF,1,function(x) M$factor_name[x==1])
        FF=FF[-1]

        # remove single factor comparisons
        L=lapply(FF,length)
        w=which(L==1)
        FF=FF[-w]

        # for each combination create an interaction factor and compute fold-change on that factor
        #for (k in 1:length(FF))  {
        k=length(FF) # interactions for all factors
        D$sample_meta$interaction=interaction(D$sample_meta[,FF[[k]]])
        FC=fold_change(alpha=M$alpha,paired=FALSE,sample_name='NA',factor_name='interaction')
        FC=method.apply(FC,D)
        #if (k==1) {
            M$fold_change=FC$fold_change
            M$upper_ci=FC$upper_ci
            M$lower_ci=FC$lower_ci
        #} else {
            # bind the results tables together
        #    M$fold_change=cbind(M$fold_change,FC$fold_change)
        #    M$upper_ci=cbind(M$upper_ci,FC$upper_ci)
        #    M$lower_ci=cbind(M$lower_ci,FC$lower_ci)
        #}
        #}

        return(M)
    }
)
