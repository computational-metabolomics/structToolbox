#' fold change for interactions class
#'
#' Calculates fold change between groups for interactions between levels of
#' factors. Note that paired forced to FALSE for all comparisons.
#'
#' @examples
#' D = sbcms_dataset()
#' D$data=D$data[,1:10,drop=FALSE]
#' M = filter_smeta(mode='exclude',levels='QC',factor_name='class') +
#'     fold_change_int(factor_name=c('class','batch'))
#' M = model.apply(M,D)
#'
#' @param alpha confidence level to use for intervals
#' @param factor_name the sample_meta column to use
#' @param threshold a threshold to define fold change as 'significant'.
#' @param control_group a level of factor name to use as the control group for
#' calculations.
#'
#' @import struct
#' @import stats
#' @export fold_change_int
fold_change_int<-setClass(
    "fold_change_int",
    contains=c('model','fold_change'),
    prototype = list(predicted='fold_change')
)

#' @export
#' @template model_apply
setMethod(f="model.apply",
    signature=c("fold_change_int",'dataset'),
    definition=function(M,D)
    {

        ## apply fold change between all pairwise combinations of levels of all factors
        # combinations of factors
        FF=full_fact(M$factor_name)
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
        FC=model.apply(FC,D)
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
