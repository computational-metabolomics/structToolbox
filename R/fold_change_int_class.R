#' @eval get_description('fold_change_int')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' D=D[,1:10,drop=FALSE]
#' M = filter_smeta(mode='exclude',levels='QC',factor_name='class') +
#'     fold_change_int(factor_name=c('class','batch'))
#' M = model_apply(M,D)
#' @export fold_change_int
fold_change_int = function(alpha=0.05,factor_name,threshold=2,control_group=character(0),...) {
    out=struct::new_struct('fold_change_int',
        alpha=alpha,
        threshold=threshold,
        control_group=control_group,
        factor_name=factor_name,
        ...)
    return(out)
}


.fold_change_int<-setClass(
    "fold_change_int",
    contains=c('model','fold_change'),
    prototype = list(
        predicted='fold_change',
        .params=c('factor_name','sample_name','alpha','paired','threshold'),
        .outputs=c('fold_change','lower_ci','upper_ci'),
        name = 'Fold change for interactions between factors',
        description = paste0('For more than one factor the fold change ',
        'calculation is extended to include all combinations of levels ',
            '(interactions) of all factors. Paired fold changes are not possible ',
            'for this computation.')
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("fold_change_int",'DatasetExperiment'),
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
        FC=model_apply(FC,D)
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
