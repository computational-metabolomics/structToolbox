#' @eval get_description('fold_change_int')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' D=D[,1:10,drop=FALSE]
#' M = filter_smeta(mode='exclude',levels='QC',factor_name='class') +
#'     fold_change_int(factor_name=c('class','batch'))
#' M = model_apply(M,D)
#' @export fold_change_int
fold_change_int = function(
    alpha=0.05,
    factor_name,
    threshold=2,
    control_group=character(0),
    method = "geometric",
    ...) {
    out=struct::new_struct('fold_change_int',
        alpha=alpha,
        threshold=threshold,
        factor_name=factor_name,
        control_group=control_group,
        method=method,
        ...)
    return(out)
}


.fold_change_int<-setClass(
    "fold_change_int",
    contains=c('model','fold_change'),
    prototype = list(
        predicted='fold_change',
        .params=c('factor_name','sample_name','alpha','threshold','control_group','method'),
        .outputs=c('fold_change','lower_ci','upper_ci'),
        name = 'Fold change for interactions between factors',
        description = paste0('For more than one factor the fold change ',
        'calculation is extended to include all combinations of levels ',
            '(interactions) of all factors. Paired fold changes are not possible ',
            'for this computation.'),
        control_group=entity(
            name='Control group',
            description = paste0('The level names of the groups used in ',
                'the denominator (where possible) when computing fold change. ',
                'One level for each factor, assumed to be in the same order as factor_name.'
            ),
            type='character',
            max_length = Inf,
            value=character(0)
        )
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
        FF=structToolbox:::full_fact(M$factor_name)
        FF=apply(FF,1,function(x) M$factor_name[x==1])
        FF=FF[-1]

        # remove single factor comparisons
        L=lapply(FF,length)
        w=which(L==1 | L>2)
        FF=FF[-w]
        
        # try to prioritise control groups for the denominator where possible
        if (length(M$control_group)>1) {
            if (length(M$control_group)>0) {
                # do the first factor later
                for (k in 2:length(M$control_group)) {
                    L=levels(factor(D$sample_meta[[M$factor_name[k]]]))
                    w=which(L==M$control_group[k])
                    L=c(L[w],L[-w]) # [put at front it get flipped to end in fold_change]
                    D$sample_meta[[M$factor_name[k]]]=factor(D$sample_meta[[M$factor_name[k]]],levels=L)
                }   
            }
        }

        # for each combination create an interaction factor and compute fold-change on that factor
        #for (k in 1:length(FF))  {
        k=length(FF) # interactions for all factors
        D$sample_meta$interaction=interaction(D$sample_meta[,FF[[k]]])
        
        # try to prioritise control groups for the denominator where possible
        if (length(M$control_group)>0) {
            LI=levels(D$sample_meta$interaction)
            # split at .
            S = strsplit(LI,'.',fixed=TRUE)
            S = unlist(lapply(S, `[[`, 1))
            # find control group
            w=which(S==M$control_group[1])
            # reorder levels
            LI=c(LI[w],LI[-w]) # put at front order is reversed in fold_change
            D$sample_meta$interaction=factor(D$sample_meta$interaction,levels=LI)
        }
        
        FC=fold_change(
            alpha=M$alpha,
            paired=FALSE,
            sample_name='NA',
            factor_name='interaction',
            method=M$method,
            threshold=M$threshold)
        
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
