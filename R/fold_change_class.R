#' fold change class
#'
#' Calculates fold change between groups for all features in a DatasetExperiment, based on
#' a log transform and t-test.
#'
#' @examples
#' D = sbcms_DatasetExperiment()
#' M = fold_change(factor_name='class')
#' M = model_apply(M,D)
#'
#' @param alpha confidence level to use for intervals
#' @param factor_name the sample_meta column to use
#' @param paired TRUE or [FALSE] to account for paired samples
#' @param sample_name the sample_meta column name to use for a paired samples
#' @param threshold a threshold to define fold change as 'significant'.
#' @param control_group a level of factor name to use as the control group for
#' calculations.
#'
#' @import struct
#' @import stats
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export fold_change
fold_change = function(
    alpha=0.05,
    factor_name,
    paired=FALSE,
    sample_name=character(0),
    threshold=2,
    control_group=character(0),
    ...) {

    out=struct::new_struct('fold_change',
        alpha=alpha,
        factor_name=factor_name,
        paired=paired,
        sample_name=sample_name,
        threshold=threshold,
        control_group=control_group,
        ...)

    return(out)
}


.fold_change<-setClass(
    "fold_change",
    contains=c('model'),
    slots=c(
        # INPUTS
        alpha='entity_stato',
        factor_name='entity',
        paired='entity',
        sample_name='entity',
        threshold='numeric',
        control_group='character',

        # OUTPUTS
        fold_change='entity',
        upper_ci='entity',
        lower_ci='entity',
        significant='data.frame'
    ),
    prototype = list(name='fold change',
        description='Calculates the fold change between all pairs of groups for each feature.',
        type="univariate",
        predicted='fold_change',
        #  stato_id="STATO:0000304",
        .params=c('factor_name','sample_name','alpha','paired','threshold','control_group'),
        .outputs=c('fold_change','lower_ci','upper_ci','significant'),

        factor_name=entity(name='Factor names',
            type='character',
            description='Name of sample_meta column to use for grouping samples'
        ),
        sample_name=entity(name='Sample names',
            type='character',
            description='Name of sample_meta columns to use for extracting pairwise comparisons'
        ),
        alpha=entity_stato(name='Confidence level',
            stato_id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for calculating confidence intervals.'
        ),
        paired=entity(name='Apply paired fold change',
            value=FALSE,
            type='logical',
            description='TRUE/FALSE to apply paired fold change.'
        ),

        threshold=2,

        fold_change=entity(name='fold change',
            type='data.frame',
            description='fold change between groups',
            value=data.frame()
        ),

        lower_ci=entity(name='Confidence interval',
            type='data.frame',
            description='lower confidence interval for fold change',
            value=data.frame()
        ),
        upper_ci=entity(name='Fold change upper confidence interval',
            type='data.frame',
            description='upper confidence interval for fold change.',
            value=data.frame()
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("fold_change",'DatasetExperiment'),
    definition=function(M,D)
    {

        # log transform
        X=log2(D$data)
        Y=D$sample_meta

        D$data=X
        D$sample_meta=Y

        # levels for factor of interest
        L=levels(as.factor(Y[[M$factor_name]]))
        # put control group first if provided
        if (length(M$control_group)>0) {
            w=which(L==M$control_group)
            if (length(w)>1) {
                L=c(L[w],L[-w])
            }
        }

        # number of pairwise comparisons
        n=((length(L)^2)-length(L))/2

        # prep some matrices
        FC=matrix(0,nrow=ncol(X),ncol=n)
        LCI=FC
        UCI=FC

        comp=character(0)

        counter=1

        D$sample_meta[[M$factor_name]]=ordered(D$sample_meta[[M$factor_name]])

        # for all pairs of groups
        for (A in 1:(length(L)-1)) {
            for (B in (A+1):(length(L))) {
                # filter groups to A and B
                FG=filter_smeta(factor_name=M$factor_name,mode='include',levels=L[c(A,B)])
                FG=model_apply(FG,D)
                # change to ordered factor so that we make use of control group
                FG$filtered$sample_meta[[M$factor_name]]=ordered(FG$filtered$sample_meta[[M$factor_name]],levels=L[c(A,B)])
                # apply t-test
                TT=ttest(alpha=M$alpha,mtc='none',factor_names=M$factor_name,paired=M$paired,paired_factor=M$sample_name)
                TT=model_apply(TT,predicted(FG))
                # log2(fold change) is the difference in estimate.mean from ttest
                if (M$paired) {
                    fc=TT$estimates[,1]
                } else {
                    fc=TT$estimates[,1]-TT$estimates[,2] # log2 fold change
                }
                FC[,counter]=fc
                LCI[,counter]=TT$conf_int[,1]
                UCI[,counter]=TT$conf_int[,2]
                counter=counter+1
                comp=c(comp,paste0(L[A],'/',L[B]))
            }
        }

        FC=as.data.frame(FC)
        LCI=as.data.frame(LCI)
        UCI=as.data.frame(UCI)

        colnames(FC)=comp
        colnames(LCI)=comp
        colnames(UCI)=comp

        rownames(FC)=colnames(D$data)
        rownames(LCI)=colnames(D$data)
        rownames(UCI)=colnames(D$data)

        M$fold_change=2^FC
        M$lower_ci=2^LCI
        M$upper_ci=2^UCI

        M$significant=as.data.frame((UCI < (-log2(M$threshold))) | (LCI>log2(M$threshold)))
        colnames(M$significant)=comp

        return(M)
    }
)

#' fold_change plot
#'
#' Plots fold change with error bars for a limited number of features.
#'
#' @import struct
#' @param number_features The number of features to display on the plot
#' @param orientation The orientation of the plot (portrait or landscape). Portrait is default.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export fold_change_plot
#' @include PCA_class.R
#' @examples
#' C = fold_change_plot()
fold_change_plot = function(number_features=20,orientation='portrait',...) {
    out=struct::new_struct('fold_change_plot',
        number_features=number_features,
        orientation=orientation,
        ...)
    return(out)
}


.fold_change_plot<-setClass(
    "fold_change_plot",
    contains='chart',
    slots=c(
        number_features='numeric',
        orientation='character'),
    prototype = list(name='Fold change plot',
        description='plots a boxplot of a chosen feature for each group of a DatasetExperiment.',
        type="boxlot",
        number_features=20,
        orientation='portrait',
        .params=c('number_features','orientation')
    )

)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("fold_change_plot",'fold_change'),
    definition=function(obj,dobj)
    {

        # choose randomly,or use provided vector
        if (length(obj$number_features)==1) {
            S=sample.int(nrow(dobj$fold_change),size=obj$number_features)
        } else {
            S=obj$number_features
        }


        A=data.frame('fc'=dobj$fold_change[S,1],'group'=colnames(dobj$fold_change)[1],'lci'=dobj$lower_ci[S,1],'uci'=dobj$upper_ci[S,1],'feature'=rownames(dobj$fold_change)[S])
        if (ncol(dobj$fold_change)>1) {
            for (k in 2:ncol(dobj$fold_change)) {
                B=data.frame('fc'=dobj$fold_change[S,k],'group'=colnames(dobj$fold_change)[k],'lci'=dobj$lower_ci[S,k],'uci'=dobj$upper_ci[S,k],'feature'=rownames(dobj$fold_change)[S])
                A=rbind(A,B)
            }
        }
        A[,c(1,3,4)]=log2(A[,c(1,3,4)])
        A$x=1:nrow(A)

        out=ggplot(data=A,aes(x=feature,y=fc,color=group)) +
            geom_rect(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=-log2(dobj$threshold),fill='#9EC086',inherit.aes = FALSE,alpha=0.02) +
            geom_rect(xmin=-Inf,xmax=Inf,ymax=Inf,ymin=log2(dobj$threshold),fill='#9EC086',inherit.aes = FALSE,alpha=0.02) +
            #geom_rect(xmin=-Inf,xmax=Inf,ymax=log2(M$threshold),ymin=-log2(M$threshold),fill='#B6B6B6',inherit.aes = FALSE,alpha=0.02) +
            geom_pointrange(aes(ymin=lci,ymax=uci),position=position_dodge(width=0.75)) +
            #geom_hline(yintercept = log2(dobj$threshold),color='red') +
            #geom_hline(yintercept = -log2(dobj$threshold),color='red') +
            xlab('Feature') +
            ylab('log2(Fold change)')+
            scale_colour_Publication() +
            theme_Publication(base_size = 12)

        if (obj$orientation=='landscape') {
            out=out+coord_flip()

        } else {
            out=out+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))
        }

        return(out)
    }
)







