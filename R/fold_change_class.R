#' fold change class
#'
#' Calculates fold change between groups for all features in a 
#' DatasetExperiment.
#'
#' @examples
#' D = MTBLS79_DatasetExperiment()
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
#' @param method the method used to calculate fold change. `method = "geometric"`
#' uses a log transform and a t-test to calculate fold change and estimate
#' confidence intervals. In the non-transformed space this is equivalent to
#' fold change using geometric means. `method = "median"` uses a log transform
#' and the method described in DOI: 10.1080/00949650212140 to calculate fold
#' change and estimate confidence intervals. In the non-transformed space this 
#' is equivalent to using group medians to calculate fold change.
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
    method = "geometric",
    ...) {

    out=struct::new_struct('fold_change',
        alpha=alpha,
        factor_name=factor_name,
        paired=paired,
        sample_name=sample_name,
        threshold=threshold,
        control_group=control_group,
        method = method,
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
        method='entity',

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
        .params=c('factor_name','sample_name','alpha','paired','threshold','control_group','method'),
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
        ),
        method=enum(name='Fold change method',
            type='character',
            description='The method used to calculate fold change. "geometric" or
            "median"',
            value="geometric",
            allowed=c("geometric","median")
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
            if (length(w)>0) {
                L=c(L[-w],L[w])
            }
        }

        # number of pairwise comparisons
        n=((length(L)^2)-length(L))/2

        # prep some matrices
        FC=matrix(NA,nrow=ncol(X),ncol=n)
        rownames(FC)=colnames(D)
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
                
                
                if (M$method=='geometric') {
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
                } else {
                    # check for pairs in features
                    if (M$paired) {
                        FF=pairs_filter(
                            factor_name=M$factor_name,
                            sample_id=M$sample_name)
                        FF=model_apply(FF,D)
                        D=predicted(FF)
                    }
                    

                    # calculate medians and confidence intervals for all features
                    out=lapply(D$data,function(x) {
                        y1=x[D$sample_meta[[M$factor_name]]==L[A]]
                        y2=x[D$sample_meta[[M$factor_name]]==L[B]]
                        val=ci_delta_nu(y1,y2,alpha=M$alpha,paired=M$paired)
                        val=matrix(val,nrow=1,ncol=3)
                        colnames(val)=c('median','lci','uci')
                        return(val)
                    })
                    
                    nmes=names(out)
                    out=do.call(rbind,out)
                    rownames(out)=nmes
                    
                    # merge with original names
                    temp=merge(
                        FC[,counter,drop=FALSE],
                        out,
                        by = 'row.names',
                        all.x=TRUE,
                        )
                    rownames(temp)=temp$Row.names
                    # sort back into original order
                    temp=temp[rownames(FC),]
                    
                    FC[,counter]=temp$median
                    LCI[,counter]=temp$lci
                    UCI[,counter]=temp$uci
                    counter=counter+1
                    comp=c(comp,paste0(L[A],'/',L[B]))
                }
            }
        }

        FC=as.data.frame(FC)
        LCI=as.data.frame(LCI)
        UCI=as.data.frame(UCI)

        colnames(FC)=comp
        colnames(LCI)=comp
        colnames(UCI)=comp

        #rownames(FC)=colnames(D$data)
        #rownames(LCI)=colnames(D$data)
        #rownames(UCI)=colnames(D$data)

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





var_nu = function(y) {
    n=length(y)
    c=((n+1)/2)-n^(1/2)
    # "c is rounded to nearest non-zero integer"
    c=max(c(round(c),1))
    i=seq(from=0,to=c-1,by=1)
    p=sum( (factorial(n)/(factorial(i)*factorial(n-i)) ) * ((0.5)^(n-1)) )
    # NB error in paper has ^(n-i)
    
    # "for large n set z = 2"
    if (n<=50) {
        z=qnorm(1-(p/2))
    } else {
        z=2
    }
    v=(y[n-c+1]-y[c])/(2*z)
    v=v^2
    return(v)
}

ci_delta_nu = function(y1,y2,alpha=0.05,paired=FALSE) {
    
    if (!paired) {
        # https://www.tandfonline.com/doi/abs/10.1080/00949650212140
        nu1=median(y1)
        nu2=median(y2)
        z=qnorm(1-(alpha/2))
        ci=z*(var_nu(y1)+var_nu(y2))^0.5
        out=c(nu1-nu2,(nu1-nu2)-ci,(nu1-nu2)+ci)
        return(out)
    } else {
        # median of differences
        delta=y1-y2
        delta_nu=median(delta)
        # usual confidence in median
        n=length(y1)
        z=qnorm(1-(alpha/2))
        lci_rank=(n/2)-((z*sqrt(n))/2)
        uci_rank=1+(n/2)+((z*sqrt(n))/2)
        out=c(delta_nu,delta[round(lci_rank)],delta[round(uci_rank)])
        return(out)
    }
    
}

