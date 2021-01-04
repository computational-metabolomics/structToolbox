#' @eval get_description('fold_change')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = fold_change(factor_name='class')
#' M = model_apply(M,D)
#' @import stats
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
        threshold='entity',
        control_group='entity',
        method='entity',
        
        # OUTPUTS
        fold_change='entity',
        upper_ci='entity',
        lower_ci='entity',
        significant='entity'
    ),
    prototype = list(name='Fold change',
        description=paste0('Fold change is the relative change in mean (or ',
            'non-parametric equivalent) intensities of a feature between all pairs of levels ',
            'in a factor.'),
        type="univariate",
        predicted='fold_change',
        citations=list(
            bibentry(
                bibtype='Article',
                year = 2002,
                volume = 72,
                number = 2,
                pages = "119-124",
                author = as.person('Robert M. Price and Douglas G. Bonett'),
                title = 'Distribution-Free Confidence Intervals for Difference and Ratio of Medians',
                journal = 'Journal of Statistical Computation and Simulation'
            )
        ),
        #  stato_id="STATO:0000304",
        .params=c('factor_name','sample_name','alpha','paired','threshold','control_group','method'),
        .outputs=c('fold_change','lower_ci','upper_ci','significant'),
        
        factor_name=ents$factor_name,
        sample_name=entity(name='Sample names',
            type='character',
            description='The name of a sample_meta column containing sample identifiers for paired sampling.'
        ),
        alpha=ents$alpha,
        paired=entity(name='Paired fold change',
            value=FALSE,
            type='logical',
            description=c(
                'TRUE' = 'Fold change is calculated taking into account paired sampling.',
                'FALSE'= 'Fold change is calculated assuming there is no paired sampling.'),
        ),
        
        fold_change=entity(name='Fold change',
            type='data.frame',
            description='The fold change between groups',
            value=data.frame()
        ),
        
        lower_ci=entity(name='Lower confidence interval',
            type='data.frame',
            description='Lower confidence interval for fold change',
            value=data.frame()
        ),
        upper_ci=entity(name='Upper confidence interval',
            type='data.frame',
            description='Upper confidence interval for fold change.',
            value=data.frame()
        ),
        method=enum(name='Fold change method',
            type='character',
            description=c(
                'geometric' = paste0('A log transform and a t-test is used ',
                    'to calculate fold change and estimate confidence ',
                    'intervals. In the non-transformed space this is ',
                    'equivalent to fold change using geometric means.'),
                "median" = paste0('A log transform and the method described ',
                    'by Price and Bonett to calculate fold change and ',
                    'estimate confidence intervals. In the non-transformed ',
                    'space this is equivalent to using group medians to ',
                    'calculate fold change.')
            ),
            value="geometric",
            allowed=c("geometric","median")
        ),
        control_group=entity(
            name='Control group',
            description = paste0('The level name of the group used in ',
                'the denominator (where possible) when computing fold change.'
            ),
            type='character',
            max_length = 1,
            value=character(0)
        ),
        threshold=entity(
            name='threshold',
            description = paste0('The fold change threshold for labelling ',
                'features as significant.'
            ),
            type='numeric',
            max_length = 1,
            value=2
        ),
        significant = entity(
            name='Significant features',
            description=paste0('A logical indictor of whether the fold change ', 
                'for a feature is greater than the threshold.'),
            type='data.frame',
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
        if (M$method=='geometric') {
            # log transform
            D$data=log2(D$data)
        }
        X=D$data
        Y=D$sample_meta
        
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
                    
                    #store in object
                    M$fold_change=as.data.frame(2^FC)
                    M$lower_ci=as.data.frame(2^LCI)
                    M$upper_ci=as.data.frame(2^UCI)
                    
                } else {
                    
                    D = predicted(FG)
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
                    
                    # store in object
                    M$fold_change = as.data.frame(FC)
                    M$lower_ci = as.data.frame(LCI)
                    M$upper_ci = as.data.frame(UCI)
                }
            }
        }
        
        
        
        colnames(FC)=comp
        colnames(LCI)=comp
        colnames(UCI)=comp
   
        M$significant=as.data.frame((UCI < (-log2(M$threshold))) | (LCI>log2(M$threshold)))
        colnames(M$significant)=comp
        
        return(M)
    }
)

#' @eval get_description('fold_change_plot')
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
        number_features='entity',
        orientation='entity'),
    prototype = list(name='Fold change plot',
        description=paste0('A plot of fold changes calculated for a chosen ',
        'subset of features. A predefined fold change threshold is indicated ',
        'by shaded regions.'),
        type="boxlot",
        .params=c('number_features','orientation'),
        number_features=entity(
            name='Features to plot',
            description = 'The number randomly selected features to plot, or 
            a list of column numbers.',
            type='numeric',
            value=10
        ),
        orientation=enum(
            name='Plot orientation',
            description = c(
            'landscape' = 'Features are plotted on the y-axis.',
            'portrait' = 'Features are plotted on the x-axis.'
            ),
            type='character',
            value='portrait',
            allowed=c('portrait','landscape'),
            max_length=1
        )
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
        
        
        A=data.frame(
            'fc'=dobj$fold_change[S,1],
            'group'=colnames(dobj$fold_change)[1],
            'lci'=dobj$lower_ci[S,1],
            'uci'=dobj$upper_ci[S,1],
            'feature'=rownames(dobj$fold_change)[S])
        if (ncol(dobj$fold_change)>1) {
            for (k in 2:ncol(dobj$fold_change)) {
                B=data.frame(
                    'fc'=dobj$fold_change[S,k],
                    'group'=colnames(dobj$fold_change)[k],
                    'lci'=dobj$lower_ci[S,k],
                    'uci'=dobj$upper_ci[S,k],
                    'feature'=rownames(dobj$fold_change)[S])
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
        # For ratio n1/n2
        n1=median(y1,na.rm=TRUE)
        n2=median(y2,na.rm=TRUE)
        z=qnorm(1-(alpha/2))
        ci=z*(var_nu(log(y1))+var_nu(log(y2)))^0.5
        out=c(n1/n2,(n1/n2)*exp(-ci),(n1/n2)*exp(ci))
        return(out)
    } else {
        if (length(y1) != length(y2)) {
            stop('all samples must be present in all groups for a paired comparison')
        }
        # median of pairwise fold changes
        delta=sort(y1/y2)
        delta_nu=median(delta)
        # confidence intervals from wilcox.test
        out=wilcox.test(
            delta,
            conf.level=1-alpha,
            alternative="two.sided",
            correct=TRUE,
            conf.int = TRUE
        )
        out=c(delta_nu,out$conf.int[1],out$conf.int[2])
        return(out)
    }
    
}

