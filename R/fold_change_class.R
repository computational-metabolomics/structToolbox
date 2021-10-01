#' @eval get_description('fold_change')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = fold_change(factor_name='Class')
#' M = model_apply(M,D)
#' @import stats
#' @export fold_change
fold_change = function(
    factor_name,
    paired=FALSE,
    sample_name=character(0),
    threshold=2,
    control_group=character(0),
    method = "geometric",
    conf_level = 0.95,
    ...) {
    
    out=struct::new_struct('fold_change',
        factor_name=factor_name,
        paired=paired,
        sample_name=sample_name,
        threshold=threshold,
        control_group=control_group,
        method = method,
        conf_level=conf_level,
        ...)
    
    return(out)
}


.fold_change<-setClass(
    "fold_change",
    contains=c('model'),
    slots=c(
        # INPUTS
        factor_name='entity',
        paired='entity',
        sample_name='entity',
        threshold='entity',
        control_group='entity',
        method='entity',
        conf_level='entity',
        
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
                year=2020,
                volume=45,
                number=6,
                pages='750-770',
                author=as.person('Robert M. Price Jr and Douglas G. Bonett'),
                title='Confidence Intervals for Ratios of Means and Medians',
                journal='Journal of Educational and Behavioral Statistics'
            )
        ),
        #  ontology="STATO:0000304",
        .params=c('factor_name','sample_name','paired','threshold','control_group','method','conf_level'),
        .outputs=c('fold_change','lower_ci','upper_ci','significant'),
        
        factor_name=ents$factor_name,
        sample_name=entity(name='Sample names',
            type='character',
            description='The name of a sample_meta column containing sample identifiers for paired sampling.'
        ),

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
                'geometric' = paste0('A log transform is applied before using ',
                    'group means to calculate fold change. In the non-tranformed',
                    'space this is equivalent to using geometric group means. ',
                    'Confidence intervals for independant and paired sampling ',
                    'are estimated using standard error of the mean in log ',
                    'transformed space before being transformed back to the ',
                    'original space.'),
                "median" = paste0('The group medians and the method described ',
                    'by Price and Bonett[1] is used to ',
                    'estimate confidence intervals. For paired data standard ',
                    'error of the median is used to estimate confidence ',
                    'intervals from the median fold change of all pairs.'),
                "mean" = paste0('The group means and the method described by ',
                    'Price and Bonnet[1] is used to estimate confidence ',
                    'intervals. For paired data standard error of the mean is ',
                    'used to estimate confidence intervals from the mean ',
                    'fold change of all pairs.')
            ),
            value="geometric",
            allowed=c("geometric","median","mean")
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
            description=paste0('A logical indictor of whether the calculated ',
            'fold change including the estimated confidence limits is greater ',
            'than the selected threshold.'),
            type='data.frame',
            value=data.frame()
        ),
        conf_level = entity(
            name = 'Confidence level',
            description = 'The confidence level of the interval.',
            type='numeric',
            value=0.95,
            max_length = 1
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
        L=rev(L)
        # put control group last, if provided
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
                    TT=ttest(alpha=0.05,mtc='none',factor_names=M$factor_name,paired=M$paired,paired_factor=M$sample_name,conf_level=M$conf_level)
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
                    
                } else if (M$method == 'median') {
                    
                    D2 = predicted(FG)
                    # check for pairs in features
                    if (M$paired) {
                        FF=pairs_filter(
                            factor_name=M$factor_name,
                            sample_id=M$sample_name)
                        FF=model_apply(FF,D2)
                        D2=predicted(FF)
                    }
                    
                    
                    # calculate medians and confidence intervals for all features
                    out=lapply(D2$data,function(x) {
                        y1=x[D2$sample_meta[[M$factor_name]]==L[A]]
                        y2=x[D2$sample_meta[[M$factor_name]]==L[B]]
                        val=ci_delta_nu(na.omit(y1),na.omit(y2),alpha=1-M$conf_level,paired=M$paired)
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
                    
                } else if (M$method == 'mean') {
                    D2 = predicted(FG)
                    # check for pairs in features
                    if (M$paired) {
                        FF=pairs_filter(
                            factor_name=M$factor_name,
                            sample_id=M$sample_name)
                        FF=model_apply(FF,D2)
                        D2=predicted(FF)
                        
                        # calculate means and confidence intervals for all features
                        out=lapply(D2$data,function(x) {
                            y1=x[D2$sample_meta[[M$factor_name]]==L[A]]
                            y2=x[D2$sample_meta[[M$factor_name]]==L[B]]
                            ret=ci.mean.paired(1-M$conf_level,y1,y2)
                            return(ret)
                        })
                        
                        
                    } else {
                        # calculate means and confidence intervals for all features
                        out=lapply(D2$data,function(x) {
                            y1=x[D2$sample_meta[[M$factor_name]]==L[A]]
                            y2=x[D2$sample_meta[[M$factor_name]]==L[B]]
                            ret=ci.mean.bs(1-M$conf_level,na.omit(y1),na.omit(y2))
                            return(ret)
                        })
                        
                        
                    }
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
                    
                    if (M$paired) {
                        FC[,counter]=temp[,3]
                        LCI[,counter]=temp[,4]
                        UCI[,counter]=temp[,5]
                    } else {
                        FC[,counter]=temp[,6]
                        LCI[,counter]=temp[,7]
                        UCI[,counter]=temp[,8]
                    }
                    counter=counter+1
                    comp=c(comp,paste0(L[A],'/',L[B]))
                }
            }
        }

        colnames(FC)=comp
        colnames(LCI)=comp
        colnames(UCI)=comp

        # store in object
        if (M$method=='geometric') {
            #store in object
            M$fold_change=as.data.frame(2^FC)
            M$lower_ci=as.data.frame(2^LCI)
            M$upper_ci=as.data.frame(2^UCI) 
            M$significant=as.data.frame((UCI < (-log2(M$threshold))) | (LCI>log2(M$threshold)))
        } else {
            M$fold_change = as.data.frame(FC)
            M$lower_ci = as.data.frame(LCI)
            M$upper_ci = as.data.frame(UCI)
            M$significant=as.data.frame((UCI < (-M$threshold)) | (LCI>(M$threshold)))
        }
        
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





ci_delta_nu = function(y1,y2,alpha=0.05,paired=FALSE) {
    
    if (!paired) {
        out=ci.median.bs(alpha=alpha,y1=y1,y2=y2)
        return(out)
    } else {
        if (length(y1) != length(y2)) {
            stop('all samples must be present in all groups for a paired comparison')
        } 
        r = y1/y2
        mr=mean(r)
        md=median(r)
        s=sd(r)/sqrt(length(r)) # standard error of mean
        z=qt(1-(alpha/2),length(r)-1)
        sf=sqrt(pi/2)
        
        out=t(c(md,md-(sf*z*s),md+(sf*z*s)))
        colnames(out)=c('fold_change','lower_ci','upper_ci')
        
        return(out)
    }
    
}

ci.mean.bs <- function(alpha, y1, y2){
    # from 10.3102/1076998620934125
    # Compute confidence interval for a ratio of population means of ratio-scale
    # measurements in a design with a 2-level between-subjects factor. 
    # Arguments:
    #   alpha:  alpha level for 1-alpha confidence
    #   y1      n1 x 1 vector of scores for group 1
    #   y2:     n2 x 1 vector of scores for group 2
    # Returns:
    #   confidence interval
    n1 <- length(y1)
    n2 <- length(y2)
    m1 <- mean(y1)
    m2 <- mean(y2)
    v1 <- var(y1)
    v2 <- var(y2)
    var <- v1/(n1*m1^2) + v2/(n2*m2^2)
    df <- var^2/(v1^2/(m1^4*(n1^3 - n1^2)) + v2^2/(m2^4*(n2^3 - n2^2)))
    t <- qt(1 - alpha/2, df)
    est <- log(m1/m2)
    se <- sqrt(var)
    ll <- exp(est - t*se)
    ul <- exp(est + t*se)
    out <- t(c(m1, m2, df, exp(est), ll, ul, est, se))
    colnames(out) <- c("Mean1", "Mean2", "df", "  Mean1/Mean2", "LL", "UL", "  Log-ratio", "SE")
    return(out)
}



ci.mean.paired = function(alpha,x,y) {
    
    r = x/y
    mr=mean(r)
    s=sd(r)/sqrt(length(x)) # standard error of mean
    z=qt(1-(alpha/2),length(x)-1)
    
    out=t(c(mr,mr-(z*s),mr+z*s))
    colnames(out)=c('fold_change','lower_ci','upper_ci')
    
    return(out)
}



ci.median.bs <- function(alpha, y1, y2) {
    # from 10.3102/1076998620934125
    # Computes confidence interval for a ratio of population medians of ratio-scale
    # measurements in a design with a 2-level between-subjects factor.
    # Arguments:
    #   alpha: alpha level for 1-alpha confidence
    #   y1:    n1 x 1 vector of scores for group 1
    #   y2:    n2 x 1 vector of scores for group 2  
    # Returns:
    #   confidence interval
    z <- qnorm(1 - alpha/2)
    n1 <- length(y1)
    y1 <- sort(y1)
    n2 <- length(y2)
    y2 <- sort(y2)
    med1 <- median(y1)
    med2 <- median(y2)
    o1 <- round(n1/2 - sqrt(n1))
    if (o1 < 1) {o1 = 1}
    o2 <- n1 - o1 + 1
    l1 <- log(y1[o1])
    u1 <- log(y1[o2])
    p <- pbinom(o1 - 1, size = n1, prob = .5)
    z0 <- qnorm(1 - p)
    se1 <- (u1 - l1)/(2*z0)
    o1 <- round(n2/2 - sqrt(n2))
    if (o1 < 1) {o1 = 1}
    o2 <- n2 - o1 + 1
    l2 <- log(y2[o1])
    u2 <- log(y2[o2])
    p <- pbinom(o1 - 1, size = n2, prob = .5)
    z0 <- qnorm(1 - p)
    se2 <- (u2 - l2)/(2*z0)
    se <- sqrt(se1^2 + se2^2)
    logratio <- log(med1/med2)
    ll <- exp(logratio - z*se)
    ul <- exp(logratio + z*se)
    out <- t(c(exp(logratio), ll, ul))
    colnames(out) <- c("fold_change", "LL", "UL")
    return(out)
}
