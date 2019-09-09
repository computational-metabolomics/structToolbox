#' Check for confounding factors in ttest
#'
#' Compares the coefficients for a ttest without including confounding factors
#' to models with confounding factor included. Currently only ttest is supported.
#'
#' @import struct
#'
#' @param alpha p-value threshold for determining significance. Default alpha = 0.05.
#' @param mtc multiple test correction method to apply. Can be: holm, hochberg,
#' hommel, bonferroni, BH, BY, fdr or [none]
#' @param factor_name the column name of sample_meta to use in regression
#' @param confounding_factors the column names of factors potentially confounding
#' with the main factor if interest
#' @param threshold the threshold (between 0 and 1) for accepting a factor as confounding
#'
#' @return A STRUCT method object with functions for applying classical least squares
#'
#' @examples
#' D = sbcms_dataset()
#' M = filter_by_name(mode='include',dimension='variable',names=colnames(D$data)[1:10]) + # first 10 features
#'     filter_smeta(mode='exclude',levels='QC',factor_name='class') + # reduce to two group comparison
#'     confounders_clsq(factor_name = 'class',confounding_factors=c('sample_order','batch'))
#' M = method.apply(M,D)
#'
#' @export confounders_clsq
confounders_clsq<-setClass(
    "confounders_clsq",
    contains='method',
    slots=c(
        # INPUTS
        params.alpha='entity.stato',
        params.mtc='entity.stato',
        params.factor_name='entity',
        params.confounding_factors='entity',
        params.threshold='entity',

        # OUTPUTS
        outputs.coefficients='data.frame',
        outputs.percent_change='data.frame',
        outputs.p_value='data.frame',
        outputs.potential_confounders='list',
        outputs.significant='data.frame'
    ),
    prototype = list(name='ttest with confounding factors',
        description='Applies least squares regression to account for confounding factors in when applying a ttest.',
        type="univariate",
        predicted='p_value',

        params.threshold=entity(name='Confounding factor threshold',
            type='numeric',
            description='threshold for accepting a factor as confounding (0 < threshold < 1)',
            value=0.15
        ),

        params.confounding_factors=entity(name='Confounding factors',
            type='character',
            description='Names of sample_meta columns to use as confounding factors'
        ),

        params.factor_name=entity(name='Name of factor to use for ttest',
            type='character',
            description='Names of sample_meta column to use for the ttest'
        ),

        params.alpha=entity.stato(name='Confidence level',
            stato.id='STATO:0000053',
            value=0.05,
            type='numeric',
            description='the p-value cutoff for determining significance.'
        ),
        params.mtc=entity.stato(name='Multiple testing Correction method',
            stato.id='OBI:0200089',
            value='fdr',
            type='character',
            description='The method used to adjust for multiple comparisons.'
        )
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("confounders_clsq",'dataset'),
    definition=function(M,D)
    {
        # classical least squares model
        clsq=classical_lsq(intercept=TRUE,alpha=M$alpha,mtc=M$mtc)

        # make list of all factors
        factor_names=c(M$factor_name,M$confounding_factors)

        # do a regression including the main factor and the counfounders one at a time
        temp=matrix(NA,nrow=ncol(D$data),ncol=length(factor_names)) # coefficients
        pvals=temp # p-values
        nm=character(length(factor_names))
        for (i in 1:length(factor_names)) {
            fn=unique(c(factor_names[1],factor_names[i]))

            # for each factor name check the na count
            FF=filter_na_count(threshold=2)
            excl=matrix(NA,nrow=ncol(D$data),ncol=length(fn))
            colnames(excl)=fn
            for (k in fn) {
                if (is.factor(D$sample_meta[,k])) {
                    FF$factor_name=k
                    FF=method.apply(FF,D)
                    excl[,k]=FF$flags$flags
                } else {
                    excl[,k]=FALSE
                }
            }

            if (!all(!excl)) {
                excl=apply(excl,1,function(x) {
                    fn[!x]
                })
            } else {
                excl=fn #
            }

            clsq$factor_names=excl
            clsq=method.apply(clsq,D)

            nm[i]=paste0(fn,collapse='_')
            temp[,i]=clsq$coefficients[,2] # first coefficient is the intercept, second is the main factor
            pvals[,i]=clsq$p_value[,2]   # for main factor
        }
        colnames(temp)=factor_names
        rownames(temp)=colnames(D$data)

        # calculate change in coefficient when including additional factor
        temp2=matrix(0,nrow=ncol(D$data),ncol=length(factor_names))
        for (i in 1:length(factor_names)) {
            temp2[,i]=abs(temp[,1]-temp[,i])/abs(temp[,1]) # (main-confounder)/confounder
        }
        colnames(temp2)=factor_names
        rownames(temp2)=colnames(D$data)

        M$coefficients=as.data.frame(temp)
        M$percent_change=as.data.frame(temp2)

        # redo regression including all potential confounders for each feature
        conf=M$percent_change>M$threshold
        factor_names=M$confounding_factors
        L=apply(conf[,2:ncol(conf),drop=FALSE],1,function(x) c(M$factor_name,factor_names[x]))
        M2=classical_lsq(intercept=TRUE,alpha=M$alpha,mtc=M$mtc,factor_names=L)
        M2=method.apply(M2,D)

        M$p_value=data.frame('ttest.p'=pvals[,1],'corrected.p'=M2$p_value[,2]) # MTC already applied
        names(L)=colnames(D$data)
        M$potential_confounders=as.list(L)
        M$significant=data.frame('sig'=M$p_value<M$alpha)

        return(M)
    }
)



##### plots
#' barchart of percent change
#'
#' plots a barchart of the percent change when including a confounding factor in a classical least squares model
#' @import struct
#' @param feature_to_plot the name or index of the feature to be plotted
#' @param threshold the threshold to be plotted (in \%)
#'
#' @return A STRUCT chart object
#'
#' @examples
#' D = sbcms_dataset()
#' M = filter_by_name(mode='include',dimension='variable',names=colnames(D$data)[1:10]) + # first 10 features
#'     filter_smeta(mode='exclude',levels='QC',factor_name='class') + # reduce to two group comparison
#'     confounders_clsq(factor_name = 'class',confounding_factors=c('sample_order','batch'))
#' M = method.apply(M,D)
#' C = C=confounders_lsq.barchart(feature_to_plot=1,threshold=15)
#' chart.plot(C,M[3])
#'
#' @export confounders_lsq.barchart
confounders_lsq.barchart<-setClass(
    "confounders_lsq.barchart",
    contains='chart',
    slots=c(
        # INPUTS
        params.feature_to_plot='entity',
        params.threshold='entity'
    ),
    prototype = list(name='Percent change',
        description='a barchart of the percent change when including a confounding factor in a classical least squares model.',
        type="barchart",
        params.feature_to_plot=entity(name='Feature to plot',
            value=1,
            type='numeric',
            description='The name of the feature to be plotted.'
        ),
        params.threshold=entity(name='Threshold',
            value=10,
            type='numeric',
            description='A horizontal line plotted to indicate the threshold'
        )
    )
)

#' @export
setMethod(f="chart.plot",
    signature=c("confounders_lsq.barchart",'confounders_clsq'),
    definition=function(obj,dobj)
    {
        # if numeric then use it as an index, else use the row name
        if (is.numeric(obj$feature_to_plot)) {
            varn=obj$feature_to_plot
        } else {
            varn=which(rownames(dobj$percent_change)==obj$feature_to_plot)
        }

        N=ncol(dobj$percent_change)
        A=data.frame(percent_change=t(dobj$percent_change[varn,2:N])*100,group=colnames(dobj$percent_change)[2:N])
        colnames(A)=c('percent_change','group')
        out=ggplot(data=A,aes_(x=~group,y=~percent_change,fill=~group)) +
            geom_bar(stat='identity') +
            theme_Publication(base_size = 12) +
            ylab('Percent change') +
            xlab('') +
            theme(legend.position="none") +
            scale_fill_manual(values=c("#386cb0", "#ef3b2c", "#7fc97f", "#fdb462", "#984ea3",
                "#a6cee3", "#778899", "#fb9a99", "#ffff33")) +
            geom_hline(yintercept = obj$threshold,color='black',size=1,linetype= "dashed") + ggtitle(rownames(dobj$percent_change)[varn])

        return(out)
    }
)

#' boxplot of percent change
#'
#' Plots a boxplot of the percent change over all features when including a
#' confounding factor in the ttest
#' @import struct
#' @param threshold the threshold to be plotted (in \%)
#'
#' @return A STRUCT chart object
#'
#' @examples
#' D = sbcms_dataset()
#' M = filter_by_name(mode='include',dimension='variable',names=colnames(D$data)[1:10]) + # first 10 features
#'     filter_smeta(mode='exclude',levels='QC',factor_name='class') + # reduce to two group comparison
#'     confounders_clsq(factor_name = 'class',confounding_factors=c('sample_order','batch'))
#' M = method.apply(M,D)
#' C = C=confounders_lsq.boxplot(threshold=15)
#' chart.plot(C,M[3])
#'
#' @export confounders_lsq.boxplot
confounders_lsq.boxplot<-setClass(
    "confounders_lsq.boxplot",
    contains='chart',
    slots=c(
        # INPUTS
        params.threshold='entity'
    ),
    prototype = list(name='Percent change',
        description='a barchart of the percent change when including a confounding factor in a classical least squares model.',
        type="barchart",
        params.threshold=entity(name='Threshold',
            value=10,
            type='numeric',
            description='A horizontal line plotted to indicate the threshold'
        )
    )
)

#' @export
setMethod(f="chart.plot",
    signature=c("confounders_lsq.boxplot",'confounders_clsq'),
    definition=function(obj,dobj)
    {

        A=data.frame('percent_change'=double(),'group'=character())
        for (i in 2:length(dobj$confounding_factors)) {
            q=data.frame('percent_change'=double(nrow(dobj$percent_change)),'group'=character(nrow(dobj$percent_change)))
            q$percent_change=dobj$percent_change[,i]*100
            q$group=colnames(dobj$percent_change)[i]
            colnames(q)=c('percent_change','group')
            A=rbind(A,q)
        }

        colnames(A)=c('percent_change','group')
        out=ggplot(data=A,aes_(x=~group,y=~percent_change,colour=~group)) +
            geom_boxplot() +
            theme_Publication(base_size = 12) +
            ylab('Percent change') +
            xlab('') +
            theme(legend.position="none") +
            scale_color_manual(values=c("#386cb0", "#ef3b2c", "#7fc97f", "#fdb462", "#984ea3",
                "#a6cee3", "#778899", "#fb9a99", "#ffff33")) +
            geom_hline(yintercept = obj$threshold,color='black',size=1,linetype= "dashed")




        return(out)
    }
)




