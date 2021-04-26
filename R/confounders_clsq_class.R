#' @eval get_description('confounders_clsq')
#' @import struct
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = filter_by_name(mode='include',dimension='variable',
#'         names=colnames(D$data)[1:10]) + # first 10 features
#'     filter_smeta(mode='exclude',levels='QC',
#'         factor_name='Class') + # reduce to two group comparison
#'     confounders_clsq(factor_name = 'Class',
#'         confounding_factors=c('run_order','Batch'))
#' M = model_apply(M,D)
#' @export confounders_clsq
confounders_clsq = function(alpha=0.05,mtc='fdr',factor_name,
    confounding_factors,threshold=0.15,...) {
    out=struct::new_struct('confounders_clsq',
        alpha=alpha,
        mtc=mtc,
        factor_name=factor_name,
        confounding_factors=confounding_factors,
        threshold=threshold,
        ...)
    return(out)
}


.confounders_clsq<-setClass(
    "confounders_clsq",
    contains='model',
    slots=c(
        # INPUTS
        alpha='entity_stato',
        mtc='enum_stato',
        factor_name='entity',
        confounding_factors='entity',
        threshold='entity',

        # OUTPUTS
        coefficients='data.frame',
        percent_change='data.frame',
        p_value='data.frame',
        potential_confounders='list',
        significant='data.frame'
    ),
    prototype = list(
        name='Check for confounding factors',
        description=paste0(
            'Univariate least squares regression models are used to compare ',
            'models with and without potential confounding factors included. ',
            'The change in coefficients (delta) is then computed for each ',
            'potential confounding factor. Factors with a large delta are ',
            'said to be having a large impact on the model and are therefore ',
            'confounding. p-values are computed for models with confounders ',
            'included to reduce potential false positives. Only suitable for ',
            'main factors with 2 levels.'),
        type="univariate",
        predicted='p_value',
        .params=c('alpha','mtc','factor_name','confounding_factors','threshold'),
        .outputs=c('coefficients','p_value','significant','percent_change',
            'potential_confounders'),

        threshold=entity(name='Confounding factor threshold',
            type='numeric',
            description=paste0('Factors with a delta greater than the ',
                'the threshold are considered to be confounding.'),
            value=0.15,
            max_length=1
        ),

        confounding_factors=entity(name='Confounding factors',
            type='character',
            description='The name(s) of factor(s) that are potential confounding factors.'
        ),

        factor_name=entity(name='Factor name',
            type='character',
            description='The name of the main factor with which other factors may be confounding.'
        ),

        alpha=ents$alpha,
        mtc=ents$mtc
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("confounders_clsq",'DatasetExperiment'),
    definition=function(M,D)
    {
        # classical least squares model
        clsq=classical_lsq(intercept=TRUE,alpha=M$alpha,mtc=M$mtc,factor_names='dummy')

        # make list of all factors
        factor_names=c(M$factor_name,M$confounding_factors)

        # do a regression including the main factor and the confounders one at a time
        temp=matrix(NA,nrow=ncol(D$data),ncol=length(factor_names)) # coefficients
        pvals=temp # p-values
        nm=character(length(factor_names))
        for (i in 1:length(factor_names)) {
            fn=unique(c(factor_names[1],factor_names[i]))

            # for each factor name check the na count
            FF=filter_na_count(threshold=2,factor_name='dummy')
            excl=matrix(NA,nrow=ncol(D$data),ncol=length(fn))
            colnames(excl)=fn
            for (k in fn) {
                if (is.factor(D$sample_meta[,k])) {
                    FF$factor_name=k # replace dummy factor name
                    FF=model_apply(FF,D)
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

            clsq$factor_names=excl # put real factor names instead of dummy
            clsq=model_apply(clsq,D)

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
        M2=model_apply(M2,D)

        M$p_value=data.frame('ttest.p'=pvals[,1],'corrected.p'=M2$p_value[,2]) # MTC already applied
        names(L)=colnames(D$data)
        M$potential_confounders=as.list(L)
        M$significant=data.frame('sig'=M$p_value<M$alpha)

        return(M)
    }
)



##### plots
#' @eval get_description('confounders_lsq_barchart') 
#' @import struct
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = filter_by_name(mode='include',dimension='variable',
#'         names=colnames(D$data)[1:10]) + # first 10 features
#'     filter_smeta(mode='exclude',levels='QC',
#'         factor_name='Class') + # reduce to two group comparison
#'     confounders_clsq(factor_name = 'Class',
#'         confounding_factors=c('run_order','Batch'))
#' M = model_apply(M,D)
#' C = C=confounders_lsq_barchart(feature_to_plot=1,threshold=15)
#' chart_plot(C,M[3])
#' @export confounders_lsq_barchart
confounders_lsq_barchart = function(feature_to_plot,threshold=10,...) {
    out=struct::new_struct('confounders_lsq_barchart',
        feature_to_plot=feature_to_plot,
        threshold=threshold,
        ...)
    return(out)
}


.confounders_lsq_barchart<-setClass(
    "confounders_lsq_barchart",
    contains='chart',
    slots=c(
        # INPUTS
        feature_to_plot='entity',
        threshold='entity'
    ),
    prototype = list(name='Confounding factor relative change barchart',
        description=paste0('A barchart of the relative change (delta) in ',
        'regression coefficient when potential confounding factors are ',
        'included, and excluded, from the model. Factors with a large delta ',
        'are considered to be confounding factors.'),
        type="barchart",
        .params=c('feature_to_plot','threshold'),

        feature_to_plot=entity(name='Feature to plot',
            value=1,
            type=c('numeric','character','integer'),
            description='The column name of the feature to be plotted.'
        ),
        threshold=entity(name='Threshold',
            value=10,
            type='numeric',
            description=paste0('A horizontal line is plotted to indicate ',
            'the threshold.')
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("confounders_lsq_barchart",'confounders_clsq'),
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

#' @eval get_description('confounders_lsq_boxplot')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' M = filter_by_name(mode='include',dimension='variable',
#'         names=colnames(D$data)[1:10]) + # first 10 features
#'     filter_smeta(mode='exclude',levels='QC',
#'         factor_name='Class') + # reduce to two group comparison
#'     confounders_clsq(factor_name = 'Class',
#'         confounding_factors=c('run_order','Batch'))
#' M = model_apply(M,D)
#' C = C=confounders_lsq_boxplot(threshold=15)
#' chart_plot(C,M[3])
#' @export confounders_lsq_boxplot
confounders_lsq_boxplot = function(threshold=10,...) {
    out=struct::new_struct('confounders_lsq_boxplot',
        threshold=threshold,
        ...)
    return(out)
}


.confounders_lsq_boxplot<-setClass(
    "confounders_lsq_boxplot",
    contains='chart',
    slots=c(
        # INPUTS
        threshold='entity'
    ),
    prototype = list(name='Confounding factor relative change boxplot',
        description='Confounding factor relative change barchart',
        description=paste0('A boxplot of the relative change (delta) in ',
            'regression coefficient when potential confounding factors are ',
            'included, and excluded, from the model. Factors with a large delta ',
            'are considered to be confounding factors.'),
        type="barchart",
        .params=c('threshold'),

        threshold=entity(name='Threshold',
            value=10,
            type='numeric',
            description=paste0('A horizontal line is plotted to indicate ',
                'the threshold.')
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("confounders_lsq_boxplot",'confounders_clsq'),
    definition=function(obj,dobj)
    {
        A=data.frame('percent_change'=double(),'group'=character())
        for (i in 1:length(dobj$confounding_factors)) {
            q=data.frame('percent_change'=double(nrow(dobj$percent_change)),'group'=character(nrow(dobj$percent_change)))
            q$percent_change=dobj$percent_change[,i+1]*100
            q$group=colnames(dobj$percent_change)[i+1]
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




