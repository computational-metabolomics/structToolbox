#################################################
#################################################

#' plsda_scores_plot class
#'
#' 2d scatter plot of plsda component scores.
#'
#' @param components The PLS components to plot (\code{numeric(2)})
#' @param points_to_label "none", "all", or "outliers" will be labelled on the plot.
#' @param factor_name The sample_meta column name for labelling the legend
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export plsda_scores_plot
#' @include PLSDA_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre()+PLSDA(factor_name='Species')
#' M = model_apply(M,D)
#'
#' C = plsda_scores_plot(factor_name='Species')
#' chart_plot(C,M[2])
plsda_scores_plot = function(components=c(1,2),points_to_label='none',factor_name,...) {
    out=struct::new_struct('plsda_scores_plot',
        components=components,
        points_to_label=points_to_label,
        factor_name=factor_name,
        ...)
    return(out)
}

.plsda_scores_plot<-setClass(
    "plsda_scores_plot",
    contains='chart',
    slots=c(
        # INPUTS
        components='entity',
        points_to_label='entity',
        factor_name='entity'
    ),
    prototype = list(name='PLSDA scores plot',
        description='scatter plot of PLSDA component scores',
        type="scatter",
        libraries=c('pls','ggplot2'),
        .params=c('components','points_to_label','factor_name'),
        
        components=entity(name='Components to plot',
            value=c(1,2),
            type='numeric',
            description='the components to be plotted e.g. c(1,2) plots component 1 on the x axis and component 2 on the y axis.',
            max_length=2
        ),
        points_to_label=entity(name='points_to_label',
            value='none',
            type='character',
            description='("none"), "all", or "outliers" will be labelled on the plot.'
        ),
        factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The name of the factor to be displayed on the plot. Appears on axis and legend titles, for example. By default the column name of the meta data will be used where possible.'
        )
    )
    
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("plsda_scores_plot",'PLSDA'),
    definition=function(obj,dobj)
    {
        opt=param_list(obj)
        opt$groups=dobj$y[[obj$factor_name]]
        
        scores=output_value(dobj,'scores')
        #pvar=(colSums(scores*scores)/output_value(dobj,'ssx'))*100 # percent variance
        #pvar=round(pvar,digits = 2) # round to 2 decimal places
        shapes <- rep(19,nrow(scores)) # filled circles for all samples
        slabels <- rownames(scores)
        x=scores[,opt$components[1]]
        y=scores[,opt$components[2]]
        xlabel=paste("LV",opt$components[[1]],sep='')
        ylabel=paste("LV",opt$components[[2]],sep='')
        
        # add a space to the front of the labels to offset them from the points, because nudge_x is in data units
        for (i in 1:length(slabels))
        {
            slabels[i]=paste0('  ',slabels[i], '  ')
        }
        
        if (is(opt$groups,'factor')) {
            plotClass= createClassAndColors(opt$groups)
            opt$groups=plotClass$class
        }
        
        # build the plot
        A <- data.frame (group=opt$groups,x=x, y=y)
        out=ggplot (data=A, aes_(x=~x,y=~y,colour=~group,label=~slabels,shapes=~shapes)) +
            geom_point(na.rm=TRUE) +
            xlab(xlabel) +
            ylab(ylabel) +
            ggtitle('PLSDA Scores', subtitle=NULL) +
            stat_ellipse(type='norm') # ellipse for individual groups
        
        if (is(opt$groups,'factor')) {# if a factor then plot by group using the colours from pmp package
            out=out+scale_colour_manual(values=plotClass$manual_colors,name=opt$factor_name)
        }
        else {# assume continuous and use the default colour gradient
            out=out+scale_colour_viridis_c(limits=quantile(opt$groups,c(0.05,0.95),na.rm = TRUE),oob=squish,name=opt$factor_name)
        }
        out=out+theme_Publication(base_size = 12)
        # add ellipse for all samples (ignoring group)
        out=out+stat_ellipse(type='norm',mapping=aes_(x=~x,y=~y),colour="#C0C0C0",linetype='dashed',data=A)
        # identify samples outside the ellipse
        build=ggplot_build(out)$data
        points=build[[1]]
        ell=build[[length(build)]]
        # outlier for DatasetExperiment ellipse
        points$in.ell=as.logical(point.in.polygon(points$x,points$y,ell$x,ell$y))
        
        # label outliers if
        if (opt$points_to_label=='outliers') {
            if (!all(points$in.ell)) {
                temp=subset(points,!points$in.ell)
                temp$group=opt$groups[!points$in.ell]
                out=out+geom_text(data=temp,aes_(x=~x,y=~y,label=~label,colour=~group),vjust="inward",hjust="inward")
            }
        }
        
        # label all points if requested
        if (opt$points_to_label=='all') {
            out=out+geom_text(vjust="inward",hjust="inward")
        }
        
        # add a list of outliers to the plot object
        out$outliers=trimws(slabels[!points$in.ell])
        
        return(out)
    }
)



#################################################
#################################################

#' plsda_predicted_plot class
#'
#' A box plot of the predicted values from a PLSDA model for each class. 
#' Only usitable for two class models.
#'
#' @param factor_name The sample_meta column name to use
#' @param style The plot style. One of 'boxplot', 'violin' or 'density'.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export plsda_predicted_plot
#' @include PLSDA_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre()+PLSDA(factor_name='Species')
#' M = model_apply(M,D)
#'
#' C = plsda_predicted_plot(factor_name='Species')
#' chart_plot(C,M[2])
plsda_predicted_plot = function(factor_name,style='boxplot',...) {
    out=struct::new_struct('plsda_predicted_plot',
        factor_name=factor_name,
        style=style,
        ...)
    return(out)
}

.plsda_predicted_plot<-setClass(
    "plsda_predicted_plot",
    contains='chart',
    slots=c(
        # INPUTS
        factor_name='entity',
        style='entity'
    ),
    prototype = list(name='PLSDA predicted plot',
        description='A boxplot of PLSDA predictions',
        type="boxplot",
        libraries=c('pls','ggplot2'),
        .params=c('factor_name','style'),
        
        factor_name=ents$factor_name,
        
        style=enum(name='Plot style',
            description="The plot style. One of 'boxplot', 'violin' or 'density'",
            type='character',
            value='boxplot',
            allowed=c('boxplot','violin','density')
        )
    )
    
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("plsda_predicted_plot",'PLSDA'),
    definition=function(obj,dobj) {
        A=data.frame(Class=dobj$y[[obj$factor_name]],Predicted=dobj$yhat[,1])
        
        plotClass= createClassAndColors(A$Class)
        
        if (obj$style=='boxplot') {
            out=ggplot(data=A,aes_(x=~Class,y=~Predicted,color=~Class)) +
                scale_color_manual(values=plotClass$manual_colors,name='Class') +
                geom_boxplot() +
                geom_hline(yintercept = 0,linetype="dashed", color = "black")
            theme(legend.position="none") +xlab('Class')+ylab('Predicted Score')
        } else if (obj$style=='violin') {
            out=ggplot(data=A,aes_(x=~Class,y=~Predicted,color=~Class)) +
                scale_color_manual(values=plotClass$manual_colors,name='Class') +
                geom_violin() +
                theme(legend.position="none") +xlab('Class')+ylab('Predicted Score') +
                geom_hline(yintercept = 0,linetype="dashed", color = "black")
        } else {
            # assume histogram
            out=ggplot(data=A,aes_(x=~Predicted,fill=~Class,colour=~Class)) +
                scale_fill_manual(values=plotClass$manual_colors,name='Class') +
                scale_colour_manual(values=plotClass$manual_colors,name='Class') +
                geom_density(alpha=0.3) +
                xlab('Predicted Score')+ylab('Frequency') +
                geom_vline(xintercept = 0,linetype="dashed", color = "black")
            
        }
        out=out+theme_Publication(base_size = 12)
        return(out)
    })





#################################################
#################################################

#' plsda_roc_plot class
#'
#' Plots the ROC curve of a PLSDA model. Only suitable for two classes.
#'
#' @param factor_name The sample_meta column name to use
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export plsda_roc_plot
#' @include PLSDA_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre()+PLSDA(factor_name='Species')
#' M = model_apply(M,D)
#'
#' C = plsda_roc_plot(factor_name='Species')
#' chart_plot(C,M[2])
plsda_roc_plot = function(factor_name,...) {
    out=struct::new_struct('plsda_roc_plot',
        factor_name=factor_name,
        ...)
    return(out)
}

.plsda_roc_plot<-setClass(
    "plsda_roc_plot",
    contains='chart',
    slots=c(
        # INPUTS
        factor_name='entity'
    ),
    prototype = list(name='PLSDA ROC plot',
        description='Plots the ROC curve of a PLSDA model',
        type="roc",
        libraries=c('pls','ggplot2'),
        .params=c('factor_name'),
        
        factor_name=ents$factor_name
    )
    
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("plsda_roc_plot",'PLSDA'),
    definition=function(obj,dobj) {
        threshold=sort(unique(c(0,dobj$yhat[,1],1)))
        
        sn=numeric()
        sp=numeric()
        for (k in threshold) {
            pred=as.numeric(dobj$yhat[,1]>=k)
            tp=sum(pred==1 & dobj$design_matrix[,1]==1)
            fp=sum(pred==1 & dobj$design_matrix[,1]==-1)
            tn=sum(pred==0 & dobj$design_matrix[,1]==-1)
            fn=sum(pred==0 & dobj$design_matrix[,1]==1)
            
            sn=c(sn,tp/(tp+fn))
            sp=c(sp,tn/(tn+fp))
        }
        
        A=data.frame(Sensitivity=sn,Specificity=1-sp)
        
        A=A[order(A$Specificity,A$Sensitivity),]
        
        AUC=0
        # approximate as trapeziums
        for (k in seq(from=2,to=length(threshold),by=1)) {
            h=1-sp[k]-(1-sp[k-1])
            a=sn[k]
            b=sn[k-1]
            AUC=AUC+(h*((a+b)/2))
        }
        

        
        out = ggplot(data=A,aes_(x=~Specificity,y=~Sensitivity)) +
                geom_abline(intercept=0,slope=1,linetype="dashed", color = "black",size=1) +
                geom_line(colour='green4',size=1)+
                xlab('1-Specificity')+ylab('Sensitivity') +
                theme_Publication(base_size = 12)
        return(out)
    }
)

#################################################
#################################################

#' plsda_vip_plot class
#'
#' Plots the vip scores of a PLSDA model.
#'
#' @param threshold the VIP threshold to plot. Default = 1
#' @param level = the group label to plot VIP scores for
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export plsda_vip_plot
#' @include PLSDA_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre()+PLSDA(factor_name='Species')
#' M = model_apply(M,D)
#'
#' C = plsda_vip_plot(level='setosa')
#' chart_plot(C,M[2])
plsda_vip_plot = function(threshold=1,level,...) {
    out=struct::new_struct('plsda_vip_plot',
        threshold = threshold,
        level=level,
        ...)
    return(out)
}

.plsda_vip_plot<-setClass(
    "plsda_vip_plot",
    contains='chart',
    slots=c(
        # INPUTS
        threshold='numeric',
        level = 'character'
    ),
    prototype = list(name='PLSDA VIP score plot',
        description='Plots the VIP scores of a PLSDA model',
        type="scatter",
        libraries=c('pls','ggplot2'),
        .params=c('threshold','level'),
        
        threshold = 1
    )
    
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("plsda_vip_plot",'PLSDA'),
    definition=function(obj,dobj) {
        
        A=data.frame(vip=dobj$vip[[obj$level]])
        A$feature=rownames(dobj$loadings)
        
        out = ggplot(data=A,aes_(x=~feature,y=~vip)) +
            geom_hline(yintercept = obj$threshold,linetype="dashed", color = "black",size=1) +
                geom_point(colour='green4') +
            xlab('Feature')+ylab('VIP score') +
            theme_Publication(base_size = 12)+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                text = element_text(size=8))
        return(out)
    }
)


#################################################
#################################################

#' plsda_regcoeff_plot class
#'
#' Plots the regression coefficients of a PLSDA model.
#'
#' @param level the group label to plot regression coefficients for
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export plsda_regcoeff_plot
#' @include PLSDA_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre()+PLSDA(factor_name='Species')
#' M = model_apply(M,D)
#'
#' C = plsda_regcoeff_plot(level='setosa')
#' chart_plot(C,M[2])
plsda_regcoeff_plot = function(level,...) {
    out=struct::new_struct('plsda_regcoeff_plot',
        level=level,
        ...)
    return(out)
}

.plsda_regcoeff_plot<-setClass(
    "plsda_regcoeff_plot",
    contains='chart',
    slots=c(
        # INPUTS
        level = 'character'
    ),
    prototype = list(name='PLSDA regression coefficient plot',
        description='Plots the regression coefficient scores of a PLSDA model',
        type="scatter",
        libraries=c('pls','ggplot2'),
        .params=c('level')
    )
)
    
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("plsda_regcoeff_plot",'PLSDA'),
    definition=function(obj,dobj) {
        
        A=data.frame(rc=dobj$reg_coeff[[obj$level]])
        A$feature=rownames(dobj$loadings)
        
        out = ggplot(data=A,aes_(x=~feature,y=~rc)) +
            geom_point(colour='green4') +
            xlab('Feature')+ylab('PLS regression coefficient') +
            theme_Publication(base_size = 12)+ 
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                text = element_text(size=8))
        return(out)
    }
)











