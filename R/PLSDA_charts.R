#################################################
#################################################

#' @eval get_description('plsda_scores_plot')
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
        description='A scatter plot of the selected PLSDA scores.',
        type="scatter",
        libraries=c('pls','ggplot2'),
        .params=c('components','points_to_label','factor_name'),
        
        components=entity(name='Components to plot',
            value=c(1,2),
            type='numeric',
            description=paste0('The components selected for plotting.'),
            max_length=2
        ),
        
        points_to_label=enum(name='Points to label',
            value='none',
            type='character',
            description=c(
                'none' = 'No samples labels are displayed.', 
                "all" = 'The labels for all samples are displayed.', 
                "outliers" = 'Labels for for potential outlier samples are displayed.'
            ),
            allowed=c('none','all','outliers')
        ),
        factor_name=ents$factor_name
    )
    
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("plsda_scores_plot",'PLSR'),
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

#' @eval get_description('plsda_predicted_plot')
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
        description='A plot of the regression coefficients from a PLSDA model.',
        type="boxplot",
        libraries=c('pls','ggplot2'),
        .params=c('factor_name','style'),
        
        factor_name=ents$factor_name,
        
        style=enum(name='Plot style',
            description=c(
                'boxplot' = 'A boxplot',
                'violin' = 'A violin plot',
                'density' = 'A density plot'),
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

#' @eval get_description('plsda_roc_plot')
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
    contains=c('chart','stato'),
    slots=c(
        # INPUTS
        factor_name='entity'
    ),
    prototype = list(name='PLSDA ROC plot',
        description=paste0('A Receiver Operator Characteristic (ROC) plot for ',
        'PLSDA models computed by adjusting the threshold for assigning group ',
        'labels from PLS predictions.'),
        type="roc",
        libraries=c('pls','ggplot2'),
        .params=c('factor_name'),
        stato_id='STATO:0000274',
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

#' @eval get_description('pls_vip_plot')
#' @export pls_vip_plot
#' @include PLSR_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre()+PLSDA(factor_name='Species')
#' M = model_apply(M,D)
#'
#' C = pls_vip_plot(ycol='setosa')
#' chart_plot(C,M[2])
pls_vip_plot = function(threshold=1,ycol=1,...) {
    out=struct::new_struct('pls_vip_plot',
        threshold = threshold,
        ycol=ycol,
        ...)
    return(out)
}

.pls_vip_plot<-setClass(
    "pls_vip_plot",
    contains=c('chart','stato'),
    slots=c(
        # INPUTS
        threshold='entity',
        ycol = 'entity'
    ),
    prototype = list(name='PLSDA VIP plot',
        description='A plot of the Variable Importance for Projection (VIP) scores for a PLSDA model.',
        type="scatter",
        libraries=c('pls','ggplot2'),
        .params=c('threshold','ycol'),
        
        threshold = entity(
            name = 'VIP threshold',
            description = 'The threshold for indicating significant features.',
            type=c('numeric','integer'),
            value=1,
            max_length=1
        ),
        ycol=entity(
            name='Y column to plot',
            description = 'The column of the Y block to be plotted.',
            type=c('character','numeric','integer'),
            value=1,
            max_length=1
        ),
        stato_id='STATO:0000580'
    )
    
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pls_vip_plot",'PLSR'),
    definition=function(obj,dobj) {
        
        A=data.frame(vip=dobj$vip[[obj$ycol]])
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

#' pls_regcoeff_plot class
#'
#' Plots the regression coefficients of a PLSDA model.
#'
#' @param level the group label to plot regression coefficients for
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export pls_regcoeff_plot
#' @include PLSR_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre()+PLSDA(factor_name='Species')
#' M = model_apply(M,D)
#'
#' C = pls_regcoeff_plot(ycol='setosa')
#' chart_plot(C,M[2])
pls_regcoeff_plot = function(ycol=1,...) {
    out=struct::new_struct('pls_regcoeff_plot',
        ycol=ycol,
        ...)
    return(out)
}

.pls_regcoeff_plot<-setClass(
    "pls_regcoeff_plot",
    contains='chart',
    slots=c(
        # INPUTS
        ycol = 'entity'
    ),
    prototype = list(name='PLSDA regression coefficient plot',
        description='Plots the regression coefficient scores of a PLSDA model',
        type="scatter",
        libraries=c('pls','ggplot2'),
        .params=c('ycol'),
        ycol=entity(name= ' Y - column',
            description = "The Y column to plot",
            value=1,
            type=c('character','numeric','integer'),
            max_length=1)
    )
)
    
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pls_regcoeff_plot",'PLSR'),
    definition=function(obj,dobj) {
        
        A=data.frame(rc=dobj$reg_coeff[[obj$ycol]])
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











