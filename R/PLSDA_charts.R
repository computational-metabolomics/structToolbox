#################################################
#################################################

#' @eval get_description('pls_scores_plot')
#' @export pls_scores_plot
#' @include PLSDA_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre()+PLSDA(factor_name='Species')
#' M = model_apply(M,D)
#'
#' C = pls_scores_plot(factor_name='Species')
#' chart_plot(C,M[2])
pls_scores_plot = function(
    xcol='LV1',
    ycol='LV2',
    points_to_label='none',
    factor_name,
    ellipse='all',
    ellipse_type='norm',
    ellipse_confidence=0.95,
    label_filter=character(0),
    label_factor='rownames',
    label_size=3.88,
    ...) {
    out=struct::new_struct('pls_scores_plot',
        xcol=xcol,
        ycol=ycol,
        points_to_label=points_to_label,
        factor_name=factor_name,
        ellipse=ellipse,
        label_filter=label_filter,
        label_factor=label_factor,
        label_size=label_size,
        ellipse_type=ellipse_type,
        ellipse_confidence=ellipse_confidence,
        ...)
    return(out)
}

.pls_scores_plot<-setClass(
    "pls_scores_plot",
    contains='scatter_chart',
    prototype = list(name='PLSDA scores plot',
        description='A scatter plot of the selected PLSDA scores.'
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pls_scores_plot",'PLSR'),
    definition=function(obj,dobj)
    {
        # copy inputs to scatter chart
        C = scatter_chart()
        param_list(C) = param_list(obj)
        
        # plot
        g = chart_plot(C,dobj$scores) +ggtitle('PLS scores')

        return(g)

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
plsda_predicted_plot = function(factor_name,style='boxplot',
    ycol=1,
    ...) {
    out=struct::new_struct('plsda_predicted_plot',
        factor_name=factor_name,
        style=style,
        ycol=ycol,
        ...)
    return(out)
}

.plsda_predicted_plot<-setClass(
    "plsda_predicted_plot",
    contains='chart',
    slots=c(
        # INPUTS
        factor_name='entity',
        style='entity',
        ycol='entity'
    ),
    prototype = list(name='PLSDA predicted plot',
        description='A plot of the regression coefficients from a PLSDA model.',
        type="boxplot",
        libraries=c('pls','ggplot2'),
        .params=c('factor_name','style','ycol'),
        
        factor_name=ents$factor_name,
        
        style=enum(name='Plot style',
            description=c(
                'boxplot' = 'A boxplot',
                'violin' = 'A violin plot',
                'density' = 'A density plot'),
            type='character',
            value='boxplot',
            allowed=c('boxplot','violin','density')
        ),
        ycol=entity(
            name='Y column to plot',
            description = 'The column of the Y block to be plotted.',
            type=c('character','numeric','integer'),
            value=1,
            max_length=1
        )
    )
    
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("plsda_predicted_plot",'PLSDA'),
    definition=function(obj,dobj) {
        A=data.frame(Class=dobj$y[[obj$factor_name]],Predicted=dobj$yhat[,obj$ycol])
        
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
plsda_roc_plot = function(factor_name,ycol=1,...) {
    out=struct::new_struct('plsda_roc_plot',
        factor_name=factor_name,
        ycol=ycol,
        ...)
    return(out)
}

.plsda_roc_plot<-setClass(
    "plsda_roc_plot",
    contains=c('chart'),
    slots=c(
        # INPUTS
        factor_name='entity',
        ycol='entity'
    ),
    prototype = list(name='PLSDA ROC plot',
        description=paste0('A Receiver Operator Characteristic (ROC) plot for ',
        'PLSDA models computed by adjusting the threshold for assigning group ',
        'labels from PLS predictions.'),
        type="roc",
        libraries=c('pls','ggplot2'),
        .params=c('factor_name','ycol'),
        ontology='STATO:0000274',
        factor_name=ents$factor_name,
        ycol=entity(
            name='Y column to plot',
            description = 'The column of the Y block to be plotted.',
            type=c('character','numeric','integer'),
            value=1,
            max_length=1
        )
    )
    
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("plsda_roc_plot",'PLSDA'),
    definition=function(obj,dobj) {
        threshold=sort(unique(c(0,dobj$yhat[,obj$ycol],1)))
        
        sn=numeric()
        sp=numeric()
        for (k in threshold) {
            pred=as.numeric(dobj$yhat[,obj$ycol]>=k)
            tp=sum(pred==1 & dobj$design_matrix[,obj$ycol]==1)
            fp=sum(pred==1 & dobj$design_matrix[,obj$ycol]==-1)
            tn=sum(pred==0 & dobj$design_matrix[,obj$ycol]==-1)
            fn=sum(pred==0 & dobj$design_matrix[,obj$ycol]==1)
            
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
        ontology='STATO:0000580'
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
#' @eval get_description('pls_regcoeff_plot')
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











