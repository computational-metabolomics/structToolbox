#' @eval get_description('kfoldxcv_grid')
#' @export kfoldxcv_grid
#' @include kfold_xval_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' I = kfold_xval(factor_name='Species') *
#'     (mean_centre() + PLSDA(factor_name='Species'))
#' I = run(I,D,balanced_accuracy())
#'
#' C = kfoldxcv_grid(factor_name='Species',level='setosa')
#' chart_plot(C,I)
#'
kfoldxcv_grid = function(factor_name,level,...) {
    out=struct::new_struct('kfoldxcv_grid',
        factor_name=factor_name,
        level=level,
        ...)
    return(out)
}


.kfoldxcv_grid<-setClass(
    "kfoldxcv_grid",
    contains='chart',
    slots=c(
        # INPUTS
        factor_name='entity',
        level='entity'
    ),
    prototype = list(name='k-fold cross validation plot',
        description=paste0('A graphic for visualising the true class and the ',
        'predicted class of samples in all groups for all cross-validation ',
        'folds. '),
        type="grid",
        .params=c('factor_name','level'),
        
        factor_name=ents$factor_name,
        
        level=entity(name='The level of the factor to plot',
            value='level_1',
            type='character',
            description='The level/group to plot.'
        )
        
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("kfoldxcv_grid",'kfold_xval'),
    definition=function(obj,dobj)
    {
        # get options
        copt=param_list(obj)
        dopt=param_list(dobj)
        X=output_value(dobj,'results')
        L=levels(as.factor(X$actual))
        plotClass= createClassAndColors(X$actual)
        plotClass$manual_colors=plotClass$manual_colors[1:nlevels(plotClass$class)]
        names(plotClass$manual_colors)=levels(plotClass$class)
        X$actual=plotClass$class
        
        p=list()
        i=which(L==obj$level)
        
        if (length(i)==0) {
            stop('no matching level for this factor')
        }
        
        # get data
        X=output_value(dobj,'results')
        X$actual=plotClass$class
        
        # reduce to level i for split factor
        X=X[X$actual %in% L[i],,drop=FALSE]
        
        X2=X[,-2]
        X=X[,-1]
        X$Set='Predicted'
        X2$Set='True'
        colnames(X)[1]='Group'
        colnames(X2)[1]='Group'
        X=rbind(X,X2)
        
        uS=unique(as.character(X$sampleid))
        uL=L
        
        # to make legend include all levels
        dummy=X[1:length(plotClass$manual_colors),]
        dummy$Group=names(plotClass$manual_colors)
        dummy$in.test=FALSE
        dummy$fold=NA
        X=rbind(X,dummy)
        
        # for test set boxes
        X2=X
        X2$sampleid[!X$in.test]=NA
        X2$fold[!X$in.test]=NA
        
        p=ggplot(data=X,aes(x=.data[["sampleid"]],y=.data[["fold"]],fill=.data[["Group"]])) +
            geom_tile(
                colour = "grey50",na.rm = TRUE) +
            #geom_point(data=te,aes_(x=~sampleid,y=~fold),shape=20,size=2) +
            geom_tile(
                data=X2,
                aes(
                    x=.data[['sampleid']],
                    y=.data[['fold']],
                    fill=.data[['Group']],
                    colour = .data[['in.test']]),
                na.rm = TRUE) +
            scale_x_discrete(limits=uS) +
            theme_Publication(base_size = 12) +
            coord_equal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
            scale_fill_manual(values=plotClass$manual_colors,drop=FALSE,name=copt$factor_name) +
            scale_colour_manual(
                values=c('TRUE'='black','FALSE'='grey'),
                breaks = c(FALSE, TRUE),
                labels = c("Training set", "Test set"),
                name = "Data")+
            facet_grid(rows=vars(X$Set),drop = FALSE)+
            guides(
                colour = guide_legend(
                    override.aes = list(fill = "white", size = 1.5)))
        
        return(p)
        
    }
)



###################

#' @eval get_description('kfoldxcv_metric')
#' @import struct
#' @export kfoldxcv_metric
#' @include kfold_xval_class.R
#' @examples
#' C = kfoldxcv_metric()
kfoldxcv_metric = function(...) {
    out=struct::new_struct('kfoldxcv_metric',...)
    return(out)
}


.kfoldxcv_metric<-setClass(
    "kfoldxcv_metric",
    contains='chart',
    prototype = list(name='kfoldxcv metric plot',
        description=paste0('A boxplot of the performance metric computed ',
        'for each fold of a k-fold cross-validation.'),
        type="boxplot"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("kfoldxcv_metric",'kfold_xval'),
    definition=function(obj,dobj)
    {
        # get options
        dopt=param_list(dobj)
        
        # get data
        X=data.frame('Metric'=output_value(dobj,'metric.train'),'Set'='Training')
        X2=data.frame('Metric'=output_value(dobj,'metric.test'),'Set'='Test')
        X=rbind(X,X2)
        X$Set=as.factor(X$Set)
        plotClass= createClassAndColors(X$Set)
        
        p=ggplot(data=X,
                 aes(y=.data[['Metric']],x=.data[['Set']],colour=.data[['Set']])) +
            geom_boxplot() +
            theme_Publication(base_size = 12) +
            scale_colour_manual(values=plotClass$manual_colors,name='Crossvalidation set') +
            xlab('Crossvalidation set') + theme(legend.position="none")
        
        
        
        return(p)
        
    }
)




