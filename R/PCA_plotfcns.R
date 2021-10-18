#' @eval get_description('pca_correlation_plot')
#' @import struct
#' @export pca_correlation_plot
#' @include PCA_class.R
#' @examples
#' C = pca_correlation_plot()
pca_correlation_plot = function(components=c(1,2),...) {
    out=struct::new_struct('pca_correlation_plot',
        components=components,
        ...)
    return(out)
}


.pca_correlation_plot<-setClass(
    "pca_correlation_plot",
    contains='chart',
    slots=c(
        # INPUTS
        components='entity'
    ),
    prototype = list(name='PCA correlation plot',
        description=paste0(
            'A plot of the correlation between the variables/features and ',
            'the selected principal component scores. Features with high ',
            'correlation are well represented by the selected component(s)'),
        type="boxlot",
        .params=c('components'),
        
        components=entity(name='Components to plot',
            value=c(1,2),
            type='numeric',
            description='The Principal Components used to generate the plot.',
            max_length=2
        )
        
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pca_correlation_plot",'PCA'),
    definition=function(obj,dobj)
    {
        opt=param_list(obj)
        A=data.frame(x=output_value(dobj,'correlation')[,opt$components[1]],y=output_value(dobj,'correlation')[,opt$components[2]])
        dat <- circleFun(c(0,0),2,npoints = 50)
        
        out=ggplot(data=A,aes_(x=~x,y=~y)) +
            geom_point() +
            scale_colour_Publication() +
            theme_Publication(base_size = 12)+
            coord_fixed(xlim = c(-1,1),ylim=c(-1,1)) +
            
            geom_path(data=dat,aes_(x=~x,y=~y),inherit.aes = FALSE)
        
        return(out)
    }
)

#################################################
#################################################

#' @eval get_description('pca_scores_plot')
#' @import struct
#' @export pca_scores_plot
#' @include PCA_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre() + PCA()
#' M = model_apply(M,D)
#' C = pca_scores_plot(factor_name = 'Species')
#' chart_plot(C,M[2])
#'
pca_scores_plot = function(
    components=c(1,2),
    points_to_label='none',
    factor_name,
    ellipse='all',
    ellipse_type='norm',
    ellipse_confidence=0.95,
    label_filter=character(0),
    label_factor='rownames',
    label_size=3.88,
    ...) {
    out=struct::new_struct('pca_scores_plot',
        components=components,
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


.pca_scores_plot<-setClass(
    "pca_scores_plot",
    contains='chart',
    slots=c(
        # INPUTS
        components='entity',
        points_to_label='enum',
        factor_name='entity',
        ellipse='enum',
        ellipse_type='enum',
        ellipse_confidence='entity',
        label_filter='entity',
        label_factor='entity',
        label_size='entity'
    ),
    
    prototype = list(name='PCA scores plot',
        description='Plots a 2d scatter plot of the selected components',
        type="scatter",
        .params=c('components','points_to_label','factor_name','ellipse',
            'label_filter','label_factor','label_size','ellipse_type',
            'ellipse_confidence'),
        
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
        factor_name=ents$factor_name,
        ellipse=enum(
            name = 'Plot ellipses',
            description=c(
                "all" = paste0('Ellipses are plotted for all groups and all samples.'),
                "group" = 'Ellipses are plotted for all groups.',
                "none" = 'Ellipses are not included on the plot.',
                "sample" = 'An ellipse is plotted for all samples (ignoring group)'),
            allowed=c('all','group','none','sample'),
            value='all'
        ),
        
        ellipse_type=enum(
            name='Type of ellipse',
            description=c(
                'norm' = paste0('Multivariate normal (p = 0.95)'),
                't' = paste0('Multivariate t (p = 0.95)')
                ),
            value='norm',
            type='character',
            max_length = 1,
            allowed=c('norm','t')
        ),
        
        ellipse_confidence=entity(
            name='Ellipse confidence level',
            description='The confidence level for plotting ellipses.',
            value=0.95,
            type='numeric',
            max_length = 1
        ),
        
        label_filter=entity(
            name='Label filter',
            value=character(0),
            type='character',
            description=paste0(
                'Labels are only plotted for the named groups. If ',
                'zero-length then all groups are included.'
            )
        ),
        label_factor=entity(name='Factor for labels',
            description=paste0('The column name of sample_meta to use for ',
                'labelling samples on the plot. "rownames" will use the row ',
                'names from sample_meta.'),
            type='character',
            value='rownames',
            max_length=1),
        label_size=entity(name='Text size of labels',
            description='The text size of labels. Note this is not in Font Units.',
            type='numeric',
            value=3.88,
            max_length=1)
    )
)



#' @importFrom sp point.in.polygon
#' @import ggplot2
#' @importFrom scales squish
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pca_scores_plot",'PCA'),
    definition=function(obj,dobj)
    {
        
        if (obj$points_to_label=='outliers' & !(obj$ellipse %in% c('all','sample'))) {
            warning('Outliers are only labelled when plotting the sample ellipse')
        }
        opt=param_list(obj)
        scores=output_value(dobj,'scores')$data
        pvar = (colSums(scores*scores)/output_value(dobj,'ssx'))*100 # percent variance
        pvar = round(pvar,digits = 2) # round to 2 decimal places
        
        if (length(obj$factor_name)==1) {
            shapes = 19 # filled circles for all samples
        } else {
            shapes = factor(dobj$scores$sample_meta[[obj$factor_name[2]]])
        }
        
        if (obj$label_factor=='rownames') {
            slabels = rownames(dobj$scores$sample_meta)
        } else {
            slabels = dobj$scores$sample_meta[[obj$label_factor]]
        }
        opt$factor_name=opt$factor_name[[1]] # only use the first factor from now on
        
        x=scores[,opt$components[1]]
        y=scores[,opt$components[2]]
        xlabel=paste("PC",opt$components[[1]],' (',sprintf("%.1f",pvar[opt$components[[1]]]),'%)',sep='')
        ylabel=paste("PC",opt$components[[2]],' (',sprintf("%.1f",pvar[opt$components[[2]]]),'%)',sep='')
        
        # get the factor from meta data
        opt$groups=dobj$scores$sample_meta[[opt$factor_name]]
        
        # add a space to the front of the labels to offset them from the points, because nudge_x is in data units
        for (i in 1:length(slabels))
        {
            slabels[i]=paste0('  ',slabels[i], '  ')
        }
        
        # filter by label_filter list if provided
        if (length(obj$label_filter)>0) {
            out=!(as.character(opt$groups) %in% obj$label_filter)
            slabels[out]=''
        }
        
        if (is(opt$groups,'factor') | is(opt$groups,'character')) {
            plotClass= structToolbox:::createClassAndColors(opt$groups)
            opt$groups=plotClass$class
        }
        
        # build the plot
        A <- data.frame (group=opt$groups,x=x, y=y,slabels=slabels)
        
        out = ggplot()
        
        # add invisible sample points for ellipse
        out = out+geom_point(data=A,aes_string(x='x',y='y'),alpha=0,show.legend=FALSE)
        
        
        if (length(obj$factor_name)==2) {
            out=out+geom_point(data=A, aes_(x=~x,y=~y,colour=~group,shape=~shapes))
        }   else {
            out=out+geom_point(data=A, aes_(x=~x,y=~y,colour=~group))
        }
        
        
        out=out+
            
            geom_point(na.rm=TRUE) +
            xlab(xlabel) +
            ylab(ylabel) +
            ggtitle('PCA Scores', subtitle=NULL)
        
        if (length(obj$factor_name)==2) {
            out=out+labs(shape=obj$factor_name[[2]],colour=obj$factor_name[[1]])
        } else {
            out=out+labs(shape=obj$factor_name[[1]])
        }
        
        if (obj$ellipse %in% c('all','group')) {
            out = out +stat_ellipse(data=A, aes_(x=~x,y=~y,colour=~group),type=obj$ellipse_type,
                level=obj$ellipse_confidence) # ellipse for individual groups
        }
        
        if (is(opt$groups,'factor')) { # if a factor then plot by group using the colours from pmp package
            out=out+scale_colour_manual(values=plotClass$manual_colors,
                name=opt$factor_name)
        }else {# assume continuous and use the default colour gradient
            out=out+scale_colour_viridis_c(limits=quantile(opt$groups,
                c(0.05,0.95),na.rm = TRUE),oob=squish,name=opt$factor_name)
        }
        out=out+structToolbox:::theme_Publication(base_size = 12)
        # add ellipse for all samples (ignoring group)
        if (obj$ellipse %in% c('all','sample')) {
            out=out+stat_ellipse(type=obj$ellipse_type,mapping=aes(x=x,y=y),
                colour="#C0C0C0",linetype='dashed',data=A,
                level=obj$ellipse_confidence)
        }
        
        if (obj$ellipse %in% c('all','sample')) { # only do this if we plotted the sample ellipse
            # identify samples outside the ellipse
            build=ggplot_build(out)$data
            points=build[[1]]
            ell=build[[length(build)]]
            # outlier for DatasetExperiment ellipse
            points$in.ell=as.logical(sp::point.in.polygon(points$x,points$y,ell$x,ell$y))
            
            # label outliers if
            if (opt$points_to_label=='outliers')
            {
                if (!all(points$in.ell))
                {
                    temp=subset(points,!points$in.ell)
                    temp$group=opt$groups[!points$in.ell]
                    temp$label=slabels[!points$in.ell]
                    out=out+geom_text(data=temp,aes_(x=~x,y=~y,label=~label,colour=~group),size=obj$label_size,vjust="inward",hjust="inward",show.legend=FALSE)
                    
                }
            }
            # add a list of outliers to the plot object
            out$outliers=trimws(slabels[!points$in.ell])
        }
        
        # label all points if requested
        if (opt$points_to_label=='all') {
            out=out+geom_text(data=A,aes_string(x='x',y='y',colour='group',label='slabels'),vjust="inward",hjust="inward",show.legend=FALSE)
        }
        
        return(out)
    }
)

#################################################################
#################################################################

#' @eval get_description('pca_biplot')
#' @import struct
#' @export pca_biplot
#' @include PCA_class.R
#' @examples
#' C = pca_biplot(factor_name='Species')
pca_biplot = function(
    components=c(1,2),
    points_to_label='none',
    factor_name,
    scale_factor=0.95,
    style='points',
    label_features=FALSE,
    ...) {
    out=struct::new_struct('pca_biplot',
        components=components,
        points_to_label=points_to_label,
        factor_name=factor_name,
        scale_factor=scale_factor,
        style=style,
        label_features=label_features,
        ...)
    return(out)
}


.pca_biplot<-setClass(
    "pca_biplot",
    contains=c('chart'),
    slots=c(
        # INPUTS
        components='entity',
        points_to_label='entity',
        factor_name='entity',
        scale_factor='entity',
        style='enum',
        label_features='entity'
    ),
    prototype = list(name='PCA biplot',
        description=paste0('A scatter plot of the selected principal ',
            'component scores overlaid with the corresponding principal ',
            'component loadings.'),
        type="boxlot",
        .params=c('components','points_to_label','factor_name','scale_factor','style','label_features'),
        
        components=entity(name='Components to plot',
            value=c(1,2),
            type='numeric',
            description='The principal components used to generate the plot.',
            max_length=2
        ),
        points_to_label=enum(name='points_to_label',
            value='none',
            type='character',
            description=c(
                "none" = 'No samples are labelled on the plot.',
                "all" = 'All samples are labelled on the plot.',
                "outliers" = 'Potential outliers are labelled on the plot.'),
            allowed=c('none','all','outliers')
        ),
        factor_name=ents$factor_name,
        scale_factor=entity(name='Loadings scale factor',
            value=0.95,
            type='numeric',
            description='The scaling factor applied to the loadings.'
        ),
        style=enum(name='Plot style',
            value='points',
            type='character',
            description=c(
                "points" = 'Loadings and scores are plotted as a scatter plot.',
                'arrows' = 'The loadings are plotted as arrow vectors.'),
            allowed=c('points','arrows')
        ),
        label_features=entity(name='Add feature labels',
            value=FALSE,
            type='logical',
            description=c(
                "TRUE" = 'Features are labelled.',
                "FALSE" = "Features are not labelled.")
        )
    )
    
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pca_biplot",'PCA'),
    definition=function(obj,dobj)
    {
        opt=param_list(obj)
        Ts=output_value(dobj,'scores')$data
        pvar=(colSums(Ts*Ts)/output_value(dobj,'ssx'))*100
        pvar=round(pvar,digits = 1)
        xlabel=paste("PC",opt$components[[1]],' (',sprintf("%.1f",pvar[opt$components[[1]]]),'%)',sep='')
        ylabel=paste("PC",opt$components[[2]],' (',sprintf("%.1f",pvar[opt$components[[2]]]),'%)',sep='')
        
        P=output_value(dobj,'loadings')
        Ev=output_value(dobj,'eigenvalues')
        
        # eigenvalues were square rooted when training PCA
        Ev=Ev[,1]
        Ev=Ev^2
        
        ## unscale the scores
        #ev are the norms of scores
        Ts=as.matrix(Ts) %*% diag(1/Ev) # these are normalised scores
        
        # scale scores and loadings by alpha
        Ts=Ts %*% diag(Ev^(1-opt$scale_factor))
        P=as.matrix(P) %*% diag(Ev^(opt$scale_factor))
        
        # additionally scale the loadings
        sf=min(max(abs(Ts[,opt$components[1]]))/max(abs(P[,opt$components[1]])),
            max(abs(Ts[,opt$components[2]]))/max(abs(P[,opt$components[2]])))
        Ts=as.data.frame(Ts)
        
        rownames(Ts)=rownames(dobj$scores) # fix dimnames for SE object
        colnames(Ts)=colnames(dobj$scores)
        dobj$scores$data=as.data.frame(Ts) # nb object not returned, so only temporary scaling
        
        # plot
        A=data.frame("x"=P[,opt$components[1]]*sf*0.8,"y"=P[,opt$components[2]]*sf*0.8)
        C=pca_scores_plot(points_to_label=obj$points_to_label,components=obj$components,factor_name=obj$factor_name)
        out=chart_plot(C,dobj)
        
        if (opt$style=='points')
        {
            out=out+
                geom_point(data=A,inherit.aes = FALSE,color='black',mapping = aes_(x=~x,y=~y))
        }
        if (opt$style=='arrows')
        {
            out=out+
                
                geom_segment(data=A,inherit.aes = FALSE,color='black',mapping = aes_(x=~0,y=~0,xend=~x,yend=~y),arrow=arrow(length=unit(8,'points')))
            
        }
        out=out+ggtitle('PCA biplot', subtitle=NULL) +
            xlab(xlabel) + ylab(ylabel)
        
        #label features if requested
        if (opt$label_features)
        {
            vlabels=rownames(dobj$loadings)
            for (i in 1:length(vlabels))
            {
                vlabels[i]=paste0('  ',vlabels[i], '  ')
            }
            A$vlabels=vlabels
            out=out+
                
                geom_text(data=A,aes_(x=~x,y=~y,label=~vlabels),vjust="inward",hjust="inward",inherit.aes = FALSE)
            
        }
        return(out)
    }
)



##################################################################
##################################################################

#' @eval get_description('pca_loadings_plot')
#' @import struct
#' @export pca_loadings_plot
#' @include PCA_class.R
#' @examples
#' C = pca_loadings_plot()
pca_loadings_plot = function(components=c(1,2),style='points',label_features=NULL,...) {
    out=struct::new_struct('pca_loadings_plot',
        components=components,
        style=style,
        label_features=label_features,
        ...)
    return(out)
}


.pca_loadings_plot<-setClass(
    "pca_loadings_plot",
    contains='chart',
    slots=c(
        # INPUTS
        components='entity',
        style='enum',
        label_features='entity'
    ),
    prototype = list(name='PCA loadings plot',
        description=paste0('A barchart (one component) or scatter plot ',
            '(two components) of the selected principal component loadings.'),
        type="boxlot",
        .params=c('components','style','label_features'),
        
        components=entity(name='Components to plot',
            value=c(1,2),
            type='numeric',
            description='The principal components used to generate the plot.',
            max_length=2
        ),
        style=enum(name='Plot style',
            value='points',
            type='character',
            description=c(
                "points" = 'Loadings and scores are plotted as a scatter plot.',
                'arrows' = 'The loadings are plotted as arrow vectors.'),
            allowed=c('points','arrows')
        ),
        label_features=entity(name='Feature labels',
            value=NULL,
            type=c('character',"NULL"),
            description=c(
                'character()'='A vector of labels for the features',
                'NULL' = 'No labels',
                'row.names' = 'Labels will be extracted from the column names of the data matrix.'
            )
        )
    )
    
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pca_loadings_plot",'PCA'),
    definition=function(obj,dobj)
    {
        opt=param_list(obj)
        
        P=output_value(dobj,'loadings')
        # 1D plot
        if (length(opt$components)==1) {
            A=data.frame("x"=1:nrow(P),"y"=P[,opt$components[1]])
            out=ggplot(data=A,aes_(x=~x,y=~y)) +
                geom_line() +
                ggtitle('PCA Loadings', subtitle=NULL) +
                xlab('Feature') +
                ylab('Loading') +
                scale_colour_Publication() +
                theme_Publication(base_size = 12)
        }
        # 2D plot
        if (length(opt$components)==2) {
            A=data.frame("x"=P[,opt$components[1]],"y"=P[,opt$components[2]])
            out=ggplot(data=A,aes_(x=~x,y=~y)) +
                geom_point() +
                ggtitle('PCA Loadings', subtitle=NULL) +
                xlab(paste0('Component ',opt$components[1])) +
                ylab(paste0('Component ',opt$components[2])) +
                scale_colour_Publication() +
                theme_Publication(base_size = 12)
            
            if (!is.null(obj$label_features)) {
                
                if (obj$label_features=='rownames') {
                    vlabels=rownames(dobj$loadings)
                } else {
                    vlabels=obj$label_features
                }
                
                for (i in 1:length(vlabels)) {
                    vlabels[i]=paste0('  ',vlabels[i], '  ')
                }
                A$vlabels=vlabels
                out=out+geom_text(data=A,aes_(x=~x,y=~y,label=~vlabels),
                    vjust="inward",hjust="inward",inherit.aes = FALSE)
            }
        }
        if (length(opt$components)>2) {
            stop('can only plot loadings for 1 or 2 components at a time')
        }
        
        return(out)
        
    }
)


#' @eval get_description('pca_scree_plot')
#' @import struct
#' @return struct object
#' @export pca_scree_plot
#' @include PCA_class.R
#' @examples
#' C = pca_scree_plot()
pca_scree_plot = function(...) {
    out=struct::new_struct('pca_scree_plot',...)
    return(out)
}


.pca_scree_plot<-setClass(
    "pca_scree_plot",
    contains=c('chart'),
    prototype = list(name='Scree plot',
        description=paste0('A plot of the percent variance and cumulative ',
            'percent variance for the components of a PCA model. '),
        type="line",
        ontology="STATO:0000386"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pca_scree_plot",'PCA'),
    definition=function(obj,dobj)
    {
        ## percent variance
        scores=output_value(dobj,'scores')$data
        pvar=(colSums(scores*scores)/output_value(dobj,'ssx'))*100
        A=data.frame("x"=1:length(pvar),"y"=c(pvar,cumsum(pvar)),"Variance"=as.factor(c(rep('Single component',length(pvar)),rep('Cumulative',length(pvar)))))
        labels=round(A$y,digits = 1)
        labels=format(labels,1)
        
        out=ggplot(data=A, aes_(x=~x,y=~y,color=~Variance)) +
            geom_line() +
            geom_point() +
            geom_text(aes_(label=~labels),color='black',vjust=0,nudge_y = 5) +
            
            ggtitle('Scree Plot', subtitle=NULL) +
            xlab('Component') +
            ylab('Variance (%)') +
            scale_colour_Publication() +
            scale_x_continuous(breaks=0:length(pvar)) +
            guides(color=guide_legend("")) +
            theme_Publication(base_size = 12)
        return(out)
    }
)

#' @eval get_description('pca_dstat_plot')
#' @import struct
#' @export pca_dstat_plot
#' @include PCA_class.R
#' @examples
#' C = pca_dstat_plot()
pca_dstat_plot = function(number_components=2,alpha=0.05,...) {
    out=struct::new_struct('pca_dstat_plot',
        number_components=number_components,
        alpha=alpha,
        ...)
    return(out)
}


.pca_dstat_plot<-setClass(
    "pca_dstat_plot",
    contains=c('chart'),
    slots=c(number_components='entity',
        alpha='entity'),
    prototype = list(name='d-statistic plot',
        description=paste0('A bar chart of the d-statistics for samples in ',
            'the input PCA model. Samples above the indicated threshold are ',
            'considered to be outlying.'),
        type="bar",
        .params=c('number_components','alpha'),
        ontology='STATO:0000132',
        number_components=entity(value = 2,
            name = 'Number of principal components',
            description = 'The number of principal components to use.',
            type='numeric'),
        alpha=entity(value=0.95,
            name='Outlier threshold',
            description='A confidence threshold for rejecting samples based on the d-statistic',
            type='numeric')
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pca_dstat_plot",'PCA'),
    definition=function(obj,dobj)
    {
        opt=param_list(obj)
        a=param_value(obj,'number_components')
        scores=output_value(dobj,'scores')$data
        I=nrow(scores)             # number of samples
        sample_names=rownames(scores)
        scores=scores[,1:a]
        scores=as.matrix(scores)
        covT=t(scores) %*% scores  # covariance matrix
        covT=solve(covT/(I-1)) # inverse
        H=numeric(length = I)
        for (i in 1:I)
        {
            H[i]=scores[i,,drop=FALSE]%*%covT%*%t(scores[i,,drop=FALSE])
        } #leverage value
        
        # threshold at alpha
        F=qf(p = opt$alpha,df1 = a,df2=I-a)
        sf=(a*(I-1)*(I+1))/(I*(I-a))
        threshold=sf*F
        # ggplot
        df=data.frame(x=sample_names,y=H)
        
        out=ggplot(data=df, aes_(x=~x,y=~y)) +
            
            geom_bar(stat="identity") +
            geom_hline(yintercept=threshold, linetype='dashed', color='grey') +
            ggtitle('d-statistic', subtitle=paste0('Number of components = ',a)) +
            xlab('Sample') +
            ylab('d-statistic') +
            scale_x_discrete (limits = sample_names,breaks=NULL) +
            scale_colour_Publication() +
            theme_Publication(base_size = 12)
        return(out)
    }
)




circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}

