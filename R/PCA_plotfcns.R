#' pca_correlation_plot class
#'
#' plots the correlation between features and selected components.
#'
#' @import struct
#' @export pca_correlation_plot
#' @include PCA_class.R
#' @examples
#' C = pca_correlation_plot()
pca_correlation_plot = function(...) {
    out=.pca_correlation_plot()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.pca_correlation_plot<-setClass(
    "pca_correlation_plot",
    contains='chart',
    slots=c(
        # INPUTS
        params_components='entity'
    ),
    prototype = list(name='Feature boxplot',
        description='plots a boxplot of a chosen feature for each group of a DatasetExperiment.',
        type="boxlot",
        params_components=entity(name='Components to plot',
            value=c(1,2),
            type='numeric',
            description='the components to be plotted e.g. c(1,2) plots component 1 on the x axis and component 2 on the y axis.',
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

#' pca_scores_plot class
#'
#' 2d scatter plot of princpal component scores.
#'
#' @import struct
#' @export pca_scores_plot
#' @include PCA_class.R
#' @examples
#' C = pca_scores_plot()
pca_scores_plot = function(...) {
    out=.pca_scores_plot()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.pca_scores_plot<-setClass(
    "pca_scores_plot",
    contains='chart',
    slots=c(
        # INPUTS
        params_components='entity',
        params_points_to_label='enum',
        params_factor_name='entity',
        params_ellipse='enum',
        params_label_filter='entity',
        params_groups='ANY', # will be deprecated
        params_label_factor='entity',
        params_label_size='entity'
    ),

    prototype = list(name='PCA scores plot',
        description='Plots a 2d scatter plot of the selected components',
        type="scatter",

        params_components=entity(name='Components to plot',
            value=c(1,2),
            type='numeric',
            description='the components to be plotted e.g. c(1,2) plots component 1 on the x axis and component 2 on the y axis.',
            max_length=2
        ),

        params_points_to_label=enum(name='points_to_label',
            value='none',
            type='character',
            description='("none"), "all", or "outliers" will be labelled on the plot.',
            list=c('none','all','outliers')
        ),
        params_factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The column name of sample meta to use for plotting. A second column can be included to plot using symbols.',
            max_length=2
        ),
        params_ellipse=enum(name = 'Plot ellipses',description=c(
            '"all" will plot all ellipses',
            '"group" will only plot group ellipses',
            '"none" will not plot any ellipses',
            '"sample" will plot ellipse for all samples (ignoring group)'),
            list=c('all','group','none','sample'),
            value='all'),
        params_label_filter=entity(name='Label filter',
            value=character(0),
            type='character',
            description='Only include the param.group labels included in params_label_filter. If zero length then all labels will be included.'
        ),
        params_label_factor=entity(names='Factor for labels',
            description='The column name of sample_meta to use as labels. "rownames" will use the row names from sample_meta.',
            type='character',
            value='rownames'),
        params_label_size=entity(names='Text size of labels',
            description='The text size of labels. Note this is not in Font Units. Default 3.88.',
            type='numeric',
            value=3.88)
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
            out=!(opt$groups %in% obj$label_filter)
            slabels[out]=''
        }

        if (is(opt$groups,'factor')) {
            plotClass= createClassAndColors(opt$groups)
            opt$groups=plotClass$class
        }

        # build the plot
        A <- data.frame (group=opt$groups,x=x, y=y)

        if (length(obj$factor_name)==2) {
            out=ggplot (data=A, aes_(x=~x,y=~y,colour=~group,label=~slabels,shape=~shapes))
        }   else {
            out=ggplot (data=A, aes_(x=~x,y=~y,colour=~group,label=~slabels))
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
            out = out +stat_ellipse(type='norm') # ellipse for individual groups
        }

        if (is(opt$groups,'factor')) { # if a factor then plot by group using the colours from pmp package
            out=out+scale_colour_manual(values=plotClass$manual_colors,name=opt$factor_name)
        }
        else {# assume continuous and use the default colour gradient
            out=out+scale_colour_viridis_c(limits=quantile(opt$groups,c(0.05,0.95),na.rm = TRUE),oob=squish,name=opt$factor_name)
        }
        out=out+theme_Publication(base_size = 12)
        # add ellipse for all samples (ignoring group)
        if (obj$ellipse %in% c('all','sample')) {
            out=out+stat_ellipse(type='norm',mapping=aes(x=x,y=y),colour="#C0C0C0",linetype='dashed',data=A)
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
                    out=out+geom_text(data=temp,aes_(x=~x,y=~y,label=~label,colour=~group),size=obj$label_size,vjust="inward",hjust="inward")

                }
            }
            # add a list of outliers to the plot object
            out$outliers=trimws(slabels[!points$in.ell])
        }

        # label all points if requested
        if (opt$points_to_label=='all')
        {
            out=out+geom_text(vjust="inward",hjust="inward")
        }

        return(out)
    }
)

#################################################################
#################################################################

#' pca_biplot_plot class
#'
#' 2d scatter plot of princpal component scores overlaid with principal component loadings.
#'
#' @import struct
#' @export pca_biplot_plot
#' @include PCA_class.R
#' @examples
#' C = pca_biplot_plot()
pca_biplot_plot = function(...) {
    out=.pca_biplot_plot()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.pca_biplot_plot<-setClass(
    "pca_biplot_plot",
    contains='chart',
    slots=c(
        # INPUTS
        params_components='entity',
        params_points_to_label='entity',
        params_factor_name='entity',
        params_groups='entity',
        params_scale_factor='entity',
        params_style='enum',
        params_label_features='entity'
    ),
    prototype = list(name='Feature boxplot',
        description='plots a boxplot of a chosen feature for each group of a DatasetExperiment.',
        type="boxlot",
        params_components=entity(name='Components to plot',
            value=c(1,2),
            type='numeric',
            description='the components to be plotted e.g. c(1,2) plots component 1 on the x axis and component 2 on the y axis.',
            max_length=2
        ),
        params_points_to_label=entity(name='points_to_label',
            value='none',
            type='character',
            description='("none"), "all", or "outliers" will be labelled on the plot.'
        ),
        params_factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The name of the factor to be displayed on the plot. Appears on axis and legend titles, for example. By default the column name of the meta data will be used where possible.'
        ),
        params_groups=entity(name='Groups',
            value=factor(),
            type='factor',
            description='The name of the factor to be displayed on the plot. Appears on axis and legend titles, for example. By default the column name of the meta data will be used where possible.'
        ),
        params_scale_factor=entity(name='Loadings scale factor',
            value=0.95,
            type='numeric',
            description='Scaling factor to apply to loadings. Default = 0.95.'
        ),
        params_style=enum(name='Plot style',
            value='points',
            type='character',
            description='Named plot styles for the biplot. [points], arrows',
            list=c('points','arrows')
        ),
        params_label_features=entity(name='Add feature labels',
            value=FALSE,
            type='logical',
            description='Include feature labels on the plot'
        )
    )

)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pca_biplot_plot",'PCA'),
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

        # additionaly scale the loadings
        sf=min(max(abs(Ts[,opt$components[1]]))/max(abs(P[,opt$components[1]])),
            max(abs(Ts[,opt$components[2]]))/max(abs(P[,opt$components[2]])))
        dobj$scores$data=as.data.frame(Ts) # nb object not returned, so only temporary scaling

        # plot
        A=data.frame("x"=P[,opt$components[1]]*sf*0.8,"y"=P[,opt$components[2]]*sf*0.8)
        C=pca_scores_plot(groups=obj$groups,points_to_label=obj$points_to_label,components=obj$components,factor_name=obj$factor_name)
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

#' pca_loadings_plot class
#'
#' 2d scatter plot of princpal component loadings.
#'
#' @import struct
#' @export pca_loadings_plot
#' @include PCA_class.R
#' @examples
#' C = pca_loadings_plot()
pca_loadings_plot = function(...) {
    out=.pca_loadings_plot()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.pca_loadings_plot<-setClass(
    "pca_loadings_plot",
    contains='chart',
    slots=c(
        # INPUTS
        params_components='entity',
        params_style='enum',
        params_label_features='entity'
    ),
    prototype = list(name='Feature boxplot',
        description='plots a boxplot of a chosen feature for each group of a DatasetExperiment.',
        type="boxlot",
        params_components=entity(name='Components to plot',
            value=c(1,2),
            type='numeric',
            description='the components to be plotted e.g. c(1,2) plots component 1 on the x axis and component 2 on the y axis.',
            max_length=2
        ),
        params_style=enum(name='Plot style',
            value='points',
            type='character',
            description='Named plot styles for the biplot. [points], arrows',
            list=c('points','arrows')
        ),
        params_label_features=entity(name='Add feature labels',
            value=FALSE,
            type='logical',
            description='Include feature labels on the plot'
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
        if (length(opt$components)==1)
        {
            A=data.frame("x"=1:nrow(P),"y"=P[,opt$components[1]])
            out=ggplot(data=A,aes_(x=~x,y=~y)) +
                geom_line() +
                ggtitle('PCA Loadings', subtitle=NULL) +
                xlab('Feature') +
                ylab('Loading') +
                scale_colour_Publication() +
                theme_Publication(base_size = 12)
            return(out)
        }
        # 2D plot
        if (length(opt$components)==2)
        {
            A=data.frame("x"=P[,opt$components[1]],"y"=P[,opt$components[2]])
            out=ggplot(data=A,aes_(x=~x,y=~y)) +
                geom_point() +
                ggtitle('PCA Loadings', subtitle=NULL) +
                xlab(paste0('Component ',opt$components[1])) +
                ylab(paste0('Component ',opt$components[2])) +
                scale_colour_Publication() +
                theme_Publication(base_size = 12)
            return(out)
        }
        if (length(opt$components)>2)
        {
            stop('can only plot loadings for 1 or 2 components at a time')
        }

    }
)


#' pca_scree_plot class
#'
#' line plot showing percent variance and cumulative peercent variance for the computed components.
#'
#' @import struct
#' @export pca_scree
#' @include PCA_class.R
#' @examples
#' C = pca_scree()
pca_scree = function(...) {
    out=.pca_scree()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.pca_scree<-setClass(
    "pca_scree",
    contains=c('chart'),
    prototype = list(name='Scree plot',
        description='plots the percent and cumulative percent variance for the calculated components',
        type="line"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("pca_scree",'PCA'),
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

#' pca_dstat_plot class
#'
#' line plot showing percent variance and cumulative peercent variance for the computed components.
#'
#' @import struct
#' @export PCA_dstat
#' @include PCA_class.R
#' @examples
#' C = PCA_dstat()
PCA_dstat = function(...) {
    out=.PCA_dstat()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.PCA_dstat<-setClass(
    "PCA_dstat",
    contains=c('chart'),
    slots=c(params_number_components='entity',
        params_alpha='entity'),
    prototype = list(name='d-statistic plot',
        description='a bar chart of the d-statistics for samples in the input PCA model',
        type="bar",
        params_number_components=entity(value = 2,
            name = 'number of principal components',
            description = 'number of principal components to use for the plot',
            type='numeric'),
        params_alpha=entity(value=0.95,
            name='threshold for rejecting outliers',
            description='a confidence threshold for rejecting samples based on the d-statistic',
            type='numeric')
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("PCA_dstat",'PCA'),
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

