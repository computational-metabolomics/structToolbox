#' @eval get_description('scatter_chart')
#' @import struct
#' @export scatter_chart
#' @examples
#' D = iris_DatasetExperiment()
#' C = scatter_chart(xcol='Petal.Width',ycol='Sepal.Width',factor_name = 'Species')
#' chart_plot(C,M[2])
#'
scatter_chart = function(
    xcol=1,
    ycol=2,
    points_to_label='none',
    factor_name='none',
    ellipse='all',
    ellipse_type='norm',
    ellipse_confidence=0.95,
    label_filter=character(0),
    label_factor='rownames',
    label_size=3.88,
    ...) {
    out=struct::new_struct('scatter_chart',
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


.scatter_chart<-setClass(
    "scatter_chart",
    contains='chart',
    slots=c(
        # INPUTS
        xcol='entity',
        ycol='entity',
        points_to_label='enum',
        factor_name='entity',
        ellipse='enum',
        ellipse_type='enum',
        ellipse_confidence='entity',
        label_filter='entity',
        label_factor='entity',
        label_size='entity'
    ),
    
    prototype = list(name='Group scatter chart',
        description='Plots a 2d scatter plot of the input data.',
        type="scatter",
        .params=c('xcol','ycol','points_to_label','factor_name','ellipse',
            'label_filter','label_factor','label_size','ellipse_type',
            'ellipse_confidence'),
        
        xcol=entity(
            name='x-axis column name',
            value=1,
            type=c('numeric','integer','character'),
            description=paste0('The column name, or index, of data to plot on the x-axis'),
            max_length=1
        ),
        
        ycol=entity(
            name='y-axis column name',
            value=2,
            type=c('numeric','integer','character'),
            description=paste0('The column name, or index, of data to plot on the y-axis'),
            max_length=1
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
    signature=c("scatter_chart",'DatasetExperiment'),
    definition=function(obj,dobj)
    {
        # if provided convert index to names
        if (is.numeric(obj$xcol)) {
            obj$xcol=colnames(dobj)[obj$xcol]
        }
        if (is.numeric(obj$ycol)) {
            obj$ycol=colnames(dobj)[obj$ycol]
        }
        
        if (obj$points_to_label=='outliers' & !(obj$ellipse %in% c('all','sample'))) {
            warning('Outliers are only labelled when plotting the sample ellipse')
        }
        
        if (length(obj$factor_name)==1) {
            shapes = 19 # filled circles for all samples
        } else {
            shapes = factor(dobj$sample_meta[[obj$factor_name[2]]])
        }
        
        if (obj$label_factor=='rownames') {
            slabels = rownames(dobj$sample_meta)
        } else {
            slabels = dobj$sample_meta[[obj$label_factor]]
        }
        obj$factor_name=obj$factor_name[[1]] # only use the first factor from now on
        
        x=dobj$data[,obj$xcol]
        y=dobj$data[,obj$ycol]
        xlabel=obj$xcol
        ylabel=obj$ycol
        
        # get the factor from meta data
        groups=dobj$sample_meta[[obj$factor_name]]
        
        # add a space to the front of the labels to offset them from the points, because nudge_x is in data units
        for (i in 1:length(slabels)) {
            slabels[i]=paste0('  ',slabels[i], '  ')
        }
        
        # filter by label_filter list if provided
        if (length(obj$label_filter)>0) {
            out=!(as.character(groups) %in% obj$label_filter)
            slabels[out]=''
        }
        
        if (is(groups,'factor') | is(groups,'character')) {
            plotClass= createClassAndColors(groups)
            groups=plotClass$class
        }
        
        # build the plot
        A <- data.frame (group=groups,x=x, y=y,slabels=slabels)
        
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
            ylab(ylabel) 
        
        if (length(obj$factor_name)==2) {
            out=out+labs(shape=obj$factor_name[[2]],colour=obj$factor_name[[1]])
        } else {
            out=out+labs(shape=obj$factor_name[[1]])
        }
        
        if (obj$ellipse %in% c('all','group')) {
            out = out +stat_ellipse(data=A, aes_(x=~x,y=~y,colour=~group),type=obj$ellipse_type,
                level=obj$ellipse_confidence) # ellipse for individual groups
        }
        
        if (is(groups,'factor')) { # if a factor then plot by group using the colours from pmp package
            out=out+scale_colour_manual(values=plotClass$manual_colors,
                name=obj$factor_name)
        }else {# assume continuous and use the default colour gradient
            out=out+scale_colour_viridis_c(limits=quantile(groups,
                c(0.05,0.95),na.rm = TRUE),oob=squish,name=obj$factor_name)
        }
        out=out+theme_Publication(base_size = 12)
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
            if (obj$points_to_label=='outliers')
            {
                if (!all(points$in.ell))
                {
                    temp=subset(points,!points$in.ell)
                    temp$group=groups[!points$in.ell]
                    temp$label=slabels[!points$in.ell]
                    out=out+geom_text(data=temp,aes_(x=~x,y=~y,label=~label,colour=~group),size=obj$label_size,vjust="inward",hjust="inward",show.legend=FALSE)
                    
                }
            }
            # add a list of outliers to the plot object
            out$outliers=trimws(slabels[!points$in.ell])
        }
        
        # label all points if requested
        if (obj$points_to_label=='all') {
            out=out+geom_text(data=A,aes_string(x='x',y='y',colour='group',label='slabels'),vjust="inward",hjust="inward",show.legend=FALSE)
        }
        
        return(out)
    }
)