#################################################
#################################################

#' plsda_scores_plot class
#'
#' 2d scatter plot of plsda component scores.
#'
#' @param components The PLS components to plot (\code{numeric(2)})
#' @param points_to_label "none", "all", or "outliers" will be labelled on the plot.
#' @param factor_name The sample_meta column name for labelling the legend
#' @param groups A vector for colouring the points (factor, character or numeric).
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export plsda_scores_plot
#' @include PLSDA_class.R
#' @examples
#' C = plsda_scores_plot()
plsda_scores_plot = function(components=c(1,2),points_to_label='none',factor_name,groups,...) {
    out=struct::new_struct('plsda_scores_plot',
        components=components,
        points_to_label=points_to_label,
        factor_name=factor_name,
        groups=groups,
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
        factor_name='entity',
        groups='entity'
        ),
    prototype = list(name='PLSDA scores plot',
        description='scatter plot of PLSDA component scores',
        type="scatter",
        libraries=c('pls','ggplot2'),
        .params=c('components','points_to_label','factor_name','groups'),

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
        ),
        groups=entity(name='Groups',
            value=factor(),
            type=c('factor','character','numeric'),
            description='A vector for colouring the points'
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
