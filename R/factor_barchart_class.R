#' DatasetExperiment.factor_barchart class
#'
#' Bar charts based on groupings by factor. Can plot up to three factors.
#'
#' @slot feature_to_plot Column ID of feature to plot.
#' @slot factor_names Names(s) of factors to plot for a feature
#'
#' @examples
#' D = iris_DatasetExperiment()
#' C = DatasetExperiment.factor_barchart(factor_names='Species',feature_to_plot='Petal.Width')
#' chart_plot(C,D)
#'
#' @import struct
#' @import grid
#' @import gridExtra
#' @param ... slots and values for the new object 
#' @return struct object
#' @export DatasetExperiment.factor_barchart
#' @include HSD_class.R
DatasetExperiment.factor_barchart = function(...) {
    out=.DatasetExperiment.factor_barchart()
    out=struct::new_struct(out,...)
    return(out)
}


.DatasetExperiment.factor_barchart<-setClass(
    "DatasetExperiment.factor_barchart",
    contains='chart',
    slots=c(
        feature_to_plot='entity',
        factor_names='entity'
    ),
    prototype = list(name='Factor barchart',
        description='bar charts split by factors in the sample_meta data',
        type="barchart",

        feature_to_plot=entity(name='Feature to plot',
            value='V1',
            type=c('character','numeric','integer'),
            description='The column name of the feature to be plotted.'
        ),
        factor_names=entity(name='Factor names',
            value='factor',
            type='character',
            description='The column name(s) of meta data to use.'
        )
    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("DatasetExperiment.factor_barchart",'DatasetExperiment'),
    definition=function(obj,dobj)
    {

        X=dobj$data

        if (is.numeric(obj$feature_to_plot)) {
            varn=colnames(X)[obj$feature_to_plot]
        } else {
            varn=obj$feature_to_plot
        }

        f=obj$feature_to_plot
        X=dobj$data[,f]
        SM=dobj$sample_meta

        if (length(obj$factor_names)==1) {
            # single factor plot(s)

            # get color pallete using pmp
            clrs= createClassAndColors(class = SM[[obj$factor_names[1]]])
            SM[[obj$factor_names[1]]]=clrs$class

            # prep the data
            A=data.frame(x=SM[[obj$factor_names[1]]],y=X)

            g=ggplot(data=A,aes(x=x,y=y,color=x)) +
                geom_boxplot() +
                theme_Publication(base_size=10) +
                scale_colour_manual(values=clrs$manual_colors,name=obj$factor_names[1])+
                ylab('')+
                xlab(obj$factor_names[1])+
                theme(legend.position="none")
            return(g)
        }

        if (length(obj$factor_names)==2) {
            # dual factor plot(s)

            # get color pallete using pmp
            clrs= createClassAndColors(class = SM[[obj$factor_names[2]]])
            SM[[obj$factor_names[2]]]=clrs$class

            # prep the data
            A=data.frame(x=SM[[obj$factor_names[1]]],z=SM[[obj$factor_names[2]]],y=X)

            g=ggplot(data=A,aes(x=x,y=y,color=z)) +
                geom_boxplot() +
                theme_Publication(base_size=10) +
                geom_vline(xintercept=(1:(nlevels(A$x)-1))+0.5,linetype="dashed",color='grey') +
                scale_colour_manual(values=clrs$manual_colors,name=obj$factor_names[2])+
                ylab('')+
                xlab(obj$factor_names[1])
            return(g)
        }

        if (length(obj$factor_names)==3) {
            # dual factor plot(s)

            # get color pallete using pmp
            clrs= createClassAndColors(class = SM[[obj$factor_names[2]]])
            SM[[obj$factor_names[2]]]=clrs$class

            A=data.frame(x=SM[[obj$factor_names[1]]],z=SM[[obj$factor_names[2]]],y=X,a=SM[[obj$factor_names[3]]])

            lab=as.character(interaction(obj$factor_names[3],levels(SM[[obj$factor_names[3]]]),sep = ': '))
            names(lab)=levels(SM[[obj$factor_names[3]]])
            p=ggplot(data=A,aes(x=x,y=y,color=z)) +
                geom_boxplot() +
                theme_Publication(base_size=10) +
                geom_vline(xintercept=(1:(nlevels(A$x)-1))+0.5,linetype="dashed",color='grey') +
                scale_colour_manual(values=clrs$manual_colors,name=obj$factor_names[2]) +
                facet_grid(.~a,labeller=as_labeller(lab))+
                theme(panel.background = element_rect(fill = NA, color = "black"))+
                theme(strip.background =element_rect(fill='black'))+
                theme(strip.text = element_text(colour = 'white'))+
                ylab('')+
                xlab(obj$factor_names[1])

            # colour the facet labels
            ## https://github.com/tidyverse/ggplot2/issues/2096 ##
            g = ggplot_gtable(ggplot_build(p))
            stripr = which(grepl('strip-t', g$layout$name))
            fills = clrs$manual_colors
            k = 1
            for (i in stripr) {
                j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
                g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
                k = k+1
            }
            #grid.draw(g)
            ##

            return(invisible(g))
        }

    }
)






