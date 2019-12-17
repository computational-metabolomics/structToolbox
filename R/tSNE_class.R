#' tSNE method class
#'
#' t-Distributed Stochastic Neighbor Embedding (tSNE) class. This object can be used to train/apply tSNE models to DatasetExperiment objects.
#'
#' @param ... slots and values for the new object 
#' @export tSNE
#' @examples
#' M = tSNE()
#'
tSNE = function(...) {
    out=.tSNE()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.tSNE<-setClass(
    "tSNE",
    contains=c('model'),
    slots=c(
        # INPUTS
        params_dims='numeric',
        params_perplexity='numeric',
        params_max_iter='numeric',
        params_theta='numeric',
        params_check_duplicates='logical',
        params_pca='logical',
        params_factor_name='character',

        # OUTPUTS
        outputs_Y='data.frame'
    ),
    prototype = list(
        name='tSNE',
        description='t-Distributed Stochastic Neighbor Embedding.',
        type="preprocessing",
        predicted='tsne',
        libraries='Rtsne',
        params_dims=2,
        params_perplexity=30,
        params_max_iter=1000,
        params_theta=0.5,
        params_check_duplicates=FALSE,
        params_pca=TRUE

    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("tSNE",'DatasetExperiment'),
    definition=function(M,D)
    {
        # do tSNE
        tsne=Rtsne::Rtsne(D$data,
            pca=M$pca,
            dims=M$dims,
            perplexity=M$perplexity,
            theta=M$theta,
            check_duplicates=M$check_duplicates,
            max_iter=M$max_iter)

        Y=tsne$Y
        Y=as.data.frame(Y)
        rownames(Y)=rownames(D$data)

        M$Y=Y

        return(M)
    }
)

#' tSNE_scatter class
#'
#' plots the new representation of data after applying tSNE
#'
#' @import struct
#' @param ... slots and values for the new object 
#' @export tSNE_scatter
#' @include PCA_class.R
#' @examples
#' M = tSNE_scatter()
#'
tSNE_scatter = function(...) {
    out=.tSNE_scatter()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.tSNE_scatter<-setClass(
    "tSNE_scatter",
    contains='chart',
    slots=c(
        params_colour_by='numeric'

        ),
    prototype = list(name='Feature boxplot',
        description='plots the new representation of data after applying tSNE.',
        type="scatter",
        libraries='Rtsne'

    )
)

#' @param ... slots and values for the new object 
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("tSNE_scatter",'tSNE'),
    definition=function(obj,dobj)
    {
        colour_by=obj$colour_by
        colour_by=factor(colour_by)
        if (nlevels(colour_by)>7) {
            colour_by=as.numeric(obj$colour_by)
        }

        A=data.frame(x=dobj$Y[,1],y=dobj$Y[,2],group=colour_by)

        out=ggplot(data=A,aes_(x=~x,y=~y,colour=~group)) +
            geom_point() +
            scale_colour_Publication() +
            theme_Publication(base_size = 12)+
            ylab('Dim 2')+
            xlab('Dim 1')

        return(out)
    }
)




