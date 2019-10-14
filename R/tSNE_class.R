#' tSNE method class
#'
#' t-Distributed Stochastic Neighbor Embedding (tSNE) class. This object can be used to train/apply tSNE models to dataset objects.
#'
#' @export tSNE
#' @examples
#' M = tSNE()
#'
tSNE<-setClass(
    "tSNE",
    contains=c('model'),
    slots=c(
        # INPUTS
        params.dims='numeric',
        params.perplexity='numeric',
        params.max_iter='numeric',
        params.theta='numeric',
        params.check_duplicates='logical',
        params.pca='logical',
        params.factor_name='character',

        # OUTPUTS
        outputs.Y='data.frame'
    ),
    prototype = list(
        name='tSNE',
        description='t-Distributed Stochastic Neighbor Embedding.',
        type="preprocessing",
        predicted='tsne',
        libraries='Rtsne',
        params.dims=2,
        params.perplexity=30,
        params.max_iter=1000,
        params.theta=0.5,
        params.check_duplicates=FALSE,
        params.pca=TRUE

    )
)

#' @export
#' @template method_apply
setMethod(f="model.apply",
    signature=c("tSNE",'dataset'),
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
#' @export tSNE_scatter
#' @include PCA_class.R
#' @examples
#' M = tSNE_scatter()
#'
tSNE_scatter<-setClass(
    "tSNE_scatter",
    contains='chart',
    slots=c(
        params.colour_by='numeric'

        ),
    prototype = list(name='Feature boxplot',
        description='plots the new representation of data after applying tSNE.',
        type="scatter",
        libraries='Rtsne'

    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
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




