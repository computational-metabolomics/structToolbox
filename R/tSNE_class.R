#' tSNE method class
#'
#' t-Distributed Stochastic Neighbor Embedding (tSNE) class. This object can be
#' used to train/apply tSNE models to DatasetExperiment objects.
#'
#' This object is a wrapper for Rtsne::Rtsne.
#'
#' @inheritParams Rtsne::Rtsne
#' @param init Initial locations of the objects. If NULL, random initialization will be used.
#' If NULL then
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export tSNE
#' @examples
#' M = tSNE()
#'
tSNE = function(dims=2,perplexity=30,max_iter=100,theta=0.5,check_duplicates=FALSE,init=NULL,eta=200,...) {
    out=struct::new_struct('tSNE',
        dims=dims,
        perplexity=perplexity,
        max_iter=max_iter,
        theta=theta,
        check_duplicates=check_duplicates,
        init=init,
        eta=eta,
        ...)
    return(out)
}


.tSNE<-setClass(
    "tSNE",
    contains=c('model'),
    slots=c(
        # INPUTS
        dims='numeric',
        perplexity='numeric',
        max_iter='numeric',
        theta='numeric',
        check_duplicates='logical',
        init='entity',
        factor_name='character',
        eta='numeric',

        # OUTPUTS
        Y='DatasetExperiment'
    ),
    prototype = list(
        name='tSNE',
        description='t-Distributed Stochastic Neighbor Embedding.',
        type="preprocessing",
        predicted='tsne',
        libraries='Rtsne',
        dims=2,
        perplexity=30,
        max_iter=1000,
        theta=0.5,
        check_duplicates=FALSE,
        init=entity(
            value=NULL,
            type=c('NULL','data.frame','DatasetExperiment'),
            name='Initial coordinates',
            description='A set of coordinates for initialising the tSNE
            algorithm. Setting to NULL (default) uses random initialisation.'),
        eta=200,
        .params=c('dims','perplexity','max_iter','theta','check_duplicates','init','eta'),
        .outputs='Y'

    )
)


#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("tSNE",'DatasetExperiment'),
    definition=function(M,D)
    {

        # convert Y_init to matrix
        Y_init=M$init
        if (!is.null(Y_init)) {
            if (is(Y_init,'DatasetExperiment')) {
                Y_init=Y_init$data
            }
            Y_init=as.matrix(Y_init)
        }

        # do tSNE
        tsne=Rtsne::Rtsne(D$data,
            Y_init=Y_init,
            dims=M$dims,
            perplexity=M$perplexity,
            theta=M$theta,
            check_duplicates=M$check_duplicates,
            max_iter=M$max_iter,
            eta=M$eta)

        Y=tsne$Y
        Y=as.data.frame(Y)
        rownames(Y)=rownames(D$data)

        Y=DatasetExperiment(data=Y,sample_meta=D$sample_meta,variable_meta=colnames(Y))
        M$Y=Y

        return(M)
    }
)

#' tSNE_scatter class
#'
#' plots the new representation of data after applying tSNE
#'
#' @import struct
#' @param factor_name Sample_meta column named used for colouring the points.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export tSNE_scatter
#' @include PCA_class.R
#' @examples
#' M = tSNE_scatter(factor_name='Species')
#'
tSNE_scatter = function(factor_name,...) {
    out=struct::new_struct('tSNE_scatter',
        factor_name=factor_name,
        ...)
    return(out)
}


.tSNE_scatter<-setClass(
    "tSNE_scatter",
    contains='chart',
    slots=c(
        factor_name='character'

        ),
    prototype = list(name='Feature boxplot',
        description='plots the new representation of data after applying tSNE.',
        type="scatter",
        libraries='Rtsne',
        .params=c('factor_name')

    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("tSNE_scatter",'tSNE'),
    definition=function(obj,dobj) {
        colour_by=dobj$Y$sample_meta[[obj$factor_name]]

        A=data.frame(x=dobj$Y$data[,1],y=dobj$Y$data[,2],group=colour_by)

        out=ggplot(data=A,aes_(x=~x,y=~y,colour=~group)) +
            geom_point() +
            scale_colour_Publication() +
            theme_Publication(base_size = 12)+
            ylab('Dim 2')+
            xlab('Dim 1')

        return(out)
    }
)




