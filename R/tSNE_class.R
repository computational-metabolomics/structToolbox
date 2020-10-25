#' @eval get_description('tSNE')
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
        dims='entity',
        perplexity='entity',
        max_iter='entity',
        theta='entity',
        check_duplicates='entity',
        init='entity',
        eta='entity',
        
        # OUTPUTS
        Y='DatasetExperiment'
    ),
    prototype = list(
        name='tSNE',
        description='t-Distributed Stochastic Neighbor Embedding.',
        type="preprocessing",
        predicted='tsne',
        libraries='Rtsne',
        dims=entity(
            name='Dimensionality',
            description = 'The number of tSNE dimensions computed.',
            type=c('numeric'),
            max_length=1,
            value=2
        ),
        perplexity=entity(
            name = 'Perplexity parameter',
            description='Perplexity parameter',
            value=30,
            type='numeric',
            max_length=1
        ),
        max_iter=entity(
            name='Maximum number of iterations',
            description='The maximum number of tSNE iterations.',
            value=1000,
            type='numeric',
            max_length=1
        ),
        theta=entity(
            name='Theta parameter',
            description = 'Speed/accuracy trade-off. A value of 0 gives an exact tSNE.',
            value=0.5,
            type='numeric',
            max_length=1
        ),
        check_duplicates=entity(
            name='Check for duplicates',
            description=c(
                "TRUE"='Checks for the presence of exact duplicate samples.',
                "FALSE"='Does not check for exact duplicate samples.'
            ),
            type='logical',
            value=FALSE,
            max_length=1
        ),
        init=entity(
            value=NULL,
            type=c('NULL','data.frame','DatasetExperiment'),
            name='Initial coordinates',
            description=paste0('A set of coordinates for initialising the tSNE ',
                'algorithm. NULL uses random initialisation.'
            ),
            max_length=1
        ),
        eta=entity(
            name = 'Learning rate',
            description = 'The learning rate parameter.',
            value=200,
            type='numeric',
            max_length=1
        ),
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

#' @eval get_description('tSNE_scatter')
#' @import struct
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
        factor_name='entity'
        
    ),
    prototype = list(name='Feature boxplot',
        description='plots the new representation of data after applying tSNE.',
        type="scatter",
        libraries='Rtsne',
        .params=c('factor_name'),
        factor_name=ents$factor_name
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




