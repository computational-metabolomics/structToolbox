#' @eval get_description('pareto_scale')
#' @examples
#' D = iris_DatasetExperiment()
#' M = pareto_scale()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#' @export pareto_scale
pareto_scale = function(...) {
    out=struct::new_struct('pareto_scale',...)
    return(out)
}

.pareto_scale<-setClass(
    "pareto_scale",
    contains=c('model'),
    slots=c(
        scaled='DatasetExperiment',
        mean='numeric',
        sd='numeric'
    ),
    prototype = list(
        name='Pareto scaling',
        type="preprocessing",
        predicted='scaled',
        description=paste0('The mean sample is subtracted from all samples ',
            'and then scaled by the square root of the standard deviation. The ',
            'transformed data has zero mean.'),
        .outputs=c('scaled','mean','sd'),
        ontology='OBI:0200037'
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("pareto_scale",'DatasetExperiment'),
    definition=function(M,D) {
        # column means
        X=D$data
        m=colMeans(X)
        output_value(M,'mean')=m
        s=apply(X, 2, sd)
        output_value(M,'sd')=s
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("pareto_scale",'DatasetExperiment'),
    definition=function(M,D) {
        X=D$data
        Xc=pscale(X,output_value(M,'mean'),output_value(M,'sd'))
        D$data=as.data.frame(Xc)
        D$name=c(D$name,'The data has been pareto scaled')
        output_value(M,'scaled')=D
        return(M)
    }
)

pscale<- function(x,m,s) {
    # http://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/
    return( (x - rep(m, rep.int(nrow(x), ncol(x)))) / rep(sqrt(s), rep.int(nrow(x), ncol(x))) )
}
