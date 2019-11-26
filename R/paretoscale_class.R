#' Pareto scaling
#'
#' Pareto scaling centres the columns of the data in a dataset object and divides by the square root of the standard deviation.
#'
#' @return A STRUCT model object with methods for pareto scaling.
#'
#' @examples
#' D = iris_dataset()
#' M = pareto_scale()
#' M = model.train(M,D)
#' M = model.predict(M,D)
#'
#' @export pareto_scale
pareto_scale<-setClass(
    "pareto_scale",
    contains='model',
    slots=c(
        outputs.scaled='dataset',
        outputs.mean='numeric',
        outputs.sd='numeric'
    ),
    prototype = list(name='Pareto scaling',
        type="preprocessing",
        predicted='scaled'
    )
)

#' @export
#' @template model_train
setMethod(f="model.train",
    signature=c("pareto_scale",'dataset'),
    definition=function(M,D)
    {
        # column means
        X=dataset.data(D)
        m=colMeans(X)
        output.value(M,'mean')=m
        s=apply(X, 2, sd)
        output.value(M,'sd')=s
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model.predict",
    signature=c("pareto_scale",'dataset'),
    definition=function(M,D)
    {
        X=dataset.data(D)
        Xc=pscale(X,output.value(M,'mean'),output.value(M,'sd'))
        dataset.data(D)=as.data.frame(Xc)
        name(D)=c(name(D),'The data has been autoscaled')
        output.value(M,'scaled')=D
        return(M)
    }
)

pscale<- function(x,m,s) {
    # http://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/
    return( (x - rep(m, rep.int(nrow(x), ncol(x)))) / rep(sqrt(s), rep.int(nrow(x), ncol(x))) )
}
