#' Autoscale
#'
#' Autoscaling centres the columns of the data in a dataset object and divides by the standard deviation.
#'
#' @return A STRUCT model object with methods for autoscaling.
#'
#' @examples
#' D = iris_dataset()
#' M = autoscale()
#' M = model.train(M,D)
#' M = model.predict(M,D)
#'
#' @export autoscale
autoscale<-setClass(
    "autoscale",
    contains='model',
    slots=c(
        outputs.autoscaled='dataset',
        outputs.mean='numeric',
        outputs.sd='numeric'
    ),
    prototype = list(name='Autoscaling',
        type="preprocessing",
        predicted='autoscaled'
    )
)

#' @export
#' @template model_train
setMethod(f="model.train",
    signature=c("autoscale",'dataset'),
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
    signature=c("autoscale",'dataset'),
    definition=function(M,D)
    {
        X=dataset.data(D)
        Xc=ascale(X,output.value(M,'mean'),output.value(M,'sd'))
        dataset.data(D)=as.data.frame(Xc)
        name(D)=c(name(D),'The data has been autoscaled')
        output.value(M,'autoscaled')=D
        return(M)
    }
)

ascale<- function(x,m,s) {
    # http://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/
    return( (x - rep(m, rep.int(nrow(x), ncol(x)))) / rep(s, rep.int(nrow(x), ncol(x))) )
}
