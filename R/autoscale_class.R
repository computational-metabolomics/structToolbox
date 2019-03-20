#' autoscale class
#'
#' autoscaling centres the columns of the data in a dataset object and divides by the standard deviation.
#' @export autoscale
autoscale<-setClass(
  "autoscale",
  contains='model',
  slots=c(outputs.autoscaled='dataset',
          outputs.mean='numeric',
          outputs.sd='numeric'
  ),
  prototype = list(name='Autoscaling',
                   type="preprocessing",
                   outputs=c('mean','sd','autoscaled'),
                   predicted='autoscaled'
  )
)

#' @export
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
