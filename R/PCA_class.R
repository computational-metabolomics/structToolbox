#' PCA model class
#'
#' Principal Component Analysis (PCA) model class. This object can be used to train/apply PCA mdoels to dataset objects.
#'
#' @import struct
#' @export PCA
#' @examples
#' M = PCA()
PCA<-setClass(
    "PCA",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        params.number_components='entity.stato',

        # OUTPUTS
        outputs.scores='entity',
        outputs.loadings='data.frame',
        outputs.eigenvalues='data.frame',
        outputs.ssx='numeric',
        outputs.correlation='data.frame',
        outputs.that='dataset'
    ),
    prototype = list(name='Principal Component Analysis (PCA)',
        description='PCA is a multivariate data reduction technique. It summarises the data in a smaller number of Principal Components that describe the maximum variation present in the dataset.',
        type="preprocessing",
        predicted='that',
        stato.id="OBI:0200051",

        params.number_components=entity.stato(name='Number of PCs',
            stato.id='STATO:0000555',
            value=2,
            type=c('numeric','integer')
        ),
        outputs.scores=entity('name'='PCA scores dataset',
            'description'='A matrix of PCA scores where each column corresponds to a Principal Component',
            'type'='dataset')
    )
)

#' @export
#' @template model_train
setMethod(f="model.train",
    signature=c("PCA",'dataset'),
    definition=function(M,D)
    {
        A=param.value(M,'number_components')
        X=as.matrix(dataset.data(D))
        model=svd(X,A,A)
        if (A==1)
        {
            scores=model$u * model$d[A]
        } else {
            scores=model$u %*% diag(model$d[1:A])
        }

        scores=as.data.frame(scores)
        rownames(scores)=rownames(X)
        varnames=rep('A',1,A)
        for (i in 1:A)
        {
            varnames[i]=paste0('PC',i)
        }
        colnames(scores)=varnames
        S=D
        S$data=scores
        output.value(M,'scores')=S

        P=as.data.frame(model$v)
        rownames(P)=colnames(X)
        colnames(P)=varnames
        output.value(M,'loadings')=P

        E=data.frame('Eigenvalues'=sqrt(model$d[1:A]))
        rownames(E)=varnames
        output.value(M,'eigenvalues')=E

        output.value(M,'ssx')=sum(X*X) # sum of squares of x
        output.value(M,'correlation')=as.data.frame(cor(X,scores))

        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model.predict",
    signature=c("PCA",'dataset'),
    definition=function(M,D)
    {
        A=param.value(M,'number_components')
        X=as.matrix(dataset.data(D))
        P=output.value(M,'loadings')
        that=X%*%as.matrix(P)

        that=as.data.frame(that)
        rownames(that)=rownames(X)
        varnames=rep('A',1,A)
        for (i in 1:A)
        {
            varnames[i]=paste0('PC',i)
        }
        colnames(that)=varnames

        # convert to dataset for preprocessing output
        S=D
        dataset.data(S)=that
        output.value(M,'that')=S

        return(M)
    }
)







