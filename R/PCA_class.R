#' PCA model class
#'
#' Principal Component Analysis (PCA) model class. This object can be used to train/apply PCA mdoels to DatasetExperiment objects.
#'
#' @import struct
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export PCA
#' @examples
#' M = PCA()
PCA = function(...) {
    out=.PCA()
    out=struct::new_struct(out,...)
    return(out)
}


.PCA<-setClass(
    "PCA",
    contains=c('model','stato'),
    slots=c(
        # INPUTS
        number_components='entity_stato',

        # OUTPUTS
        scores='entity',
        loadings='data.frame',
        eigenvalues='data.frame',
        ssx='numeric',
        correlation='data.frame',
        that='DatasetExperiment'
    ),
    prototype = list(name='Principal Component Analysis (PCA)',
        description='PCA is a multivariate data reduction technique. It summarises the data in a smaller number of Principal Components that describe the maximum variation present in the DatasetExperiment.',
        type="preprocessing",
        predicted='that',
        stato_id="OBI:0200051",

        number_components=entity_stato(name='Number of PCs',
            stato_id='STATO:0000555',
            value=2,
            type=c('numeric','integer')
        ),
        scores=entity('name'='PCA scores DatasetExperiment',
            'description'='A matrix of PCA scores where each column corresponds to a Principal Component',
            'type'='DatasetExperiment')
    )
)

#' @param ... additional slots and values passed to struct_class
#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("PCA",'DatasetExperiment'),
    definition=function(M,D)
    {
        A=param_value(M,'number_components')
        X=as.matrix(D$data)
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
        S=DatasetExperiment(data=scores,sample_meta=D$sample_meta,variable_meta=varnames)
        output_value(M,'scores')=S

        P=as.data.frame(model$v)
        rownames(P)=colnames(X)
        colnames(P)=varnames
        output_value(M,'loadings')=P

        E=data.frame('Eigenvalues'=sqrt(model$d[1:A]))
        rownames(E)=varnames
        output_value(M,'eigenvalues')=E

        output_value(M,'ssx')=sum(X*X) # sum of squares of x
        output_value(M,'correlation')=as.data.frame(cor(X,scores))

        return(M)
    }
)

#' @param ... additional slots and values passed to struct_class
#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("PCA",'DatasetExperiment'),
    definition=function(M,D)
    {
        A=param_value(M,'number_components')
        X=as.matrix(D$data)
        P=output_value(M,'loadings')
        that=X%*%as.matrix(P)

        that=as.data.frame(that)
        rownames(that)=rownames(X)
        varnames=rep('A',1,A)
        for (i in 1:A)
        {
            varnames[i]=paste0('PC',i)
        }
        colnames(that)=varnames

        # convert to DatasetExperiment for preprocessing output
        S=DatasetExperiment(data=that,sample_meta=D$sample_meta,variable_meta=varnames)
        output_value(M,'that')=S

        return(M)
    }
)







