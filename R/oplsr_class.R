#' @eval get_description('OPLSR')
#' @export OPLSR
#' @examples
#' M = OPLSR('number_components'=2,factor_name='Species')
OPLSR = function(number_components=2,factor_name,...) {
    out=struct::new_struct('OPLSR',
        number_components = number_components,
        factor_name=factor_name,
        ...)
    return(out)
}

.OPLSR<-setClass(
    "OPLSR",
    contains=c('model'),
    slots=c(
        number_components='entity',
        factor_name='entity',
        opls_model='list',
        filtered='DatasetExperiment',
        orthogonal='DatasetExperiment'
    ),
    prototype = list(name='Orthogonal Partial Least Squares regression',
        type="filtering",
        predicted='filtered',
        description=paste0('OPLS splits a data matrix into two parts. One part 
        contains information orthogonal to the input vector, and the other is non-orthogonal.'),
        .params=c('number_components','factor_name'),
        .outputs=c('opls_model','filtered','orthogonal'),
        number_components=entity(value = 2,
            name = 'Number of OPLS components',
            description = 'The number of orthgonal components',
            type = c('numeric','integer')
        ),
        factor_name=structToolbox:::ents$factor_name
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("OPLSR",'DatasetExperiment'),
    definition=function(M,D) {
        
        # prepare
        y=D$sample_meta
        
        y=y[,M$factor_name,drop=FALSE] # might be multiple columns (?)

        X=as.matrix(D$data) # convert X to matrix
        y=as.matrix(y)
        
        # orthogonal PLS
        opls_model = nipals_opls(X, y, M$number_components)
        
        # create DE from filtered X
        Xfilt = DatasetExperiment(
            data=as.data.frame(opls_model$Xfilt),
            sample_meta = D$sample_meta,
            variable_meta = D$variable_meta,
            name = 'OPLS filtered'
        )
        Xorth = DatasetExperiment(
            data=as.data.frame(opls_model$Xorth),
            sample_meta = D$sample_meta,
            variable_meta = D$variable_meta,
            name = 'OPLS orthogonal'
        )
        # store the results
        M$opls_model = opls_model
        M$filtered = Xfilt
        M$orthogonal = Xorth
        
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("OPLSR",'DatasetExperiment'),
    definition=function(M,D) {
        
        X = as.matrix(D$data)
        Wo= M$opls_model$Wo
        Po= M$opls_model$Po
        Tno = list()
        
        # remove orthogonal components from test set
        for (n in 1:M$number_components) {
            wo = Wo[,n,drop=FALSE]
            po = Po[,n,drop=FALSE]
            tno = (X %*% wo) / as.numeric((t(wo) %*% wo))
            # deflate
            X = X - (tno %*% t(po)) 
            Tno[[n]]=tno
        }
        Tno = do.call(cbind,Tno)
        Xorth = Tno %*% t(Po)
        # create DE from filtered X
        Xfilt = DatasetExperiment(
            data=as.data.frame(X),
            sample_meta = D$sample_meta,
            variable_meta = D$variable_meta,
            name = 'OPLS filtered'
        )
        Xorth = DatasetExperiment(
            data=as.data.frame(Xorth),
            sample_meta = D$sample_meta,
            variable_meta = D$variable_meta,
            name = 'OPLS orthogonal'
        )
        M$filtered=Xfilt
        M$orthogonal=Xorth
        
        return(M)
    }
)


nipals_opls = function(X, Y, N) {
    # X matrix (samples in rows, variables in columns)
    # Y matrix (samples in rows, k columns)
    # N number of orthogonal components
    
    # prepare
    Wo = list()
    To = list()
    Po = list()
    
    # compute weights
    W = list()
    for (k in 1:ncol(Y)) {
        y=Y[,k,drop=FALSE]
        v = t(X) %*% y 
        w = v / as.numeric((t(v) %*% v))        
        W[[k]]=w
    }
    W = do.call(cbind,W) # J x K
    
    # PCA of weights matrix
    model=svd(W,ncol(W),ncol(W)) # assume fewer Y columns than X columns
    # get the pca scores
    if (ncol(W)==1) {
        Tw=model$u * model$d[ncol(W)]
    } else {
        Tw=model$u %*% diag(model$d[1:ncol(W)])
    }
    
    # for each orthogonal component
    for (n in 1:N) {
        u = Y[,1,drop=FALSE]
        unew = u*100
        
        while (sqrt(sum((u - unew)^2)) > 1e-6) {
            u = unew
            
            v = t(X) %*% u                              # u-weights
            w = v / sqrt(as.numeric(t(u) %*% u))        # normalised u-weights
            
            t = X %*% w                                 # x-scores
            q = (t(Y) %*% t) / as.numeric((t(t) %*% t)) # Y-loadings
            
            unew = Y %*% q / as.numeric(t(q) %*% q)
        }
        
        p = (t(X) %*% t) / as.numeric((t(t) %*% t)) # x-loadings
        for (k in 1:ncol(Tw)) {
            tw=Tw[,k,drop=FALSE]
            p = p - as.numeric(((t(tw) %*% p) / (t(tw) %*% tw))) * tw
        }
        wo = p
        
        wo = wo / as.numeric(sqrt(t(wo) %*% wo))
        to = X %*% wo / as.numeric(t(wo) %*% wo)
        po = (t(X) %*% to) / as.numeric(t(to) %*% to)
        
        # deflate
        X = X - (to %*% t(po))
        # need to deflate Y?
        
        # store the results
        Wo[[n]] = wo
        To[[n]] = to
        Po[[n]] = po
    }
    Wo = do.call(cbind,Wo)
    To = do.call(cbind,To)
    Po = do.call(cbind,Po)
    
    colnames(Wo)=paste0('OC',1:n)
    colnames(To)=paste0('OC',1:n)
    colnames(Po)=paste0('OC',1:n)
    
    opls_model=list(
        Wo = Wo, 
        To = To, 
        Po = Po, 
        Xfilt = X, 
        Xorth = To %*% t(Po))
    
    return(opls_model)
}





