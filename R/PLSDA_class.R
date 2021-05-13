#' @eval get_description('PLSDA')
#' @export PLSDA
#' @examples
#' M = PLSDA('number_components'=2,factor_name='Species')
PLSDA = function(number_components=2,factor_name,...) {
    out=struct::new_struct('PLSDA',
        number_components=number_components,
        factor_name=factor_name,
        ...)
    return(out)
}

.PLSDA<-setClass(
    "PLSDA",
    contains=c('model','stato'),
    slots=c(number_components='entity',
        factor_name='entity',
        scores='data.frame',
        loadings='data.frame',
        yhat='data.frame',
        design_matrix='data.frame',
        y='data.frame',
        reg_coeff='data.frame',
        probability='data.frame',
        vip='data.frame',
        pls_model='list',
        pred='data.frame',
        threshold='numeric',
        sr = 'entity',
        sr_pvalue='entity'
        
    ),
    prototype = list(name='Partial least squares discriminant analysis',
        type="classification",
        predicted='pred',
        libraries='pls',
        description=paste0('PLS is a multivariate regression technique that ',
            'extracts latent variables maximising covariance between the input ',
            'data and the response. The Discriminant Analysis variant uses group ',
            'labels in the response variable and applies a threshold to the ',
            'predicted values in order to predict group membership for new samples.'),
        .params=c('number_components','factor_name'),
        .outputs=c(
            'scores',
            'loadings',
            'yhat',
            'design_matrix',
            'y',
            'reg_coeff',
            'probability',
            'vip',
            'pls_model',
            'pred',
            'threshold',
            'sr',
            'sr_pvalue'),
        
        number_components=entity(value = 2,
            name = 'Number of components',
            description = 'The number of PLS components',
            type = c('numeric','integer')
        ),
        factor_name=ents$factor_name,
        sr = entity(
            name = 'Selectivity ratio',
            description = paste0(
                "Selectivity ratio for a variable represents a measure of a ",
                "variable's importance in the PLS model. The output data.frame ",
                "contains a column of selectivity ratios, a column of p-values ",
                "based on an F-distribution and a column indicating ",
                "significance at p < 0.05."
            ),
            type='data.frame'
        ),
        sr_pvalue = entity(
            name = 'Selectivity ratio p-value',
            description = paste0(
                "A p-value computed from the Selectivity Ratio based on an ",
                "F-distribution."
            ),
            type='data.frame'
        ),
        stato_id='STATO:0000572',
        citations=list(
            bibentry(
                bibtype ='Article',
                year = 2009,
                volume = 95,
                number = 2,
                pages = '122-128',
                author = as.person("Nestor F. Perez and Joan Ferre and Ricard Boque"),
                title = paste0('Calculation of the reliability of ',
                    'classification in discriminant partial least-squares ',
                    'binary classification'),
                journal = "Chemometrics and Intelligent Laboratory Systems"
            ),
            bibentry(
                bibtype='Article',
                year = 2003,
                volume = 17,
                number = 3,
                pages = '166-173',
                author = as.person('Matthew Barker and William Rayens'),
                title = 'Partial least squares for discrimination',
                journal = 'Journal of Chemometrics'
            )
        )
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("PLSDA",'DatasetExperiment'),
    definition=function(M,D)
    {
        SM=D$sample_meta
        y=(SM[[M$factor_name]])
        # convert the factor to a design matrix
        z=model.matrix(~y+0)
        z[z==0]=-1 # +/-1 for PLS
        
        X=as.matrix(D$data) # convert X to matrix
        
        Z=as.data.frame(z)
        output_value(M,'design_matrix')=Z
        output_value(M,'y')=D$sample_meta
        
        #pls_model=pls::plsr(y ~ X,validation="none",method='oscorespls',model=TRUE,ncomp=param_value(M,'number_components'),
        #    center=FALSE,
        #    scale=FALSE)
        
        
        pls_model=pls::oscorespls.fit(X,z,ncomp=param_value(M,'number_components'),
            center=FALSE)
        class(pls_model)='mvr'
        
        ny=ncol(z)
        nr=ncol(output_value(M,'reg_coeff'))
        output_value(M,'reg_coeff')=as.data.frame(pls_model$projection %*% t(pls_model$Yloadings)) # keep only the requested number of components
        output_value(M,'vip')=as.data.frame(vips(pls_model))
        colnames(M$vip)=levels(y)
        
        yhat=predict(pls_model, ncomp = param_value(M,'number_components'), newdata = X)
        yhat=as.matrix(yhat[,,dim(yhat)[3]])
        output_value(M,'yhat')=as.data.frame(yhat)
        
        probs=prob(yhat,yhat,D$sample_meta[[M$factor_name]])
        output_value(M,'probability')=as.data.frame(probs$ingroup)
        output_value(M,'threshold')=probs$threshold
        output_value(M,'pls_model')=list(pls_model)
        
        scores=pls::scores(pls_model)
        output_value(M,'scores')=as.data.frame(matrix(scores,nrow = nrow(scores),ncol=ncol(scores)))
        colnames(M$scores)=as.character(interaction('LV',1:ncol(M$scores)))
        rownames(M$scores)=rownames(D$data)
        
        loadings=pls::loadings(pls_model)
        M$loadings=as.data.frame(matrix(pls_model$loadings,nrow=nrow(loadings),ncol=ncol(loadings)))
        colnames(M$loadings)=as.character(interaction('LV',1:ncol(M$loadings)))
        rownames(M$loadings)=colnames(D$data)
        rownames(M$vip)=rownames(M$loadings)
        rownames(M$reg_coeff)=rownames(M$loadings)
        colnames(M$reg_coeff)=colnames(M$vip)
        
        SR=SRp=data.frame(row.names=colnames(X))
        for(k in 1:ncol(M$reg_coeff)){
            SR[,k] = SelRat(pls_model,X,b=as.matrix(M$reg_coeff[,k]))
            SRp[,k]= pf(SR[,k],nrow(X)-2,nrow(X)-3,lower.tail = FALSE)
        }
        colnames(SR)=levels(y)
        colnames(SRp)=levels(y)

        rownames(SR) = colnames(X)
        rownames(SRp) = colnames(X)
        
        
        M$sr = SR
        M$sr_pvalue = SRp
        
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("PLSDA",'DatasetExperiment'),
    definition=function(M,D)
    {
        # convert X to matrix
        X=as.matrix(D$data)
        # get training set y vector
        SM=D$sample_meta
        y=(SM[[M$factor_name]])
        # get predictions
        p=predict(output_value(M,'pls_model')[[1]], ncomp = param_value(M,'number_components'), newdata = X)
        p=p[,,dim(p)[3]]
        if (!is.matrix(p)){
            p=t(as.matrix(p)) # in case of leave one out p is a numeric instead of a matrix
        }
        
        ## probability estimate
        # http://www.eigenvector.com/faq/index.php?id=38%7C
        d=prob(x=p,yhat=output_value(M,'yhat'),ytrue=M$y[,M$factor_name])
        pred=(p>d$threshold)*1
        pred=apply(pred,MARGIN=1,FUN=which.max)
        hi=apply(d$ingroup,MARGIN=1,FUN=which.max) # max probability
        if (sum(is.na(pred)>0))
        {
            pred[is.na(pred)]=hi[is.na(pred)] # if none above threshold, use group with highest probability
        }
        pred=factor(pred,levels=1:length(levels(SM[[M$factor_name]])),labels=levels(SM[[M$factor_name]])) # make sure pred has all the levels of y
        q=data.frame("pred"=pred)
        output_value(M,'pred')=q
        return(M)
    }
)


prob=function(x,yhat,ytrue)
{
    # x is predicted values
    # yhat is training model
    # ytrue are real group labels
    ytrue=as.factor(ytrue)
    L=levels(ytrue)
    
    din=dout=x*0
    d=list()
    threshold=numeric(length(L))
    for (i in 1:length(L))
    {
        ingroup=yhat[ytrue==L[i],i]
        m1=mean(ingroup)
        s1=max(c(sd(ingroup),1e-5)) # make sure sd is not zero
        din[,i]=dnorm(x[,i,drop=FALSE],m1,s1)
        
        outgroup=yhat[ytrue!=L[i],i]
        m2=mean(outgroup)
        s2=max(c(sd(outgroup),1e-5)) # make sure sd is not 0
        dout[,i]=dnorm(x[,i,drop=FALSE],m2,s2)
        
        # threshold where distributions cross
        t=gauss_intersect(m1,m2,s1,s2)
        
        if (is.complex(t)) {
            # get the real values but only if the imaginary part is small
            w=which(abs(Im(t)) < 1e-6)
            if (length(w)==0){
                t=0 # all imaginary parts too large, so default to 0
            }
            t=Re(t[w])
        }
        
        if (length(t)>1) {
            # multiple cross over points so choose the one closest to 0
            t=t[which.min(abs(t))]
        }
        
        if (length(t)==0 ) {
            # length is zero. how does this happen? default to 0.
            t=0
        }
        
        if (is.na(t)) {
            # if polyroot failed return 0
            t=0
        }
        
        
        threshold[i]=t
    }
    d$ingroup=din/(din+dout) # assume probabilities sum to 1
    d$outgroup=dout/(din+dout) # assume probabilities sum to 1
    # (has to belong to in or outgroup)
    d$ingroup[is.nan(d$ingroup)]=0
    d$outgroup[is.nan(d$outgroup)]=0
    d$threshold=threshold
    return(d)
}


gauss_intersect=function(m1,m2,s1,s2)
{
    #https://stackoverflow.com/questions/22579434/python-finding-the-intersection-point-of-two-gaussian-curves
    a=(1/(2*s1*s1))-(1/(2*s2*s2))
    b=(m2/(s2*s2)) - (m1/(s1*s1))
    c=((m1*m1) / (2*s1*s1)) - ((m2*m2)/(2*s2*s2)) - log(s2/s1)
    t=tryCatch({
        g=polyroot(c(c,b,a))
        return(g)
    },warning=function(w) {
        g = NA # polyroot unreliable so return NA
        return(g)
    }, error=function(e) {
        g = NA # polyroot failed so return NA
        return(g)
    }
    )
    
    return() # in reverse order vs numpy.roots
}

vips<-function(object)
{
    # modified from http://mevik.net/work/software/pls.html
    q=object$Yloadings
    ny=nrow(object$Yloadings) # number of groups
    vip=matrix(0,nrow=nrow(object$loadings),ny)
    for (i in 1:ny)
    {
        qq=q[i,,drop=FALSE] # for group i only
        SS <- c(qq)^2 * colSums(object$scores^2)
        Wnorm2 <- colSums(object$loading.weights^2)
        SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
        v=sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
        if (ncol(SSW)>1)
            vip[,i]=t(v[nrow(v),,drop=FALSE]) # get number of calculated components only
        else
        {
            vip[,i]=t(v)
        }
    }
    return(vip)
}


set_scale <- function(y=NULL,name='PCA',...){
    
    #library(scales)
    pal=c("#386cb0","#ef3b2c","#7fc97f","#fdb462","#984ea3","#a6cee3","#778899","#fb9a99","#ffff33") #  default palette
    if (name=='PCA') {
        w=which(levels(y)=='QC') # NB returns length 0 if y is not a factor, so default colour palette
        if (length(w)!=0)
        {
            # there are QCs so add black
            pal=c('#000000',pal)
        }
    }
    discrete_scale(c("colour","fill"),"Publication",manual_pal(values = pal), drop=FALSE, name=NULL,...) # sets both fill and colour aesthetics to chosen palette.
}

SelRat=function(model,X,b) {
    # model is the output from pls::oscorespls.fit
    # X is the data used to train the model
    X=as.matrix(X)
    
    bnorm=sqrt(sum(b*b))
    # normalised regression coefficients
    wtp=b/bnorm
    wtp=matrix(wtp,nrow=ncol(X))
    
    ttp=X %*% wtp
    ptp=(t(X) %*% ttp) / sum(ttp*ttp)
    
    xtp=ttp %*% t(ptp)
    etp=X-xtp
    
    SR = colSums(xtp*xtp)/colSums(etp*etp)
    return(SR)
}