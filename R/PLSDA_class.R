#' PLSDA model class
#'
#' Partial least squares (PLS) discriminant analysis (DA) model class. This object can be used to train/apply PLS models.
#' @export PLSDA
#' @importFrom pls plsr scores
PLSDA<-setClass(
  "PLSDA",
  contains='model',
  slots=c(params.number_components='entity',
          params.factor_name='entity',
          outputs.scores='data.frame',
          outputs.loadings='data.frame',
          outputs.yhat='data.frame',
          outputs.design_matrix='data.frame',
          outputs.y='data.frame',
          outputs.reg_coeff='data.frame',
          outputs.probability='data.frame',
          outputs.vip='data.frame',
          outputs.pls_model='list',
          outputs.pred='data.frame',
          outputs.threshold='numeric'

  ),
  prototype = list(name='Partial least squares discriminant analysis',
    type="classification",
    predicted='pred',
    params.number_components=entity(value = 2,name = 'Number of PLS components',description = 'The number of PLS components to use',type = 'numeric'),
    params.factor_name=entity(value = 'V1',name = 'Name of sample_meta column',description = 'The name of the sample_meta column to use for the PLS models',type = 'character')
  )
)

#' @export
setMethod(f="model.train",
          signature=c("PLSDA",'dataset'),
          definition=function(M,D)
          {
            SM=dataset.sample_meta(D)
            y=as.matrix(SM[[M$factor_name]])
            # convert the factor to a design matrix
            z=model.matrix(~y+0)
            z[z==0]=-1 # +/-1 for PLS
            X=as.matrix(dataset.data(D)) # convert X to matrix

            Z=as.data.frame(z)
            output.value(M,'design_matrix')=Z
            output.value(M,'y')=dataset.sample_meta(D)

            pls_model=pls::plsr(z ~ X,validation="none",method='oscorespls',model=TRUE,ncomp=param.value(M,'number_components'),
              center=FALSE,
              scale=FALSE)
            ny=ncol(z)
            nr=ncol(output.value(M,'reg_coeff'))
            output.value(M,'reg_coeff')=as.data.frame(pls_model$coefficients[,,M$number_components]) # keep only the requested number of components
            output.value(M,'vip')=as.data.frame(vips(pls_model))
            yhat=predict(pls_model, ncomp = param.value(M,'number_components'), newdata = X)
            yhat=yhat[,,dim(yhat)[3]]
            output.value(M,'yhat')=as.data.frame(yhat)
            probs=prob(yhat,yhat,dataset.sample_meta(D))
            output.value(M,'probability')=as.data.frame(probs$ingroup)
            output.value(M,'threshold')=probs$threshold
            output.value(M,'pls_model')=list(pls_model)
            scores=pls::scores(pls_model)
            output.value(M,'scores')=as.data.frame(matrix(scores,nrow = nrow(scores),ncol=ncol(scores)))
            return(M)
          }
)

#' @export
setMethod(f="model.predict",
          signature=c("PLSDA",'dataset'),
          definition=function(M,D)
          {
            # convert X to matrix
            X=as.matrix(dataset.data(D))
            # get training set y vector
            SM=dataset.sample_meta(D)
            y=as.matrix(SM[[M$factor_name]])
            # get predictions
            p=predict(output.value(M,'pls_model')[[1]], ncomp = param.value(M,'number_components'), newdata = X)
            p=p[,,dim(p)[3]]
            if (!is.matrix(p)){
              p=t(as.matrix(p)) # in case of leave one out p is a numeric instead of a matrix
            }

            ## probability estimate
            # http://www.eigenvector.com/faq/index.php?id=38%7C
            d=prob(x=p,yhat=output.value(M,'yhat'),ytrue=y)
            pred=(p>d$threshold)*1
            pred=apply(pred,MARGIN=1,FUN=which.max)
            hi=apply(d$ingroup,MARGIN=1,FUN=which.max) # max probability
            if (sum(is.na(pred)>0))
            {
              pred[is.na(pred)]=hi[is.na(pred)] # if none above threshold, use group with highest probability
            }
            pred=factor(pred,levels=1:length(levels(SM[[M$factor_name]])),labels=levels(SM[[M$factor_name]])) # make sure pred has all the levels of y
            q=data.frame("pred"=pred)
            output.value(M,'pred')=q
            return(M)
          }
)


prob=function(x,yhat,ytrue)
{
  # x is predicted values
  # yhat is training model
  # ytrue are real group labels
  ytrue=as.factor(ytrue[,1])
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
    t=Re(t) # only take real part
    if (length(t)>1)
    {
      # multiple cross over points so choose the one closest to 0
      t=t[which.min(abs(t))]
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
  return(polyroot(c(c,b,a))) # in reverse order vs numpy.roots
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
