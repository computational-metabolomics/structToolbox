#' PLSR model class
#'
#' Partial least squares (PLS) Regression model class. This object can be used to train/apply PLS models.
#' @export PLSR
#' @importFrom pls plsr scores
PLSR<-setClass(

    "PLSR",
    contains='model',

    slots=c(
        params.number_components = 'entity',
        params.factor_name       = 'entity',
        outputs.scores           = 'data.frame',
        outputs.loadings         = 'data.frame',
        outputs.yhat             = 'data.frame',
        outputs.y                = 'data.frame',
        outputs.reg_coeff        = 'data.frame',
        outputs.vip              = 'data.frame',
        outputs.pls_model        = 'list',
        outputs.pred             = 'data.frame'
    ),

    prototype = list(name='Partial least squares regression',
        type="regression",
        predicted='pred',
        params.number_components=entity(value = 2,name = 'Number of PLS components',description = 'The number of PLS components to use',type = 'numeric'),
        params.factor_name=entity(name='Factor name', description='A vector of sample_meta column names to use')
    )

)

#' @export
setMethod(f="model.train",
    signature=c("PLSR",'dataset'),
    definition=function(M,D)
    {
        y=dataset.sample_meta(D)
        y=y[,M$factor_name] # might be multiple columns (?)

        output.value(M,'y')=as.data.frame(y)

        X=as.matrix(dataset.data(D)) # convert X to matrix
        y=as.matrix(y)
        #print(param.value(M,'number_components'))
        pls_model=pls::plsr(y ~ X,validation="none",
            method='oscorespls',
            model=TRUE,
            ncomp=param.value(M,'number_components'),
            center=FALSE,
            scale=FALSE)
        ny=ncol(y)
        nr=ncol(output.value(M,'reg_coeff'))
        output.value(M,'reg_coeff')=as.data.frame(output.value(M,'reg_coeff')[,(nr-ny+1):nr]) # keep only the requested number of components
        output.value(M,'vip')=as.data.frame(vips(pls_model))
        yhat=predict(pls_model, ncomp = param.value(M,'number_components'), newdata = X)
        yhat=yhat[,,dim(yhat)[3]]
        output.value(M,'yhat')=as.data.frame(yhat)

        output.value(M,'pls_model')=list(pls_model)
        scores=pls::scores(pls_model)
        output.value(M,'scores')=as.data.frame(matrix(scores,nrow = nrow(scores),ncol=ncol(scores)))
        return(M)
    }
)

#' @export
setMethod(f="model.predict",
    signature=c("PLSR",'dataset'),
    definition=function(M,D)
    {
        # convert X to matrix
        X=as.matrix(dataset.data(D))
        # get training set y
        y=output.value(M,'y')

        # get predictions
        p=predict(output.value(M,'pls_model')[[1]], ncomp = param.value(M,'number_components'), newdata = X)
        p=p[,,dim(p)[3]]

        q=data.frame("pred"=p)
        output.value(M,'pred')=q
        return(M)
    }
)



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

#' plsr_prediction_plot class
#'
#' plots the true values against the predicted values for a PLSR model.
#'
#' @import struct
#' @export plsr_prediction_plot
#' @include PLSR_class.R
plsr_prediction_plot<-setClass(
    "plsr_prediction_plot",
    contains='chart',
    prototype = list(name='PLSR prediction plot',
        description='plots the true values against the predicted values for a PLSR model.',
        type="scatterplot"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("plsr_prediction_plot",'PLSR'),
    definition=function(obj,dobj)
    {
        R2=r_squared()
        R2=calculate(R2,dobj$y[,1],dobj$yhat[,1])

        B=data.frame(x=rep(dobj$y[,1],2),y=c(dobj$y[,1],dobj$yhat[,1]),group=rep(1:nrow(dobj$y),2))
        A=data.frame(x=dobj$y[,1],y=dobj$yhat[,1])
        p=ggplot(data=A,aes(x=x,y=y)) +
            geom_line(data=B,aes(x=x,y=y,group=group),color='#B2B2B2') +
            geom_point(color="blue") +
            geom_abline(slope=1,intercept=0,color="red")+
            scale_colour_Publication() +
            theme_Publication(base_size = 12) +
            xlab('True values') +
            ylab('Predicted values') +


            annotate("text",x=-Inf,y=Inf,label=paste0('R^2 == ',format(value(R2),digits = 2)),vjust=2,hjust=-1,parse=TRUE)
        return(p)
    }
)

#' plsr_residual_hist class
#'
#' plots a histogram of the residuals
#'
#' @import struct
#' @export plsr_residual_hist
#' @include PLSR_class.R
plsr_residual_hist<-setClass(
    "plsr_residual_hist",
    contains='chart',
    prototype = list(name='PLSR residuals histogram',
        description='Plots a histogram of the residuals for a PLSR model.',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("plsr_residual_hist",'PLSR'),
    definition=function(obj,dobj)
    {
        d=dobj$y[,1]-dobj$yhat[,1]
        bw=0.2
        n=length(d)
        nx=seq(min(d),max(d),length.out = 100)
        nc=dnorm(nx,0,sd(d))*n*bw

        B=data.frame(nx,nc)
        A=data.frame(y=d)
        p=ggplot(data=A,aes(x=y)) +
            geom_histogram(color="#10C8CD",fill="#b2e9eb",binwidth=0.2) +
            geom_line(data=B,aes(x=nx,y=nc),color="blue") +
            scale_colour_Publication() +
            theme_Publication(base_size = 12) +
            xlab('Residual')+
            ylab('Count')

        return(p)
    }
)

#' plsr_qq_plot class
#'
#' plots the quantiles of PLSR residuals against the qualtiles of a normal
#' distribution
#'
#' @import struct
#' @export plsr_qq_plot
#' @include PLSR_class.R
plsr_qq_plot<-setClass(
    "plsr_qq_plot",
    contains='chart',
    prototype = list(name='PLSR QQ plot',
        description='plots the quantiles of PLSR residuals against the qualtiles of a normal
    distribution',
        type="scatter"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("plsr_qq_plot",'PLSR'),
    definition=function(obj,dobj)
    {

        x=sort(dobj$y[,1]-dobj$yhat[,1])
        p=seq(0,1,length.out = length(x))
        q=qnorm(p,0,sd(x))
        A=data.frame('x'=q,'y'=x)
        p=ggplot(data=A,aes(x=x,y=y)) +
            geom_point(color="blue")+
            geom_abline(slope=1,intercept=0,color="red")+

            scale_colour_Publication() +
            theme_Publication(base_size = 12) +
            xlab('Theoretical quantiles')+
            ylab('Residuals quantiles')

        return(p)
    }
)

#' plsr_residual_plot class
#'
#' Plots the residuals for a PLSR model
#'
#' @import struct
#' @export plsr_cook_dist
#' @include PLSR_class.R
plsr_cook_dist<-setClass(
    "plsr_cook_dist",
    contains='chart',
    prototype = list(name='Cook distance barchart',
        description='Plots Cook\'s distance for each sample of a PLSR model',
        type="barchart"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("plsr_cook_dist",'PLSR'),
    definition=function(obj,dobj)
    {

        # residual
        e=as.matrix(dobj$y-dobj$yhat)^2 # e^2

        # leverage
        T=as.matrix(dobj$scores)
        H=T %*% solve(t(T) %*% T) %*% t(T)
        H=diag(H)
        s2=(1/(nrow(T)-ncol(T))) * sum(e)

        # cook distance
        CD=(e/(s2*ncol(T)))*(H/((1-H)^2))

        A=data.frame(x=seq_len(length(CD)),y=CD)
        p=ggplot(data=A,aes(x=x,y=y)) +
            geom_col(width=1,color="black",fill='#B2B2B2')+
            scale_colour_Publication() +
            theme_Publication(base_size = 12) +
            xlab('Sample number')+
            ylab('Cook\'s distance')+
            geom_hline(yintercept=4/(nrow(T)-ncol(T)),color='red')


        return(p)
    }
)
