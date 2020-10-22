#' @eval get_description('PLSR')
#' @export PLSR
#' @examples
#' M = PLSR(factor_name='run_order')
PLSR = function(number_components=2,factor_name,...) {
    out=struct::new_struct('PLSR',
        number_components=number_components,
        factor_name=factor_name,
        ...)
    return(out)
}

.PLSR<-setClass(

    "PLSR",
    contains=c('model','stato'),

    slots=c(
        number_components = 'entity',
        factor_name       = 'entity',
        scores           = 'data.frame',
        loadings         = 'data.frame',
        yhat             = 'data.frame',
        y                = 'data.frame',
        reg_coeff        = 'data.frame',
        vip              = 'data.frame',
        pls_model        = 'list',
        pred             = 'data.frame'
    ),

    prototype = list(name='Partial least squares regression',
        type="regression",
        predicted='pred',
        libraries='pls',
        stato_id='STATO:0000571',
        description=paste0('PLS is a multivariate regression technique that ',
        'extracts latent variables maximising covariance between the input ',
        'data and the response. For regression the response is a continuous ',
        'variable.'),
        .params=c('number_components','factor_name'),
        .outputs=c(
            'scores',
            'loadings',
            'y',
            'yhat',
            'reg_coeff',
            'vip',
            'pls_model',
            'pred'),

        number_components=entity(
            value = 2,
            name = 'Number of PLS components',
            description = 'The number of PLS components'
            ,type = c('numeric','integer')
            ),
        factor_name=ents$factor_name
    )

)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("PLSR",'DatasetExperiment'),
    definition=function(M,D)
    {
        y=D$sample_meta
        y=y[,M$factor_name] # might be multiple columns (?)

        output_value(M,'y')=as.data.frame(y)

        X=as.matrix(D$data) # convert X to matrix
        y=as.matrix(y)
        #print(param_value(M,'number_components'))
        pls_model=pls::plsr(y ~ X,validation="none",
            method='oscorespls',
            model=TRUE,
            ncomp=param_value(M,'number_components'),
            center=FALSE,
            scale=FALSE)
        ny=ncol(y)
        nr=ncol(output_value(M,'reg_coeff'))
        output_value(M,'reg_coeff')=as.data.frame(output_value(M,'reg_coeff')[,(nr-ny+1):nr]) # keep only the requested number of components
        output_value(M,'vip')=as.data.frame(vips(pls_model))
        yhat=predict(pls_model, ncomp = param_value(M,'number_components'), newdata = X)
        yhat=yhat[,,dim(yhat)[3]]
        output_value(M,'yhat')=as.data.frame(yhat)

        output_value(M,'pls_model')=list(pls_model)
        scores=pls::scores(pls_model)
        output_value(M,'scores')=as.data.frame(matrix(scores,nrow = nrow(scores),ncol=ncol(scores)))
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("PLSR",'DatasetExperiment'),
    definition=function(M,D)
    {
        # convert X to matrix
        X=as.matrix(D$data)
        # get training set y
        y=output_value(M,'y')

        # get predictions
        p=predict(output_value(M,'pls_model')[[1]], ncomp = param_value(M,'number_components'), newdata = X)
        p=p[,,dim(p)[3]]

        q=data.frame("pred"=p)
        output_value(M,'pred')=q
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

#' @eval get_description('plsr_prediction_plot')
#' @export plsr_prediction_plot
#' @include PLSR_class.R
#' @examples
#' C = plsr_prediction_plot()
plsr_prediction_plot = function(...) {
    out=struct::new_struct('plsr_prediction_plot',...)
    return(out)
}

.plsr_prediction_plot<-setClass(
    "plsr_prediction_plot",
    contains='chart',
    prototype = list(name='PLSR prediction plot',
        description='A scatter plot of the true response values against the predicted values for a PLSR model.',
        type="scatterplot"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
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

#' @eval get_description('plsr_residual_hist')
#' @export plsr_residual_hist
#' @include PLSR_class.R
#' @examples
#' C = plsr_residual_hist()
plsr_residual_hist = function(...) {
    out=struct::new_struct('plsr_residual_hist',...)
    return(out)
}

.plsr_residual_hist<-setClass(
    "plsr_residual_hist",
    contains='chart',
    prototype = list(name='PLSR residuals histogram',
        description='A histogram of the residuals for a PLSR model.',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
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

#' @eval get_description('plsr_qq_plot')
#' @import struct
#' @export plsr_qq_plot
#' @include PLSR_class.R
#' @examples
#' C = plsr_qq_plot()
plsr_qq_plot = function(...) {
    out=struct::new_struct('plsr_qq_plot',...)
    return(out)
}


.plsr_qq_plot<-setClass(
    "plsr_qq_plot",
    contains=c('chart','stato'),
    prototype = list(
        name='PLSR QQ plot',
        description=paste0('A plot of the quantiles of the residuals from a ',
        'PLSR model against the quantiles of a normal distribution.'),
        type="scatter",
        stato_id='STATO:0000241'
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
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

#' @eval get_description('plsr_cook_dist')
#' @import struct
#' @export plsr_cook_dist
#' @include PLSR_class.R
#' @examples
#' C = plsr_cook_dist()
plsr_cook_dist = function(...) {
    out=struct::new_struct('plsr_cook_dist',...)
    return(out)
}

.plsr_cook_dist<-setClass(
    "plsr_cook_dist",
    contains='chart',
    prototype = list(name="Cook's distance barchart",
        description=paste0("A barchart of Cook’s distance for each sample ",
        "used to train a PLSR model. Cook's distance is used to estimate the ",
        "influence of a sample on the model and can be used to identify ",
        "potential outliers."),
        type="barchart"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("plsr_cook_dist",'PLSR'),
    definition=function(obj,dobj) {
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
