#' Autoscale
#'
#' Autoscaling centres the columns of the data in a DatasetExperiment object and divides by the standard deviation.
#' @param mode Used to control whether centring is apply to the data, the meta data or both.
#'  Can be any one of "data","sample_meta" or "both". default is "data".
#' @return A STRUCT model object with methods for autoscaling.
#'
#' @examples
#' D = iris_DatasetExperiment()
#' M = autoscale()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export autoscale
autoscale = function(mode='data',...) {
    out=struct::new_struct('autoscale',mode=mode,...)
    return(out)
}

.autoscale<-setClass(
    "autoscale",
    contains='model',
    slots=c(
        mode='enum',
        autoscaled='DatasetExperiment',
        mean_data='numeric',
        sd_data='numeric',
        mean_sample_meta='numeric',
        sd_sample_meta='numeric'
    ),
    prototype = list(
        name='Autoscaling',
        type="preprocessing",
        predicted='autoscaled',
        .params=c('mode'),
        .outputs=c('autoscaled','mean_data','sd_data','mean_sample_meta','sd_sample_meta'),
        mode=enum(name = 'Mode of action', description=c(
            '"data" will apply autoscaling to the data block',
            '"sample_meta" will apply autoscaling to the sample_meta block',
            '"both" will apply autoscaling to both the data and the sample_meta blocks'),
            value='data',
            allowed=c('data','sample_meta','both')
        )
        
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("autoscale",'DatasetExperiment'),
    definition=function(M,D)
    {
        ## xblock
        X=D$data
        m=colMeans(X,na.rm = TRUE)
        output_value(M,'mean_data')=m
        s=apply(X, 2, sd, na.rm=TRUE)
        output_value(M,'sd_data')=s
        
        
        ## yblock
        X=D$sample_meta
        # can only centre numeric variables
        nu=unlist(lapply(X,is.numeric))
        m=rep(NA,length(nu)) # put NA if not numeric
        m[nu]=colMeans(X[nu],na.rm = TRUE)
        output_value(M,'mean_sample_meta')=m
        
        s=rep(NA,length(nu)) # put NA if not numeric
        s[nu]=apply(X[nu], 2, sd, na.rm=TRUE)
        output_value(M,'sd_sample_meta')=s
        
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("autoscale",'DatasetExperiment'),
    definition=function(M,D)
    {
        # xblock
        if (M$mode %in% c('data','both')) {
            X = D$data
            Xc = ascale(X,output_value(M,'mean_data'),output_value(M,'sd_data'))
            D = DatasetExperiment(data=Xc,sample_meta=D$sample_meta,variable_meta=D$variable_meta)
        }
        if (M$mode %in% c('sample_meta','both')) {
            X=D$sample_meta
            # can only centre numeric variables
            nu=unlist(lapply(X,is.numeric))
            Xc=X
            Xc[nu]=ascale(X[nu],output_value(M,'mean_sample_meta')[nu],output_value(M,'sd_sample_meta')[nu])
            D = DatasetExperiment(data=D$data,sample_meta=Xc,variable_meta=D$variable_meta)
        }

        output_value(M,'autoscaled')=D
        return(M)
    }
)

#' @export
#' @template model_reverse
setMethod(f='model_reverse',
    signature=c('autoscale','DatasetExperiment'),
    definition = function(M,D) {
        # xblock
        if (M$mode %in% c('data','both')) {
            X=D$data
            Xc=unascale(X,output_value(M,'mean_data'),output_value(M,'sd_data'))
            D = DatasetExperiment(data=Xc,sample_meta=D$sample_meta,variable_meta=D$variable_meta)
        }
        if (M$mode %in% c('sample_meta','both')) {
            X=D$sample_meta
            # can only centre numeric variables
            nu=unlist(lapply(X,is.numeric))
            Xc=X
            Xc[nu]=unascale(X[nu],output_value(M,'mean_sample_meta')[nu],output_value(M,'sd_sample_meta')[nu])
            D = DatasetExperiment(data=D$data,sample_meta=Xc,variable_meta=D$variable_meta)
        }
        return(D)
    }
)

ascale<- function(x,m,s) {
    # http://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/
    return( (x - rep(m, rep.int(nrow(x), ncol(x)))) / rep(s, rep.int(nrow(x), ncol(x))) )
}

unascale<- function(x,m,s) {
    return( (x * rep(s, rep.int(nrow(x), ncol(x)))) + rep(m, rep.int(nrow(x), ncol(x)))) 
}