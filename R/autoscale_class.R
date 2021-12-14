#' @eval get_description('autoscale')
#' @examples
#' D = iris_DatasetExperiment()
#' M = autoscale()
#' M = model_train(M,D)
#' M = model_predict(M,D)
#' @export autoscale
autoscale = function(mode='data',...) {
    out=struct::new_struct('autoscale',mode=mode,...)
    return(out)
}

.autoscale<-setClass(
    "autoscale",
    contains=c('model'),
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
        description=paste0(
            'Each variable/feature is mean centred and scaled by the standard ',
            'deviation. The transformed variables have zero-mean and ',
            'unit-variance.'
        ),
        type="preprocessing",
        predicted='autoscaled',
        .params=c('mode'),
        .outputs=c('autoscaled','mean_data','sd_data','mean_sample_meta','sd_sample_meta'),
        mode=enum(name = 'Mode of action', description=c(
            "data" = 'Autoscaling is applied to the data matrix only.',
            "sample_meta" = 'Autoscaling is applied to the sample_meta data only.',
            "both" =  'Autoscaling is applied to both the data matrix and the meta data.'),
            value='data',
            allowed=c('data','sample_meta','both')
        ),
        ontology='OBI:0200149'
        
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