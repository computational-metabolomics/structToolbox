#' mean_centre model class
#'
#' Mean centres the columns a dataset object
#' @export mean_centre
#' @examples
#' M = mean_centre()
mean_centre<-setClass(
    "mean_centre",
    contains='preprocess',
    slots=c(params.mode='enum',
        outputs.centred='dataset',
        outputs.mean_data='numeric',
        outputs.mean_sample_meta='numeric'
    ),
    prototype = list(name='Mean centre',
        type="preprocessing",
        params.mode=enum(name = 'Mode of action', description=c(
            '"data" will apply centring to data block',
            '"sample_meta" will apply centring to the sample_meta block',
            '"both" will apply centring to both the data and the sample_meta blocks'),
            value='data',
            list=c('data','sample_meta','both')
        ),
        predicted='centred'
    )
)

#' @export
#' @template model_train
setMethod(f="model.train",
    signature=c("mean_centre",'dataset'),
    definition=function(M,D)
    {
        ## xblock
        X=dataset.data(D)
        m=colMeans(X,na.rm = TRUE)
        output.value(M,'mean_data')=m

        ## yblock
        X=dataset.sample_meta(D)
        # can only centre numeric variables
        nu=unlist(lapply(X,is.numeric))
        m=rep(NA,length(nu)) # put NA if not numeric
        m[nu]=colMeans(X[nu],na.rm = TRUE)
        output.value(M,'mean_sample_meta')=m

        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model.predict",
    signature=c("mean_centre",'dataset'),
    definition=function(M,D)
    {
        # xblock
        if (M$mode %in% c('data','both')) {
            X=dataset.data(D)
            Xc=center_colmeans(X,output.value(M,'mean_data'))
            dataset.data(D)=as.data.frame(Xc)
        }
        if (M$mode %in% c('sample_meta','both')) {
            X=dataset.sample_meta(D)
            # can only centre numeric variables
            nu=unlist(lapply(X,is.numeric))
            Xc=X
            Xc[nu]=center_colmeans(X[nu],output.value(M,'mean_sample_meta')[nu])
            dataset.sample_meta(D)=as.data.frame(Xc)
        }
        output.value(M,'centred')=D
        return(M)
    }
)

#' @export
#' @template model_reverse
setMethod(f='model.reverse',
    signature=c('mean_centre','dataset'),
    definition = function(M,D) {
        # xblock
        if (M$mode %in% c('data','both')) {
            X=dataset.data(D)
            Xc=uncenter_colmeans(X,output.value(M,'mean_data'))
            dataset.data(D)=as.data.frame(Xc)
        }
        if (M$mode %in% c('sample_meta','both')) {
            X=dataset.sample_meta(D)
            # can only centre numeric variables
            nu=unlist(lapply(X,is.numeric))
            Xc=X
            Xc[nu]=uncenter_colmeans(X[nu],output.value(M,'mean_sample_meta')[nu])
            dataset.sample_meta(D)=as.data.frame(Xc)
        }
        return(D)
    }
)

center_colmeans <- function(x,m) {
    # http://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/
    return(x - rep(m, rep.int(nrow(x), ncol(x))))
}

uncenter_colmeans <- function(x,m) {
    return(x + rep(m, rep.int(nrow(x), ncol(x))))
}

