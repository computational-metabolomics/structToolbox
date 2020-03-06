#' mean_centre model class
#'
#' Mean centres the columns of a DatasetExperiment object. Can also centre the meta
#' data for e.g regression models, if required.
#'
#' @param mode Used to control whether centring is apply to the data, the meta data or both.
#'  Can be any one of "data","sample_meta" or "both". default is "data".
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export mean_centre
#' @examples
#' M = mean_centre()
mean_centre = function(mode='data',...) {
    out=struct::new_struct('mean_centre',
        mode=mode,
        ...)
    return(out)
}


.mean_centre<-setClass(
    "mean_centre",
    contains='preprocess',
    slots=c(mode='enum',
        centred='DatasetExperiment',
        mean_data='numeric',
        mean_sample_meta='numeric'
    ),
    prototype = list(name='Mean centre',
        type="preprocessing",
        mode=enum(name = 'Mode of action', description=c(
            '"data" will apply centring to data block',
            '"sample_meta" will apply centring to the sample_meta block',
            '"both" will apply centring to both the data and the sample_meta blocks'),
            value='data',
            allowed=c('data','sample_meta','both')
        ),
        predicted='centred',
        .params=c('mode'),
        .outputs=c('centred','mean_data','mean_sample_meta')
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("mean_centre",'DatasetExperiment'),
    definition=function(M,D)
    {
        ## xblock
        X=D$data
        m=colMeans(X,na.rm = TRUE)
        output_value(M,'mean_data')=m

        ## yblock
        X=D$sample_meta
        # can only centre numeric variables
        nu=unlist(lapply(X,is.numeric))
        m=rep(NA,length(nu)) # put NA if not numeric
        m[nu]=colMeans(X[nu],na.rm = TRUE)
        output_value(M,'mean_sample_meta')=m

        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("mean_centre",'DatasetExperiment'),
    definition=function(M,D)
    {
        # xblock
        if (M$mode %in% c('data','both')) {
            X=D$data
            Xc=center_colmeans(X,output_value(M,'mean_data'))
            D = DatasetExperiment(data=Xc,sample_meta=D$sample_meta,variable_meta=D$variable_meta)
        }
        if (M$mode %in% c('sample_meta','both')) {
            X=D$sample_meta
            # can only centre numeric variables
            nu=unlist(lapply(X,is.numeric))
            Xc=X
            Xc[nu]=center_colmeans(X[nu],output_value(M,'mean_sample_meta')[nu])
            D = DatasetExperiment(data=D$data,sample_meta=Xc,variable_meta=D$variable_meta)
        }
        output_value(M,'centred')=D
        return(M)
    }
)

#' @export
#' @template model_reverse
setMethod(f='model_reverse',
    signature=c('mean_centre','DatasetExperiment'),
    definition = function(M,D) {
        # xblock
        if (M$mode %in% c('data','both')) {
            X=D$data
            Xc=uncenter_colmeans(X,output_value(M,'mean_data'))
            D$data=as.data.frame(Xc)
        }
        if (M$mode %in% c('sample_meta','both')) {
            X=D$sample_meta
            # can only centre numeric variables
            nu=unlist(lapply(X,is.numeric))
            Xc=X
            Xc[nu]=uncenter_colmeans(X[nu],output_value(M,'mean_sample_meta')[nu])
            D$sample_meta=as.data.frame(Xc)
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

