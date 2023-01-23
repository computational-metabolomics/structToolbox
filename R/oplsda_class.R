#' @eval get_description('OPLSDA')
#' @export OPLSDA
#' @include oplsr_class.R
#' @examples
#' M = OPLSR('number_components'=2,factor_name='Species')
OPLSDA = function(number_components=1,factor_name,...) {
    out=struct::new_struct('OPLSDA',
        number_components = number_components,
        factor_name=factor_name,
        ...)
    return(out)
}

.OPLSDA<-setClass(
    "OPLSDA",
    contains=c('OPLSR'),
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("OPLSDA",'DatasetExperiment'),
    definition=function(M,D) {
        
        SM = D$sample_meta
        y = SM[[M$factor_name]]
        # convert the factor to a design matrix
        z=model.matrix(~y+0)
        z[z==0]=-1 # +/-1 for PLS
        
        X=as.matrix(D$data) # convert X to matrix
        
        Z=as.data.frame(z)
        colnames(Z)=as.character(interaction('PLSDA',1:ncol(Z),sep='_'))
        
        D$sample_meta=cbind(D$sample_meta,Z)
        
        # OPLSR model
        N = OPLSR(number_components=M$number_components,factor_name=colnames(Z))
        N = model_apply(N,D)
        
        # copy outputs across
        output_list(M) = output_list(N)
        
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("OPLSDA",'DatasetExperiment'),
    definition=function(M,D) {
            # call OPLSR predict
            N=callNextMethod(M,D)
            # copy outputs across
            output_list(M) = output_list(N)
            return(M)
    }
)
