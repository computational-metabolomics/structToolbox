#' @eval get_description('balanced_accuracy')
#' @examples
#' D = iris_DatasetExperiment()
#' XCV = kfold_xval(folds=5,factor_name='Species') *
#'       (mean_centre() + PLSDA(number_components=2,factor_name='Species'))
#' MET = balanced_accuracy()
#' XCV = run(XCV,D,MET)
#' @export balanced_accuracy
balanced_accuracy = function(...) {
    out=struct::new_struct('balanced_accuracy',...)
    return(out)
}


.balanced_accuracy<-setClass(
    "balanced_accuracy",
    contains='metric',
    prototype = list(name='Balanced Accuracy',
        type="classification",
        description=paste0('Balanced Accuracy is the average proportion of ',
        'correctly classified samples across all groups.')
    )
)

#' @export
#' @template calculate
setMethod(f="calculate",
    signature=c('balanced_accuracy'),
    definition=function(obj,Y,Yhat)
    {
        Y_F=Y
        lvls=levels(Y_F)
        result=Yhat
        CE=rep(0,length(lvls))
        for (i in 1:length(lvls))
        {
            TP=sum(Y_F==lvls[i] & result==lvls[i])
            FP=sum(Y_F!=lvls[i] & result==lvls[i])
            TN=sum(Y_F!=lvls[i] & result!=lvls[i])
            FN=sum(Y_F==lvls[i] & result!=lvls[i])
            TPR=TP/(TP+FN)
            TNR=TN/(TN+FP)
            CE[i]=(TPR+TNR)/2
        }
        CE=1-CE
        CE=mean(CE)
        obj@value=CE
        return(obj)
    }
)


