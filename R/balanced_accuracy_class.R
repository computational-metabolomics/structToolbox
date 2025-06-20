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
    prototype = list(
        name='Balanced Accuracy',
        type="classification",
        description=paste0(
            'Balanced Accuracy is the average proportion of correctly ',
            'identified samples within each class.')
    )
)

#' @export
#' @template calculate
setMethod(f="calculate",
          signature=c('balanced_accuracy'),
          definition=function(obj,Y,Yhat) {
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
                  CE[i] = TP / (TP + FN)
              }
              CE=mean(CE)
              obj@value=CE
              return(obj)
          }
)


#' @eval get_description('balanced_error')
#' @examples
#' D = iris_DatasetExperiment()
#' XCV = kfold_xval(folds=5,factor_name='Species') *
#'       (mean_centre() + PLSDA(number_components=2,factor_name='Species'))
#' MET = balanced_error()
#' XCV = run(XCV,D,MET)
#' @export balanced_error
balanced_error = function(...) {
    out=struct::new_struct('balanced_error',...)
    return(out)
}


.balanced_error<-setClass(
    "balanced_error",
    contains='metric',
    prototype = list(
        name='Balanced error',
        type="classification",
        description=paste0(
            'Balanced Accuracy is the average proportion of correctly ',
            'identified samples within each class. Balanced error is ',
            '1 - Balanced Accuracy.')
    )
)

#' @export
#' @template calculate
setMethod(f="calculate",
          signature=c('balanced_error'),
          definition=function(obj,Y,Yhat)
          {
              BA = balanced_accuracy()
              BA = calculate(BA,Y,Yhat)
              obj@value=1-value(BA)
              return(obj)
          }
)
