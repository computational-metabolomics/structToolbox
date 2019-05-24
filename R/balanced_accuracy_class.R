#' Balanced Accuracy class
#'
#' Balanced Accuracy.
#' @export balanced_accuracy

balanced_accuracy<-setClass(
    "balanced_accuracy",
    contains='metric',
    prototype = list(name='Balanced Accuracy',
        type="classification"
    )
)

#' @export
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


