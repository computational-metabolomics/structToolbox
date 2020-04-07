#' Area under ROC
#'
#' Area under ROC calculated by approximating the curve as a series of trapeziums.
#' Only suitable for 2 classes.
#' 
#' @return A metric object with methods for calculating AUC
#'
#' @examples
#' D = iris_DatasetExperiment()
#' XCV = kfold_xval(folds=5,factor_name='Species') *
#'       (mean_centre() + PLSDA(number_components=2,factor_name='Species'))
#' MET = AUC()
#' XCV = run(XCV,D,MET)
#'
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export AUC
AUC = function(...) {
    out=struct::new_struct('AUC',...)
    return(out)
}


.AUC<-setClass(
    "AUC",
    contains='metric',
    prototype = list(name='Area under ROC',
        type="classification"
    )
)

#' @export
#' @template calculate
setMethod(f="calculate",
    signature=c('AUC'),
    definition=function(obj,Y,Yhat)
    {
        threshold=sort(unique(c(0,Yhat,1)))
        
        Y=as.numeric(Y)
        Y[Y==2]=-1
        
        sn=numeric()
        sp=numeric()
        for (k in threshold) {
            pred=as.numeric(Yhat>=k)
            tp=sum(pred==1 & Y==1)
            fp=sum(pred==1 & Y==-1)
            tn=sum(pred==0 & Y==-1)
            fn=sum(pred==0 & Y==1)
            
            sn=c(sn,tp/(tp+fn))
            sp=c(sp,tn/(tn+fp))
        }
        
        A=data.frame(Sensitivity=sn,Specificity=1-sp)
        
        A=A[order(A$Specificity,A$Sensitivity),]
        
        AUC=0
        # approximate as trapeziums
        for (k in seq(from=2,to=length(threshold),by=1)) {
            h=A$Specificity[k]-(A$Specificity[k-1])
            a=A$Sensitivity[k]
            b=A$Sensitivity[k-1]
            AUC=AUC+(h*((a+b)/2))
        }
        obj@value=AUC
        
        return(obj)
    }
)