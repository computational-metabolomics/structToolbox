#' PLS fold change
#'
#' Partial least squares (PLS) fold change estimates
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export PLSFC
#' @include fold_change_class.R
#' @examples
#' C = PLSFC()
PLSFC = function(...) {
    out=.PLSFC()
    out=struct::new_struct(out,...)
    return(out)
}


.PLSFC<-setClass(
    "PLSFC",
    contains='fold_change',
    slots=c(
        number_components='entity'
    ),
    prototype = list(name='Partial least squares discriminant analysis',
        type="classification",
        predicted='pred',
        libraries=c('pls'),
        number_components=entity(value = 2,name = 'Number of PLS components',description = 'The number of PLS components to use',type = 'numeric')
    )
)

#' @param ... additional slots and values passed to struct_class
#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("PLSFC",'DatasetExperiment'),
    definition=function(M,D) {
        # log transform
        X=log2(D$data)
        Y=D$sample_meta

        D$data=X
        D$sample_meta=Y

        # levels for factor of interest
        L=levels(as.factor(Y[[M$factor_name]]))
        # put control group first if provided
        if (length(M$control_group)>0) {
            w=which(L==M$control_group)
            if (length(w)>1) {
                L=c(L[w],L[-w])
            }
        }

        # number of pairwise comparisons
        n=((length(L)^2)-length(L))/2

        # prep some matrices
        FC=matrix(0,nrow=ncol(X),ncol=n)

        comp=character(0)

        counter=1

        D$sample_meta[[M$factor_name]]=ordered(D$sample_meta[[M$factor_name]])
        BHAT=matrix(0,nrow=n,ncol=ncol(X))
        SE=BHAT
        # for all pairs of groups
        for (A in 1:(length(L)-1)) {
            for (B in (A+1):(length(L))) {
                # filter groups to A and B
                FG=filter_smeta(factor_name=M$factor_name,mode='include',levels=L[c(A,B)])
                FG=model_apply(FG,D)
                # change to ordered factor so that we make use of control group
                FG$filtered$sample_meta[[M$factor_name]]=ordered(FG$filtered$sample_meta[[M$factor_name]],levels=L[c(A,B)])

                # mean_centred PCA
                MC=mean_centre()+PCA(number_components=min(c(nrow(X)-1,ncol(X))))
                MC=model_train(MC,FG$filtered)
                MC=model_predict(MC,FG$filtered)
                S=DatasetExperiment(data=MC[2]$scores,sample_meta=FG$filtered$sample_meta)

                # R2 for each X and y component
                r2=matrix(0,nrow=ncol(MC[2]$scores),ncol=1)
                k=1
                # pls
                P=PLSDA(factor_name=M$factor_name,number_components=M$number_components)
                P=model_train(P,S)
                P=model_predict(P,S)
                sy=P$design_matrix[,1]
                sx=as.matrix(MC[2]$scores)

                # get regression coefficients
                b=P$reg_coeff[,1]
                for (j in 1:ncol(sx)) {
                    yhat=sx[,j]*b[j]
                    SSe=sum((sy-yhat)^2)
                    SSt=sum(sy^2)
                    r2[j,k]=1-(SSe/SSt)
                }
                b=as.matrix(b)

                # invert
                bhat=r2/b
                # reverse pca
                Vx=as.matrix(MC[2]$loadings)
                bhat=t(bhat)%*%t(Vx)

                # guestimate confidence intervals
                x=MC[1]$centred$data
                xhat=sy%*%(bhat)
                ssx=(x-xhat)^2
                ssx=apply(ssx,2,sum) # for each column of x
                ssy=sum(sy^2) # y_bar is zero by design
                SE[counter,]=(sqrt(ssx)/(sqrt(ssy)*sqrt((nrow(x)-2))))*qt(0.975,nrow(x)-2)*2


                BHAT[counter,]=bhat*2

                counter=counter+1
                comp=c(comp,paste0(L[A],'/',L[B]))


            }
        }

        FC=as.data.frame(t(BHAT))
        SE=as.data.frame(t(SE))
        colnames(FC)=comp
        colnames(SE)=comp

        rownames(FC)=colnames(D$data)
        rownames(SE)=colnames(D$data)

        M$fold_change=2^FC
        M$upper_ci=2^(FC+SE)
        M$lower_ci=2^(FC-SE)

        return(M)
    }
)

