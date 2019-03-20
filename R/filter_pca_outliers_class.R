#' find PCA outliers
#'
#' returns a list of PCA outliers
#' @export filter_pca_outliers

filter_pca_outliers<-setClass(
    "filter_pca_outliers",
    contains = c('method'),
    slots=c(params.conf='entity',
            params.mode='entity',
            params.factor_name='entity',
            params.npc = 'entity',
            params.pca_model='PCA',
            outputs.filtered='dataset',
            outputs.distance='data.frame',
            outputs.threshold='numeric'
    ),
    prototype=list(type = 'filter',

        predicted = 'filtered',

        params.mode=enum(value='confidence_ellipse',
            name='Filter mode',
            description = '"confidence_ellipse" excludes samples outside the
                ellipse calculated for all points regardless of group.
                "group_ellipse" excludes samples outside the group ellipses.',
            type='character',
            value = "confidence_ellipse"),

        params.conf=entity(name = 'Confidence',
                                description = 'The confidence limit for
            rejecting outliers',
            type = 'numeric',
            value = 0.95),

        params.factor_name=entity(name = 'Factor name',
            description = 'The same of the sample_meta column to use',
            type='character'),

        params.npc=entity(name='Number of Principal Components',
            description='The number of principal components to use for
            calculating the confidence ellipse',value=2,type='numeric')


    )
)

#' @export
setMethod(f="method.apply",
    signature=c("filter_pca_outliers","dataset"),
    definition=function(M,D)
    {
        P=M$pca_model
        A=M$npc

        if (M$mode=='confidence_ellipse') {

            # mahal dist
            C=cov(P$scores[,1:A])
            M=apply(P$scores[,1:A],2,mean)
            d2=mahalanobis(P$scores[,1:A],center = M,cov=C)
            d2=data.frame(distance=d2)
            rownames(d2)=rownames(D$data)

            # threshold
            I = nrow(D$data)
            t = (A*(I-1)*(I+1)) / (I*(I-A))
            t=t*qf(M$conf, df1 = A, df2 = I-A)
            d2$outlier=d2$distance>t

            out=rownames(d2)[d2$outlier]

            FF=filter_by_name(mode='exclude',dimension='sample',names=out)
            FF=method.apply(FF,D)
            M$threshold=t
            M$distance=d2
            M$filtered=FF$filtered
        }
        return(M)
    }
)



