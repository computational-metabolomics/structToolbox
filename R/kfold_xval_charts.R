#' kfoldxcv_grid class
#'
#' plot of cross validation results
#'
#' @import struct
#' @export kfoldxcv_grid
#' @include kfold_xval_class.R
kfoldxcv_grid<-setClass(
    "kfoldxcv_grid",
    contains='chart',
    slots=c(
        # INPUTS
        params.factor_name='entity'
    ),
    prototype = list(name='kfoldxcv grid plot',
        description='plots the predictions for each cross-validation loop',
        type="grid",
        params.factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The name of the factor to be displayed on the plot. Appears on axis and legend titles, for example. By default the column name of the meta data will be used where possible.'
        )

    )
)

#' @export
setMethod(f="chart.plot",
    signature=c("kfoldxcv_grid",'kfold_xval'),
    definition=function(obj,dobj)
    {
        # get options
        copt=param.list(obj)
        dopt=param.list(dobj)
        X=output.value(dobj,'results')
        L=levels(as.factor(X$actual))
        plotClass= createClassAndColors(X$actual)
        X$actual=plotClass$class

        p=list()
        for (i in 1:length(L)) {
            # get data
            X=output.value(dobj,'results')

            # reduce to level i for split factor
            X=X[X$actual==L[i],,drop=FALSE]

            X2=X[,-2]
            X=X[,-1]
            X$Set='Predicted'
            X2$Set='True'
            colnames(X)[1]='Group'
            colnames(X2)[1]='Group'
            X=rbind(X,X2)

            uS=unique(as.character(X$sampleid))
            uL=L

            # for test set boxes
            X2=X
            X2$sampleid[!X$in.test]=NA
            X2$fold[!X$in.test]=NA

            p[[i]]=ggplot(data=X,aes_(x=~sampleid,y=~fold,fill=~Group)) +
                geom_tile(colour = "grey50") +
                #geom_point(data=te,aes_(x=~sampleid,y=~fold),shape=20,size=2) +
                geom_tile(data=X2,aes_(x=~sampleid,y=~fold,fill=~Group),colour = "black") +
                scale_x_discrete(limits=uS) +
                theme_Publication(base_size = 12) +
                coord_equal() +
                theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
                scale_fill_manual(values=plotClass$manual_colors,drop=FALSE,name=copt$factor_name) +
                facet_grid(rows=vars(X$Set))
        }

        return(p)

    }
)



###################

#' kfoldxcv_metric class
#'
#' box plot of cross validation results
#'
#' @import struct
#' @export kfoldxcv_metric
#' @include kfold_xval_class.R
kfoldxcv_metric<-setClass(
    "kfoldxcv_metric",
    contains='chart',
    prototype = list(name='kfoldxcv metric plot',
        description='box plot of the metric for each cross-validation loop',
        type="boxplot"

    )
)

#' @export
setMethod(f="chart.plot",
    signature=c("kfoldxcv_metric",'kfold_xval'),
    definition=function(obj,dobj)
    {
        # get options
        dopt=param.list(dobj)

        # get data
        X=data.frame('Metric'=output.value(dobj,'metric.train'),'Set'='Training')
        X2=data.frame('Metric'=output.value(dobj,'metric.test'),'Set'='Test')
        X=rbind(X,X2)
        X$Set=as.factor(X$Set)
        plotClass= createClassAndColors(X$Set)

        p=ggplot(data=X,aes_(y=~Metric,x=~Set,colour=~Set)) +
            geom_boxplot() +
            theme_Publication(base_size = 12) +
            scale_colour_manual(values=plotClass$manual_colors,name='Crossvalidation set') +
            xlab('Crossvalidation set') + theme(legend.position="none")



        return(p)

    }
)




