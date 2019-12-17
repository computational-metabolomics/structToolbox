#' feature_boxplot class
#'
#' plots a boxplot of a chosen feature for each group of a DatasetExperiment
#'
#' @import struct
#'
#' @param label_outliers [TRUE] or FALSE to label outliers on the plot
#' @param feature_to_plot the column id to plot.
#' @param factor_name the sample_meta column to use
#' @param show_counts [TRUE] or FALSE to include the number of samples on the
#' plot
#'
#' @examples
#' D = sbcms_DatasetExperiment
#' C = feature_boxplot(factor_name='Species',feature_to_plot='Petal.Width')
#' chart_plot(C,D)
#'
#' @export feature_boxplot
feature_boxplot = function(...) {
    out=.feature_boxplot()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.feature_boxplot<-setClass(
    "feature_boxplot",
    contains=c('chart','stato'),
    slots=c(
        # INPUTS
        params_label_outliers='entity',
        params_feature_to_plot='entity',
        params_factor_name='entity',
        params_show_counts='entity'
    ),
    prototype = list(name='Feature boxplot',
        description='plots a boxplot of a chosen feature for each group of a DatasetExperiment.',
        type="boxlot",
        stato_id='STATO:0000243',

        params_label_outliers=entity(name='Label outliers',
            value=TRUE,
            type='logical',
            description='(TRUE) of FALSE to print labels for outliers.'
        ),
        params_feature_to_plot=entity(name='Feature to plot',
            value='V1',
            type=c('character','numeric','integer'),
            description='The column name of the feature to be plotted.'
        ),
        params_factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The column name of meta data to use.'
        ),
        params_show_counts=entity(name='Show counts',
            value=TRUE,
            type='logical',
            description='(TRUE) or FALSE to display the number of values used to create each boxplot.'
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("feature_boxplot",'DatasetExperiment'),
    definition=function(obj,dobj)
    {
        # get options
        opt=param_list(obj)
        # get data
        Xt=dobj$data
        # column name
        if (is.numeric(opt$feature_to_plot)) {
            varn=colnames(Xt)[opt$feature_to_plot]
        } else {
            varn=opt$feature_to_plot
        }
        # get requested column
        Xt=Xt[[opt$feature_to_plot]]
        # meta data
        SM=dobj$sample_meta
        SM=SM[[obj$factor_name]]

        # remove NA
        SM=SM[!is.na(Xt)]
        Xt=Xt[!is.na(Xt)]

        # count number of values
        L=levels(SM)
        count=numeric(length(L))
        for (i in 1:length(L)) {
            count[i]=sum(SM==L[i])
        }

        # get color pallete using pmp
        clrs= createClassAndColors(class = SM)
        SM=clrs$class

        #prep the plot
        temp=data.frame(x=SM,y=Xt)
        p<-ggplot(temp, aes_(x=~x,y=~y,color=~x)) +
            geom_boxplot() +
            xlab(opt$factor) +
            ylab('') +
            ggtitle(varn) +
            scale_colour_manual(values=clrs$manual_colors,name=opt$factor_name) +
            theme_Publication(base_size = 12) +
            theme(legend.position="none")

        if (opt$show_counts) {
            newlabels=as.character(count)
            newlabels=paste0(as.character(L),'\n(n = ',newlabels,')')
            p=p+scale_x_discrete(labels=newlabels)
        }

        if (opt$label_outliers) {
            outliers=numeric()
            for (l in L) {
                IN=which(SM==l)
                outliers=c(outliers,IN[which( Xt[IN]>(quantile(Xt[IN], 0.75) + 1.5*IQR(Xt[IN]))) ] )
                outliers=c(outliers,IN[which( Xt[IN]<(quantile(Xt[IN], 0.25) - 1.5*IQR(Xt[IN]))) ] )
            }
            outlier_df=temp[outliers,]
            outlier_df$out_label=paste0('  ',rownames(dobj$data))[outliers]
            p=p+geom_text(data=outlier_df,aes_(group=~x,color=~x,label=~out_label),hjust='left')
        }

        return(p)

    }
)



######################################
######################################


#' mv_histogram class
#'
#' histograms indicating the numbers of missing values per sample/feature
#'
#' @param label_outliers [TRUE] or FALSE to label outliers on the plot
#' @param by_sample [TRUE] to plot by sample or FALSE to plot by features
#'
#' @examples
#' D = sbcms_DatasetExperiment()
#' C = mv_histogram(label_outliers=FALSE,by_sample=FALSE)
#' chart_plot(C,D)
#'
#' @import struct
#' @export mv_histogram
mv_histogram = function(...) {
    out=.mv_histogram()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.mv_histogram<-setClass(
    "mv_histogram",
    contains='chart',
    slots=c(
        # INPUTS
        params_label_outliers='entity',
        params_by_sample='entity'
    ),
    prototype = list(name='Missing value histogram',
        description='Histogram of missing values per sample/feature.',
        type="histogram",
        params_by_sample=entity(name='Plot by sample or by feature',
            value=TRUE,
            type='logical',
            description='(TRUE) to plot missing values per sample or FALSE to plot by feature.'
        ),
        params_label_outliers=entity(name='Label outliers on the plot',
            value=FALSE,
            type='logical',
            description='TRUE or FALSE to include labels for outlying samples. Default FALSE'
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("mv_histogram",'DatasetExperiment'),
    definition=function(obj,dobj)
    {
        # get options
        opt=param_list(obj)
        # get data
        Xt=dobj$data
        # meta data
        SM=dobj$sample_meta[ ,1]

        if (opt$by_sample)
        {
            # count NS per sample
            count=apply(Xt,1,function(x) {sum(is.na(x))/length(x)*100})
            txt='Missing values per sample'
        } else {
            # count NS per feature
            count=apply(Xt,2,function(x) {sum(is.na(x))/length(x)*100})
            txt='Missing values per feature'
        }

        A=data.frame(x=count)
        p=ggplot (data=A, aes_(x=~x)) + geom_histogram()+
            xlab ("missing values, %")+ ggtitle(txt)+
            xlim (0,100)+
            scale_colour_Publication()+ theme_Publication(base_size = 12)

        return(p)

    }
)


######################################
######################################


#' mv_boxplot class
#'
#' Boxplot of the numbers of missing values per sample/feature
#'
#' @param label_outliers [TRUE] or FALSE to label outliers on the plot
#' plot
#' @param by_sample by_sample [TRUE] to plot by sample or FALSE to plot by features
#' @param factor_name the sample_meta column to use
#' @param show_counts [TRUE] or FALSE to include the number of samples on the plot
#' @examples
#' D = sbcms_DatasetExperiment()
#' C = mv_boxplot(factor_name='class')
#' chart_plot(C,D)
#'
#' @import struct
#' @export mv_boxplot
mv_boxplot = function(...) {
    out=.mv_boxplot()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.mv_boxplot<-setClass(
    "mv_boxplot",
    contains='chart',
    slots=c(
        # INPUTS
        params_label_outliers='entity',
        params_by_sample='entity',
        params_factor_name='entity',
        params_show_counts='entity'
    ),
    prototype = list(name='Missing value boxplots',
        description='Histogram ofmissing values per sample/feature.',
        type="histogram",
        params_label_outliers=entity(name='Label outliers',
            value=TRUE,
            type='logical',
            description='(TRUE) of FALSE to print labels for outliers.'
        ),
        params_by_sample=entity(name='Plot by sample or by feature',
            value=TRUE,
            type='logical',
            description='(TRUE) to plot missing values per sample or FALSE to plot by feature.'
        ),
        params_factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The name of the factor to be displayed on the plot. Appears on axis and legend titles, for example. By default the column name of the meta data will be used where possible.'
        ),
        params_show_counts=entity(name='Show counts',
            value=TRUE,
            type='logical',
            description='(TRUE) or FALSE to display the number of values used to create each boxplot.'
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("mv_boxplot",'DatasetExperiment'),
    definition=function(obj,dobj)
    {
        # get options
        opt=param_list(obj)
        # get data
        Xt=dobj$data
        # meta data
        SM=dobj$sample_meta[ ,obj$factor_name]

        L=levels(SM)

        if (opt$by_sample)
        {
            # count NS per sample
            count=apply(Xt,1,function(x) {sum(is.na(x))/length(x)*100})
            result=matrix(0,nrow=nrow(Xt),ncol=2)
            result[,1]=count
            result[,2]=SM
            if (sum(result[,1])==0) {
                warning('No missing values were detected')
            }
            txt='Missing values per sample'
            # get color pallete using pmp
            clrs= createClassAndColors(class = SM)
            A=data.frame(x=clrs$class,y=result[,1])
        } else {
            for (i in 1:length(L)) {
                # count NS per feature per group
                count=apply(Xt[SM==L[i],,drop=FALSE],2,function(x) {sum(is.na(x))/sum(SM==L[i])*100})
                temp=data.frame(y=count,x=L[i])
                if (i==1) {
                    result=temp
                } else {
                    result=rbind(result,temp)
                }
            }
            if (sum(result$y)==0) {
                warning('No missing values were detected')
            }
            txt='Missing values per feature'
            # get color pallete using pmp
            clrs= createClassAndColors(class = as.factor(result$x))
            A=data.frame(x=clrs$class,y=result$y)
        }


        p=ggplot (data=A, aes_(x=~x,y=~y,color=~x)) +
            geom_boxplot() +
            ggtitle(txt) +
            xlab(opt$factor) +
            ylim (0,100)+
            scale_colour_manual(values=clrs$manual_colors,name=opt$factor_name) +
            theme_Publication(base_size = 12) +
            ylab ("missing values, %") +
            coord_flip()+
            theme(legend.position="none")

        if (opt$show_counts) {
            L=levels(A$x)
            num=numeric(length(L))
            for (i in 1:length(L)) {
                num[i]=sum(A$x==L[i])
            }
            newlabels=as.character(num)
            newlabels=paste0(as.character(L),'\n(n = ',newlabels,')')
            p=p+scale_x_discrete(labels=newlabels)
        }

        if (opt$label_outliers) {
            outliers=numeric()
            for (l in L) {
                IN=which(A$x==l)
                outliers=c(outliers,IN[which( A$y[IN]>(quantile(A$y[IN], 0.75) + 1.5*IQR(A$y[IN]))) ] )
                outliers=c(outliers,IN[which( A$y[IN]<(quantile(A$y[IN], 0.25) - 1.5*IQR(A$y[IN]))) ] )
            }
            outlier_df=A[outliers,]

            if (length(outliers)>0){
                if (opt$by_sample) {
                    outlier_df$out_label=paste0('  ',rownames(dobj$data))[outliers]
                } else
                {
                    outlier_df$out_label=paste0('  ',rep(colnames(dobj$data),length(L))[outliers])
                }
                p=p+geom_text(data=outlier_df,aes_(group=~x,color=~x,label=~out_label,angle =~ 90),hjust='left')
            }

        }

        return(p)
    }
)

#' Distribution plot
#'
#' Visualise distributions of values in features/samples.
#'
#' @param factor_name the sample_meta column to use
#' @param per_class = [TRUE] or FALSE to plot distrubutions for each level in
#' factor_name
#'
#' @examples
#' D = sbcms_DatasetExperiment()
#' C = DatasetExperiment.dist(factor_name='class')
#' chart_plot(C,D)
#'
#' @import struct
#' @export DatasetExperiment.dist
DatasetExperiment.dist = function(...) {
    out=.DatasetExperiment.dist()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.DatasetExperiment.dist<-setClass(
    "DatasetExperiment.dist",
    contains='chart',
    slots=c(
        # INPUTS
        params_factor_name='entity',
        params_per_class='entity'
    ),
    prototype = list(name='Distribution plot',
        description='Plot of the distribution of all values in the data matrix.',
        type="boxlot",

        params_factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The name of the factor to be displayed on the plot. Appears on axis and legend titles, for example. By default the column name of the meta data will be used where possible.'
        ),
        params_per_class=entity(name='Plot per class',
            value=TRUE,
            type='logical',
            description='[TRUE] or FALSE to plot distributions by class.'
        )
    )
)

#' @export
#' @import pmp
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("DatasetExperiment.dist",'DatasetExperiment'),
    definition=function(obj,dobj)
    {
        opt=param_list(obj)
        X=as.matrix(dobj$data)
        S=dobj$sample_meta[[opt$factor_name]]

        if (opt$per_class) {
            L=levels(S)
            for (k in L) {
                M=X[S==k,,drop=FALSE]
                if (k==L[1]) {
                    temp=data.frame(values=as.vector(M),group=k)
                } else {
                    temp=rbind(temp,data.frame(values=as.vector(M),group=k))
                }
            }
            out=ggplot(data=temp, aes_(x=~values,color=~group))+
                geom_freqpoly(bins=100)
        } else {
            temp=data.frame(values=as.vector(X),group='Sample')
            out=ggplot(data=temp, aes_(x=~values,color=~group)) +
                geom_freqpoly(bins=100,color='black')
        }
        out = out  +
            xlab('Values') +
            ylab('Density') +
            scale_colour_Publication(name=opt$factor_name)+
            theme_Publication(base_size = 12)

        return(out)

    }
)

#' DatasetExperiment boxplot
#'
#' Boxplot of values per sample/feature in a DatasetExperiment
#'
#' @param factor_name the column name of sample_meta to use
#' @param by_sample [TRUE] or FALSE to plot by samples or features respectively
#' @param per_class [TRUE] or FALSE to plot per level in factro_name
#' @param number the number of samples/features to plot
#'
#' @examples
#' D = sbcms_DatasetExperiment()
#' C = DatasetExperiment.boxplot(factor_name='class',number=10,per_class=FALSE)
#' chart_plot(C,D)
#'
#' @import struct
#' @export DatasetExperiment.boxplot
DatasetExperiment.boxplot = function(...) {
    out=.DatasetExperiment.boxplot()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.DatasetExperiment.boxplot<-setClass(
    "DatasetExperiment.boxplot",
    contains='chart',
    slots=c(
        # INPUTS
        params_factor_name='entity',
        params_by_sample='entity',
        params_per_class='entity',
        params_number='entity'
    ),
    prototype = list(name='Distribution plot',
        description='Plot of the distribution of all values in the data matrix.',
        type="boxlot",
        params_factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The name of the factor to be displayed on the plot. Appears on axis and legend titles, for example. By default the column name of the meta data will be used where possible.'
        ),
        params_by_sample=entity(name='Plot by sample or by feature',
            value=TRUE,
            type='logical',
            description='[TRUE] to plot by sample or FALSE to plot by feature.'
        ),
        params_per_class=entity(name='Plot per class',
            value=TRUE,
            type='logical',
            description='[TRUE] or FALSE to plot distributions by class.'
        ),
        params_number=entity(name='Number of features/samples',
            value=50,
            type='numeric',
            description='The number of features/samples to plot.'
        )
    )
)

#' @export
#' @import pmp
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("DatasetExperiment.boxplot",'DatasetExperiment'),
    definition=function(obj,dobj)
    {
        opt=param_list(obj)
        X=dobj$data
        SM=dobj$sample_meta
        if (!opt$by_sample) {
            s=sample(ncol(X),min(c(ncol(X),opt$number)),replace=FALSE)
            ylabel='Features'
        } else {
            s=sample(nrow(X),min(c(nrow(X),opt$number)),replace=FALSE)
            X=as.data.frame(t(X))
            colnames(X)=rownames(dobj$data)
            ylabel='Samples'
        }
        s=unique(floor(s))

        for (i in s) {
            if (!opt$by_sample){
                sm=SM[[obj$factor_name]]
                fn=row
            } else {
                sm=SM[[obj$factor_name]][i]
            }
            if (!opt$per_class) {
                sm='Sample'
            }
            if (i==s[1]) {
                temp=data.frame(value=X[,i],feature=colnames(X)[i],group=sm)
            } else {
                temp=rbind(temp,data.frame(value=X[,i],feature=colnames(X)[i],group=sm))
            }
        }
        out=ggplot(data=temp, aes_(x=~feature,y=~value,color=~group)) +
            geom_boxplot()+
            xlab(ylabel) +
            ylab('Values') +
            scale_colour_Publication(name=opt$factor_name)+
            theme_Publication(base_size = 12)+
            coord_flip() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        if  (!opt$per_class) {
            out=out+theme(legend.position="none")
        }
        return(out)

    }
)


#' Compare distributions
#'
#' A combination of plots to compare distributions of samples/features in two
#' datasets
#'
#' @param factor_name the sample_meta colum to use
#'
#' @examples
#' D1=sbcms_DatasetExperiment(filtered=FALSE)
#' D2=sbcms_DatasetExperiment(filtered=TRUE)
#' C = compare_dist(factor_name='class')
#' chart_plot(C,D1,D2)
#' @import struct
#' @export compare_dist
compare_dist = function(...) {
    out=.compare_dist()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.compare_dist<-setClass(
    "compare_dist",
    contains='chart',
    slots=c(params_factor_name='entity'),
    prototype = list(name='Compare distributions',
        description='Distributions and box plots to compare two datasets',
        type="mixed",
        params_factor_name=entity(name='Factor name',
            value='factor',
            type='character',
            description='The name of the factor to be displayed on the plot. Appears on axis and legend titles, for example.'
        )

    )
)

#' @export
#' @import gridExtra
#' @template chart_plot
#' @param eobj a second DatasetExperiment object to compare with the first
setMethod(f="chart_plot",
    signature=c("compare_dist",'DatasetExperiment'),
    definition=function(obj,dobj,eobj)
    {
        C=DatasetExperiment.boxplot(by_sample=FALSE,per_class=FALSE,number=30,factor_name=obj$factor_name)

        C1=chart_plot(C,dobj)+
            labs(tag='c)')+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(NULL,'Before processing')

        C2=chart_plot(C,eobj)+
            labs(tag='d)')+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(NULL,'After processing')

        C=DatasetExperiment.dist(factor_name=obj$factor_name,per_class=TRUE)

        C3=chart_plot(C,dobj)+
            labs(tag='a)')+
            theme(legend.position="none",axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(NULL,'Before processing')

        C4=chart_plot(C,eobj)+
            labs(tag='b)')+
            theme(legend.position="none",axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(NULL,'After processing')

        rC1=ggplot_build(C1)$layout$panel_scales_y[[1]]$range$range
        rC2=ggplot_build(C2)$layout$panel_scales_y[[1]]$range$range
        rC3=ggplot_build(C3)$layout$panel_scales_x[[1]]$range$range
        rC4=ggplot_build(C4)$layout$panel_scales_x[[1]]$range$range
        rC1=max(abs(rC1))
        rC2=max(abs(rC2))
        rC3=max(abs(rC3))
        rC4=max(abs(rC4))

        C1=C1+ylim(c(-rC1,rC1))
        C3=C3+xlim(c(-rC1,rC1))
        C2=C2+ylim(c(-rC2,rC2))
        C4=C4+xlim(c(-rC2,rC2))


        gA <- ggplotGrob(C3)
        gB <- ggplotGrob(C4)
        gC <- ggplotGrob(C1)
        gD <- ggplotGrob(C2)

        L=matrix(nrow=4,ncol=1)
        L=as.matrix(rbind(c(1,2),c(3,4),c(3,4),c(3,4)))

        temp=gtable_cbind(gA,gB)
        heights1=temp$heights
        gA$heights=heights1
        gB$heights=heights1

        temp=gtable_cbind(gC,gD)
        heights2=temp$heights
        gC$heights=heights2
        gD$heights=heights2

        maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5],gC$widths[2:5],gD$widths[2:5])
        gA$widths[2:5] <- as.list(maxWidth)
        gB$widths[2:5] <- as.list(maxWidth)
        gC$widths[2:5] <- as.list(maxWidth)
        gD$widths[2:5] <- as.list(maxWidth)

        p=grid.arrange(grobs=list(gA,gB,gC,gD),layout_matrix=L)

        return(p)

    }
)


#############################################################
#############################################################

#' DatasetExperiment.heatmap class
#'
#' plots a DatasetExperiment as a heatmap
#'
#' @import struct
#' @import reshape2
#' @export DatasetExperiment.heatmap
#' @examples
#' C = DatasetExperiment.heatmap()
DatasetExperiment.heatmap = function(...) {
    out=.DatasetExperiment.heatmap()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.DatasetExperiment.heatmap<-setClass(
    "DatasetExperiment.heatmap",
    contains=c('chart'),
    slots=c(
        # INPUTS
        params_na_colour='entity'
    ),
    prototype = list(name='DatasetExperiment heatmap',
        description='plots a heatmap of a DatasetExperiment',
        type="scatter",

        params_na_colour=entity(name='NA colour',
            value='#FF00E4',
            type='character',
            description='A hex colour code to use for missing values'
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("DatasetExperiment.heatmap",'DatasetExperiment'),
    definition=function(obj,dobj)
    {
        X=melt(as.matrix(dobj$data))
        colnames(X)=c('Sample','Feature','Peak area')
        p=ggplot(data=X,aes(x=`Feature`,y=`Sample`,fill=`Peak area`)) + geom_raster() +
            scale_colour_Publication()+
            theme_Publication(base_size = 12)+
            scale_fill_viridis_c(na.value=obj$na_colour)+
            theme(axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
            )


        return(p)

    }
)
