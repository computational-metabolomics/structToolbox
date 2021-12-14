#' @eval get_description('feature_boxplot')
#' @examples
#' D = MTBLS79_DatasetExperiment
#' C = feature_boxplot(factor_name='Species',feature_to_plot='Petal.Width')
#' chart_plot(C,D)
#' @export feature_boxplot
feature_boxplot = function(label_outliers=TRUE,feature_to_plot,
    factor_name,show_counts=TRUE,style='boxplot',jitter=FALSE,fill=FALSE,...) {
    out=struct::new_struct('feature_boxplot',
        label_outliers=label_outliers,
        feature_to_plot=feature_to_plot,
        factor_name=factor_name,
        show_counts=show_counts,
        style=style,
        jitter=jitter,
        fill=fill,
        ...)
    return(out)
}


.feature_boxplot<-setClass(
    "feature_boxplot",
    contains=c('chart'),
    slots=c(
        # INPUTS
        label_outliers='entity',
        feature_to_plot='entity',
        factor_name='entity',
        show_counts='entity',
        style='enum',
        jitter='entity',
        fill='entity'
    ),
    prototype = list(name='Feature boxplot',
        description='A boxplot to visualise the distribution of values within a feature.',
        type="boxlot",
        ontology='STATO:0000243',
        .params=c('label_outliers','feature_to_plot','factor_name','show_counts','style','jitter','fill'),
        
        label_outliers=entity(name='Label outliers',
            value=TRUE,
            type='logical',
            description=c(
                'TRUE' = 'The index for outlier samples is included on the plot.',
                'FALSE' = 'No labels are displayed.'
            )
        ),
        feature_to_plot=entity(name='Feature to plot',
            value='V1',
            type=c('character','numeric','integer'),
            description='The column name of the plotted feature.'
        ),
        factor_name=ents$factor_name,
        show_counts=ents$show_counts,
        style=enum(
            name='Plot style',
            description=c(
                'boxplot' = 'boxplot style',
                'violin' = 'violon plot style'
            ),
            allowed=c('boxplot','violin'),
            value='boxplot',
            max_length=1,
            type='character'
        ),
        jitter=entity(
            name = 'Jitter',
            description = 'Include points plotted with added jitter.',
            value=FALSE,
            type='logical',
            max_length=1
        ),
        fill=entity(
            name = 'Fill',
            description = 'Block fill the boxes or violins with the group colour.',
            value=FALSE,
            type='logical',
            max_length=1
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
        names(SM)=rownames(dobj)
        
        # remove NA
        SM=SM[!is.na(Xt)]
        Xt=Xt[!is.na(Xt)]
        
        # get color pallete using pmp
        clrs= createClassAndColors(class = SM)
        SM=clrs$class
        
        # count number of values
        L=levels(SM)
        count=numeric(length(L))
        for (i in 1:length(L)) {
            count[i]=sum(SM==L[i])
        }
        
        #prep the plot
        temp=data.frame(x=SM,y=Xt,row.names=names(SM))
        
        if (obj$fill) {
            A = aes_string(x='x',y='y',color='x',fill='x')
        } else {
            A = aes_string(x='x',y='y',color='x')
        }

        
        p<-ggplot(temp, A) 
        
        if (obj$style=='boxplot') {
            p=p+geom_boxplot(alpha=0.3)
        } else {
            p=p+geom_violin(alpha=0.3, draw_quantiles = c(0.25, 0.5, 0.75), 
                            scale = "width", adjust = .5)
        }
        
        if (obj$jitter) {
            p=p+geom_jitter(show.legend = F, width = 0.1)
        }
            p=p+xlab(opt$factor) +
            ylab('') +
            ggtitle(varn) +
            scale_colour_manual(values=clrs$manual_colors,name=opt$factor_name)
            
            p=p+scale_fill_manual(values=clrs$manual_colors,name=opt$factor_name)

            p=p+theme_Publication(base_size = 12) +
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
            outlier_df$out_label=paste0('  ',rownames(temp))[outliers]
            p=p+geom_text(data=outlier_df,aes_(group=~x,color=~x,label=~out_label),hjust='left')
        }
        
        return(p)
        
    }
)



######################################
######################################


#' @eval get_description('mv_histogram')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' C = mv_histogram(label_outliers=FALSE,by_sample=FALSE)
#' chart_plot(C,D)
#'
#' @import struct
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export mv_histogram
mv_histogram = function(label_outliers=TRUE,by_sample=TRUE,...) {
    out=struct::new_struct('mv_histogram',
        label_outliers=label_outliers,
        by_sample=by_sample,
        ...)
    return(out)
}


.mv_histogram<-setClass(
    "mv_histogram",
    contains='chart',
    slots=c(
        # INPUTS
        label_outliers='entity',
        by_sample='entity'
    ),
    prototype = list(name='Missing value histogram',
        description='A histogram of the numbers of missing values per sample/feature',
        type="histogram",
        .params=c('label_outliers','by_sample'),
        
        by_sample=ents$by_sample,
        label_outliers=ents$label_outliers
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
        
        if (opt$by_sample) {
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


#' @eval get_description('mv_boxplot')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' C = mv_boxplot(factor_name='Class')
#' chart_plot(C,D)
#'
#' @import struct
#' @export mv_boxplot
mv_boxplot = function(label_outliers=TRUE,by_sample=TRUE,factor_name,show_counts=TRUE,...) {
    out=struct::new_struct('mv_boxplot',
        label_outliers=label_outliers,
        by_sample=by_sample,
        factor_name=factor_name,
        show_counts=show_counts,
        ...)
    return(out)
}


.mv_boxplot<-setClass(
    "mv_boxplot",
    contains='chart',
    slots=c(
        # INPUTS
        label_outliers='entity',
        by_sample='entity',
        factor_name='entity',
        show_counts='entity'
    ),
    prototype = list(name='Missing value boxplots',
        description='Boxplots of the number of missing values per sample/feature.',
        type="boxplot",
        .params=c('label_outliers','by_sample','factor_name','show_counts'),
        
        by_sample=ents$by_sample,
        label_outliers=ents$label_outliers,
        factor_name=ents$factor_name,
        show_counts=ents$show_counts
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("mv_boxplot",'DatasetExperiment'),
    definition=function(obj,dobj) {
        # get options
        opt=param_list(obj)
        # get data
        Xt=dobj$data
        # meta data
        SM=dobj$sample_meta[ ,obj$factor_name]
        
        L=levels(SM)
        
        if (opt$by_sample) {
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

#' @eval get_description('DatasetExperiment_dist')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' C = DatasetExperiment_dist(factor_name='Class')
#' chart_plot(C,D)
#' @import struct
#' @export DatasetExperiment_dist
DatasetExperiment_dist = function(factor_name,per_class=TRUE,...) {
    out=struct::new_struct('DatasetExperiment_dist',
        factor_name=factor_name,
        per_class=per_class,
        ...)
    return(out)
}

.DatasetExperiment_dist<-setClass(
    "DatasetExperiment_dist",
    contains='chart',
    slots=c(
        # INPUTS
        factor_name='entity',
        per_class='entity'
    ),
    prototype = list(name='Feature distribution histogram',
        description=paste0('A histogram to visualise the distribution of ',
            'values within features.'),
        type="histogram",
        .params=c('factor_name','per_class'),
        
        factor_name=ents$factor_name,
        per_class=entity(name='Plot per class',
            value=TRUE,
            type='logical',
            description=c(
                "TRUE" = 'The distributions are plotted for each class.',
                "FALSE" = 'The distribution is plotted for all samples')
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("DatasetExperiment_dist",'DatasetExperiment'),
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

#' @eval get_description('DatasetExperiment_boxplot')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' C = DatasetExperiment_boxplot(factor_name='Class',number=10,per_class=FALSE)
#' chart_plot(C,D)
#' @return struct object
#' @export DatasetExperiment_boxplot
DatasetExperiment_boxplot = function(factor_name,by_sample=TRUE,per_class=TRUE,number=50,...) {
    out=struct::new_struct('DatasetExperiment_boxplot',
        factor_name=factor_name,
        by_sample=by_sample,
        per_class=per_class,
        number=number,
        ...)
    return(out)
}


.DatasetExperiment_boxplot<-setClass(
    "DatasetExperiment_boxplot",
    contains='chart',
    slots=c(
        # INPUTS
        factor_name='entity',
        by_sample='entity',
        per_class='entity',
        number='entity'
    ),
    prototype = list(name='Feature distribution histogram',
        description=paste0('A boxplot to visualise the distribution of ',
            'values within a subset of features.'),
        type="boxplot",
        .params=c('factor_name','by_sample','per_class','number'),
        
        factor_name=ents$factor_name,
        by_sample=entity(name='Plot by sample',
            value=TRUE,
            type='logical',
            description=c(
                'TRUE' = 'The data is plotted across features for a subset of samples.',
                'FALSE' = 'The data is plotted across samples for a subset of features.')
        ),
        per_class=entity(name='Plot per class',
            value=TRUE,
            type='logical',
            description=c(
                "TRUE" = 'The data is plotted for each class.',
                "FALSE" = 'The data is plotted for all samples')
        ),
        number=entity(name='Number of features/samples',
            value=50,
            type=c('numeric','integer'),
            description='The number of features/samples plotted.',
            max_length=1
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("DatasetExperiment_boxplot",'DatasetExperiment'),
    definition=function(obj,dobj)
    {
        opt=param_list(obj)
        X=dobj$data
        SM=dobj$sample_meta
        if (!opt$by_sample) {
            #s=sample(ncol(X),min(c(ncol(X),opt$number)),replace=FALSE)
            s=seq(from=1,to=ncol(X),length.out = min(c(opt$number,ncol(X))))
            ylabel='Features'
        } else {
            #s=sample(nrow(X),min(c(nrow(X),opt$number)),replace=FALSE)
            s=seq(from=1,to=nrow(X),length.out = min(c(opt$number,nrow(X))))
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


#' @eval get_description('compare_dist')
#' @examples
#' D1=MTBLS79_DatasetExperiment(filtered=FALSE)
#' D2=MTBLS79_DatasetExperiment(filtered=TRUE)
#' C = compare_dist(factor_name='Class')
#' chart_plot(C,D1,D2)
#' @import struct
#' @export compare_dist
compare_dist = function(factor_name,...) {
    out=struct::new_struct('compare_dist',
        factor_name=factor_name,
        ...)
    return(out)
}

.compare_dist<-setClass(
    "compare_dist",
    contains=c('chart'),
    slots=c(factor_name='entity'),
    prototype = list(name='Compare distributions',
        description=paste0('Histograms and boxplots computed across samples ',
            'and features are used to visually compare two datasets e.g. before ',
            'and after filtering and/or normalisation.'),
        type="mixed",
        ontology='STATO:0000161',
        .params=c('factor_name'),
        factor_name=ents$factor_name
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
        
        # match features across datasets
        inboth=intersect(colnames(dobj),colnames(eobj))
        dobj=dobj[,inboth]
        eobj=eobj[,inboth]
        
        C=DatasetExperiment_boxplot(by_sample=FALSE,per_class=FALSE,number=30,factor_name=obj$factor_name)
        
        C1=chart_plot(C,dobj)+
            labs(tag='c)')+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(NULL,'Before processing')
        
        C2=chart_plot(C,eobj)+
            labs(tag='d)')+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(NULL,'After processing')
        
        C=DatasetExperiment_dist(factor_name=obj$factor_name,per_class=TRUE)
        
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

#' @eval get_description('DatasetExperiment_heatmap')
#' @export DatasetExperiment_heatmap
#' @examples
#' D = iris_DatasetExperiment()
#' C = DatasetExperiment_heatmap()
#' chart_plot(C,D)
DatasetExperiment_heatmap = function(na_colour='#FF00E4',...) {
    out=struct::new_struct('DatasetExperiment_heatmap',
        na_colour=na_colour,...)
    return(out)
}


.DatasetExperiment_heatmap<-setClass(
    "DatasetExperiment_heatmap",
    contains=c('chart'),
    slots=c(
        # INPUTS
        na_colour='entity'
    ),
    prototype = list(name='DatasetExperiment heatmap',
        description='A heatmap to visualise the measured values in a data matrix.',
        type="scatter",
        libraries='reshape2',
        .params=c('na_colour'),
        
        na_colour=entity(name='Missing value colour',
            value='#FF00E4',
            type='character',
            description='The hex colour code used to plot missing values.',
            max_length=1
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("DatasetExperiment_heatmap",'DatasetExperiment'),
    definition=function(obj,dobj)
    {
        X=reshape2::melt(as.matrix(dobj$data))
        colnames(X)=c('Sample','Feature','peak_area')
        X$Feature=as.character(X$Feature)
        X$Sample=as.character(X$Sample)
        p=ggplot(data=X,aes_string(x='Feature',y='Sample',fill='peak_area')) + geom_raster() +
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
