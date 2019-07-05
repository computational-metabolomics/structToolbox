#' HCA method class
#'
#' HCA method class. Calculate a hierarchical clustering for the input data
#'
#' @export HCA
#' @examples
#' M = HCA()
HCA<-setClass(
    "HCA",
    contains=c('method'),
    slots=c(
        # INPUTS
        params.dist_method='enum',
        params.cluster_method='enum',
        params.minkowski_power='numeric',
        params.factor_name='character',
        # OUTPUTS
        outputs.dist_matrix='entity',
        outputs.hclust='entity',
        outputs.factor_df='data.frame'
    ),
    prototype = list(name='Hierarchical Cluster Analysis',
        description='Applies hierarchical clustering to a dataset.',
        type="univariate",
        predicted='dist_matrix',


        params.dist_method=enum(name='Distance method',
            value='euclidean',
            type='character',
            description='The distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"',
            list=c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
        ),
        params.cluster_method=enum(name='Clustering method',
            value='complete',
            type='character',
            description='The agglomeration method to be used. This should be one of "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid"',
            list=c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
        ),

        outputs.dist_matrix=entity(name='distance structure',
            type='dist',
            description='An object containing pairwise distance information between samples'
        ),
        outputs.hclust=entity(name='clustering object',
            type='hclust',
            description='An object of class hclust which describes the tree produced by the clustering process'
        )
    )
)

#' @export
setMethod(f="method.apply",
    signature=c("HCA",'dataset'),
    definition=function(M,D)
    {

        M$dist_matrix=dist(D$data, method = M$dist_method, diag = FALSE, upper = FALSE, p = M$minkowski_power)

        M$hclust=hclust(M$dist_matrix, method = M$cluster_method, members = NULL)

        df=D$sample_meta[,M$factor_name,drop=FALSE]
        df$orig_order=1:nrow(df)
        df$label=rownames(D$data)
        M$factor_df=df
        return(M)
    }
)



#' hca_dendrogram class
#'
#' plots a dendrogram for HCA
#'
#' @export hca_dendrogram
#' @import ggdendro
#' @include hca_class.R
#' @examples
#' C = hca_dendrogram()
hca_dendrogram<-setClass(
    "hca_dendrogram",
    contains='chart'
)

#' @export
setMethod(f="chart.plot",
    signature=c("hca_dendrogram",'HCA'),
    definition=function(obj,dobj)
    {
        hcdata=dendro_data(dobj$hclust)

        A=label(hcdata)

        A=A[order(dobj$factor_df$label),,drop=FALSE]
        dobj$factor_df[order(dobj$factor_df$label),,drop=FALSE]
        A$group=dobj$factor_df[,1]

        g= ggplot() +
            geom_segment(data=segment(hcdata), aes(x=x, y=y, xend=xend, yend=yend)) +
            geom_point(data=A, aes(x=x, y=y,color=group))+
            structToolbox:::scale_colour_Publication() +
            structToolbox:::theme_Publication(base_size = 12) +
            labs(color = colnames(dobj$factor_df)[1]) +
            theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank()) +
            ylab('dissimilarity')

        return(g)
    }
)





