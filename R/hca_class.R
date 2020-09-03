#' HCA method class
#'
#' HCA method class. Calculate a hierarchical clustering for the input data.
#'
#' @param dist_method The distance method to use for clustering. Can be any one of
#' "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". Default
#' is "euclidean".
#' @param cluster_method The clustering method to use. Can be any one of "ward.D",
#' "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".
#' Default is 'complete'.
#' @param minkowski_power This parameter is only used when \code{dist_method = 'minkowski'}.
#' @param factor_name The sample_meta column to use.
#' @param ... additional slots and values passed to struct_class
#'
#' @return struct object
#' @export HCA
#' @examples
#' D = iris_DatasetExperiment()
#' M = HCA(factor_name='Species')
#' M = model_apply(M,D)
#'
HCA = function(dist_method='euclidean',cluster_method='complete',minkowski_power=2,factor_name,...) {
    out=struct::new_struct('HCA',
        dist_method=dist_method,
        cluster_method=cluster_method,
        minkowski_power=minkowski_power,
        factor_name=factor_name,
        ...)
    return(out)
}


.HCA<-setClass(
    "HCA",
    contains=c('model'),
    slots=c(
        # INPUTS
        dist_method='enum',
        cluster_method='enum',
        minkowski_power='numeric',
        factor_name='character',
        # OUTPUTS
        dist_matrix='entity',
        hclust='entity',
        factor_df='data.frame'
    ),
    prototype = list(name='Hierarchical Cluster Analysis',
        description='Applies hierarchical clustering to a DatasetExperiment.',
        type="univariate",
        predicted='dist_matrix',
        .params=c('dist_method','cluster_method','minkowski_power','factor_name'),
        .outputs=c('dist_matrix','hclust','factor_df'),


        dist_method=enum(name='Distance method',
            value='euclidean',
            type='character',
            description='The distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"',
            allowed=c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
        ),
        cluster_method=enum(name='Clustering method',
            value='complete',
            type='character',
            description='The agglomeration method to be used. This should be one of "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid"',
            allowed=c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
        ),

        dist_matrix=entity(name='distance structure',
            type='dist',
            description='An object containing pairwise distance information between samples'
        ),
        hclust=entity(name='clustering object',
            type='hclust',
            description='An object of class hclust which describes the tree produced by the clustering process'
        )
    )
)

#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("HCA",'DatasetExperiment'),
    definition=function(M,D)
    {

        M$dist_matrix=dist(D$data, method = M$dist_method, diag = FALSE, upper = FALSE, p = M$minkowski_power)

        M$hclust=hclust(M$dist_matrix, method = M$cluster_method, members = NULL)

        df=D$sample_meta[,M$factor_name,drop=FALSE]
        df$orig_order=1:nrow(df)
        df$label=rownames(D$data)
        df$order=M$hclust$order
        M$factor_df=df
        return(M)
    }
)



#' hca_dendrogram class
#'
#' plots a dendrogram for HCA
#'
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export hca_dendrogram
#' @include hca_class.R
#' @examples
#' C = hca_dendrogram()
hca_dendrogram = function(...) {
    out=struct::new_struct('hca_dendrogram',...)
    return(out)
}


.hca_dendrogram<-setClass(
    "hca_dendrogram",
    contains='chart',
    prototype = list(libraries='ggdendro')
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("hca_dendrogram",'HCA'),
    definition=function(obj,dobj)
    {
        hcdata=ggdendro::dendro_data(dobj$hclust)

        A=ggdendro::label(hcdata)

        A=A[order(dobj$factor_df$order),,drop=FALSE]
        dobj$factor_df[order(dobj$factor_df$order),,drop=FALSE]
        A$group=dobj$factor_df[,1]

        g= ggplot() +
            geom_segment(data=ggdendro::segment(hcdata), aes(x=x, y=y, xend=xend, yend=yend)) +
            geom_point(data=A, aes(x=x, y=y,color=group))+
            scale_colour_Publication() +
            theme_Publication(base_size = 12) +
            labs(color = colnames(dobj$factor_df)[1]) +
            theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank()) +
            ylab('dissimilarity')

        return(g)
    }
)





