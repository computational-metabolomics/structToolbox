#' @eval get_description('plsda_feature_importance_plot')
#' @export plsda_feature_importance_plot
#' @include PLSDA_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre()+PLSDA(factor_name='Species')
#' M = model_apply(M,D)
#'
#' C = plsda_feature_importance_plot(n_features=30,metric='vip')
#' chart_plot(C,M[2])
plsda_feature_importance_plot = function(n_features=30,metric='vip',...) {
    out=struct::new_struct('plsda_feature_importance_plot',
        n_features=n_features,
        metric=metric,
        ...)
    return(out)
}

.plsda_feature_importance_plot<-setClass(
    "plsda_feature_importance_plot",
    contains=c('chart'),
    slots=c(
        n_features='entity',
        metric='enum'
    ),
    prototype = list(
        name='PLSDA feature importance summary plot',
        description='A plot of the selected feature significance metric for a PLSDA model for the top selected features.',
        type="chart",
        libraries=c('pls','ggplot2','reshape2','cowplot'),
        .params=c('n_features','metric'),
        
        n_features = entity(
            name = 'Number of features',
            description = 'The number of features to include in the summary.',
            type=c('numeric','integer'),
            value=50,
            max_length=1
        ),
        metric=enum(
            name='Metric to plot',
            description=c(
                'sr' = 'Plot Selectivity Ratio',
                'sr_pvalue'='Plot SR p-values',
                'vip'='Plot Variable Importance in Projection scores'
            ),
            type='character',
            allowed=c('sr','sr_pvalue','vip'),
            value='vip'
        )
    )
)


#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("plsda_feature_importance_plot",'PLSDA'),
    definition=function(obj,dobj) {
        
        # max vip
        if (obj$metric=='sr') {
            data=dobj$sr
            txt='Selectivity Ratio'
        } else if(obj$metric=='sr_pvalue') {
            data=1-dobj$sr_pvalue
            txt='1 - (p-value)'
        } else if(obj$metric=='vip') {
            data=dobj$vip
            txt='VIP score'
        }
        
        # max and min of reg coeff 0 and 1
        d2=dobj$reg_coeff
        #lo=apply(d2,1,min)
        hi=apply(abs(d2),1,max)
        #lo=matrix(rep(lo,ncol(d2)),nrow=nrow(d2))
        hi=matrix(rep(hi,ncol(d2)),nrow=nrow(d2))
        d2=(d2)/(hi)
            
        # max sure we dont over the max number of features
        obj$n_features=min(nrow(data),obj$n_features)
        
        max_vip=apply(data,1,max)
        data$feature_id=rownames(data)
        data$max=max_vip
        
        # sort by max
        vip_order=order(-data$max)
        data=data[vip_order,]
        
        d2=d2[vip_order,]
        d2$feature_id=rownames(d2)
        d2$max=data$max
        
        data2=reshape2::melt(d2[1:obj$n_features,seq_len(ncol(data)-1)],id.vars = 'feature_id')
        data2$max=rep(data$max[1:obj$n_features],ncol(data)-2)
        
        g1 = ggplot(data=data[1:obj$n_features,],aes_string(x='max',y='reorder(feature_id,max)')) +
            geom_point() +
            labs(x=txt,
                y="Feature") +
            structToolbox:::theme_Publication() +
            theme(
                panel.background = element_blank(),
                panel.grid.major = element_line(colour="#f0f0f0")
            )
        
        
        g2 = ggplot(data=data2,aes_string(x='variable',y='reorder(feature_id,max)')) +
            geom_tile(aes_string(fill='value'),colour = "black",width=0.8,height=0.8) +
            scale_fill_gradient2(low='#5e4fa2',mid='#ffffbf',high='#9e0142',
                midpoint=0,
                limits=c(-1,1),name='Reg. coefficient',breaks=c(-1,0,1),labels=c('-','0','+')) +
            structToolbox:::theme_Publication() +
            theme(axis.title.y=element_blank(),
                axis.text.y=element_blank()) +
            coord_fixed()+
            theme(axis.text.x = element_text(angle = 45,hjust=-0.5),axis.title.x = element_blank()) +
            scale_x_discrete(position = "top") +
            theme(legend.position='right',legend.direction = 'vertical')
        
        
        G=cowplot::plot_grid(g1,g2,nrow=1,align = 'h',axis='tb')
        
        return(G)
    }
)
