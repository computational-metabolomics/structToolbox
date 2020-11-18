#' @eval get_description('feature_profile')
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' C = feature_profile(run_order='sample_order',
#'     qc_label='QC',
#'     qc_column='class',
#'     colour_by='class',
#'     feature_to_plot=1)
#' chart_plot(C,D)
#' @export feature_profile
feature_profile_array = function(run_order,qc_label,qc_column,colour_by,feature_to_plot,nrow=5,log=TRUE,...) {
    out=struct::new_struct('feature_profile_array',
        run_order=run_order,
        qc_label=qc_label,
        qc_column=qc_column,
        colour_by=colour_by,
        feature_to_plot=feature_to_plot,
        nrow=nrow,
        log=log,
        ...)
    return(out)
}


.feature_profile_array<-setClass(
    "feature_profile_array",
    contains=c('chart'),
    slots=c(
        # INPUTS
        run_order='entity',
        qc_label='entity',
        qc_column='entity',
        colour_by='entity',
        feature_to_plot='entity',
        nrow='entity',
        log='entity'
    ),
    prototype = list(name='Feature profile',
        description=paste0('A plot visualising the change in intensity of a ',
            'feature with a continuous variable such as time, dose, or run order.'),
        type="scatter",
        .params=c('run_order','qc_label','qc_column','colour_by','feature_to_plot','nrow','log'),
        nrow = entity(
            name='Number of rows',
            description = 'The number of rows in the plot.',
            type=c('numeric','integer'),
            max_length=1,
            value=1
        ),
        run_order=entity(name='run order',
            description='The sample-meta column name containing run order.',
            type='character',
            max_length=1),
        qc_label = entity(name='QC label',
            description = 'The label used to identify QC samples',
            type='character',
            max_length=1),
        qc_column = entity(name='QC column',
            description='The sample-meta column name containing the labels used to identify QC samples.',
            type='character',
            max_length=1),
        colour_by=entity(name = 'Factor to colour by',
            description='The sample-meta column name to used to colour the plot.',
            type='character',
            max_length=1
        ),
        feature_to_plot=entity(name='Feature to plot',
            description='The name or column id of the plotted feature.',
            type=c('numeric','character','integer')
        ),
        log=entity(
            name='Log transform',
            description=c(
                "TRUE"='The data is log tranformed before plotting.',
                "FALSE"='The data is not transformed before plotting.'
            ),
            type='logical',
            value=TRUE,
            max_length=1
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("feature_profile_array",'DatasetExperiment'),
    definition=function(obj,dobj) {
        
        groups=createClassAndColors(class = dobj$sample_meta[[obj$colour_by]],
            QC_label=obj$qc_label)
        
        nm=obj$feature_to_plot
        if (!is(obj$feature_to_plot,'character')) {
            nm=colnames(dobj)[obj$feature_to_plot]
            obj$feature_to_plot=nm
        }
        
        ylabel='Intensity'
        if (obj$log) {
            dobj$data=log10(DE$data)
            ylabel='log10(intensity)'
        }
        
        # ggplot data frame
        X=data.frame(feature=(dobj$data[,obj$feature_to_plot[1]]),
            run_order=dobj$sample_meta[[obj$run_order]],
            group=groups$class,
            feature_id=obj$feature_to_plot[1]
        )
        
        if (length(obj$feature_to_plot)>1) {
            for (k in 2:length(obj$feature_to_plot)){
                X2=data.frame(feature=(dobj$data[,obj$feature_to_plot[k]]),
                    run_order=dobj$sample_meta[[obj$run_order]],
                    group=groups$class,
                    feature_id=obj$feature_to_plot[k]
                )
                X=rbind(X,X2)
            }
        }
        

        
        g=ggplot(X,aes(x=run_order,y=feature,colour=group)) +
            geom_point()+
            theme_Publication(base_size = 12) +
            scale_colour_manual(values=groups$manual_colors,name=obj$colour_by)+
            ylab(ylabel) +
            xlab('Run order')+
            facet_wrap("feature_id",nrow=obj$nrow)
        return(g)
    }
    
)
