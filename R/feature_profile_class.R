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
feature_profile = function(run_order,qc_label,qc_column,colour_by,feature_to_plot,...) {
    out=struct::new_struct('feature_profile',
        run_order=run_order,
        qc_label=qc_label,
        qc_column=qc_column,
        colour_by=colour_by,
        feature_to_plot=feature_to_plot,
        ...)
    return(out)
}


.feature_profile<-setClass(
    "feature_profile",
    contains=c('chart'),
    slots=c(
        # INPUTS
        run_order='entity',
        qc_label='entity',
        qc_column='entity',
        colour_by='entity',
        feature_to_plot='entity'
    ),
    prototype = list(name='Feature profile',
        description=paste0('A plot visualising the change in intensity of a ',
        'feature with a continuous variable such as time, dose, or run order.'),
        type="scatter",
        .params=c('run_order','qc_label','qc_column','colour_by','feature_to_plot'),
        
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
        )
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("feature_profile",'DatasetExperiment'),
    definition=function(obj,dobj) {
        
        groups=createClassAndColors(class = dobj$sample_meta[[obj$colour_by]],
            QC_label=obj$qc_label)
        
        # ggplot data frame
        X=data.frame(feature=dobj$data[,obj$feature_to_plot],
            run_order=dobj$sample_meta[[obj$run_order]],
            group=groups$class
        )
        
        # mean of QCs
        FT=filter_smeta(mode='include',levels=obj$qc_label,factor_name=obj$qc_column)
        FT=model_apply(FT,dobj)
        MQC=mean(predicted(FT)$data[,obj$feature_to_plot],na.rm=TRUE)
        SQC=sd(predicted(FT)$data[,obj$feature_to_plot],na.rm=TRUE)
        # mean of samples
        FT=filter_smeta(mode='exclude',levels=obj$qc_label,factor_name=obj$qc_column)
        FT=model_apply(FT,dobj)
        MS=mean(predicted(FT)$data[,obj$feature_to_plot],na.rm=TRUE)
        SS=sd(predicted(FT)$data[,obj$feature_to_plot],na.rm=TRUE)
        
        nm=obj$feature_to_plot
        if (!is(obj$feature_to_plot,'character')) {
            nm=colnames(dobj)[obj$feature_to_plot]
        }
        
        g=ggplot(X,aes(x=run_order,y=feature,colour=group)) +
            geom_point()+
            geom_hline(yintercept = MQC,colour='grey') +
            geom_hline(yintercept = MQC+(2*SQC),colour='grey',linetype=2) +
            geom_hline(yintercept = MQC-(2*SQC),colour='grey',linetype=2) +
            geom_hline(yintercept = MS,colour='skyblue') +
            geom_hline(yintercept = MS+(2*SS),colour='skyblue',linetype=2) +
            geom_hline(yintercept = MS-(2*SS),colour='skyblue',linetype=2) +
            theme_Publication(base_size = 12) +
            scale_colour_manual(values=groups$manual_colors,name=obj$colour_by)+
            ylab('log10 peak area') +
            xlab('Run order')+
            #annotate("text",x=Inf,y=MQC,label=' mean(QC)',vjust='center',hjust='left')+
            annotate("text",x=Inf,y=MQC+(2*SQC),label='+2SD(QC)',vjust='center',hjust='left')+
            annotate("text",x=Inf,y=MQC-(2*SQC),label='-2SD(QC)',vjust='center',hjust='left')+
            #annotate("text",x=Inf,y=MS,label=' mean(sample)',vjust='center',hjust='left')+
            annotate("text",x=Inf,y=MS+(2*SS),label='+2SD(sample)',vjust='center',hjust='left')+
            annotate("text",x=Inf,y=MS-(2*SS),label='-2SD(sample)',vjust='center',hjust='left')+
            theme(plot.margin=unit(c(1,6,1,1),'lines'))+
            coord_cartesian(clip='off') +
            ggtitle(nm)
        return(g)
    }
    
)
