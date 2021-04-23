#' @eval get_description('tic_chart')
#' @export tic_chart
#' @examples
#' D = iris_DatasetExperiment()
#' D$sample_meta$run_order=1:nrow(D)
#' C = tic_chart(factor_name='Species',run_order='run_order')
#' chart_plot(C,D)
#'
tic_chart = function(
    run_order,
    factor_name,
    ...) {
    out=struct::new_struct('tic_chart',
        run_order=run_order,
        factor_name=factor_name,
        ...)
    return(out)
}


.tic_chart<-setClass(
    "tic_chart",
    contains=c('chart'),
    slots=c(
        # INPUTS
        factor_name='entity',
        run_order='entity'
    ),
    prototype = list(
        name='Total Ion Count chart.',
        description=paste0('A scatter plot of Total Ion Count (sum of each ',
        'sample) versus run order.'),
        type="scatter",
        .params=c('factor_name','run_order'),
        factor_name=ents$factor_name,
        run_order=entity(
            name = 'Sample run order column',
            description = paste0('The column name of sample_meta indicating ',
            'the run order of the samples.'),
            value = character(0),
            type='character')
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("tic_chart",'DatasetExperiment'),
    definition=function(obj,dobj) {
        
          clrs= createClassAndColors(class = dobj$sample_meta[[obj$factor_name]])
        
          df=data.frame(
              x=dobj$sample_meta[[obj$run_order]],
              y=rowSums(dobj$data,na.rm = TRUE),
              clr=clrs$class
          )
          
          g = ggplot(data = df,aes_string(x='x',y='y',color='clr')) +
              geom_line(color='#D8D8D8',size=1)+
              geom_point() +
              xlab('Run order') +
              ylab('Total peak area') +
              scale_colour_manual(values=clrs$manual_colors,name=obj$factor_name) +
              theme_Publication(base_size = 12) +
              theme(legend.position="none")
          
          return(g)
          
    }
)