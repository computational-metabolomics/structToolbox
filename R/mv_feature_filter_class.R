#' filter features by fraction missing values
#'
#' filters features by the percent number of missing values
#' @param ... slots and values for the new object
#' @return struct object
#' @export mv_feature_filter
#' @import pmp
#' @examples
#' M = mv_feature_filter()
mv_feature_filter = function(...) {
    out=.mv_feature_filter()
    out=struct::new_struct(out,...)
    return(out)
}


.mv_feature_filter<-setClass(
    "mv_feature_filter",
    contains = c('model'),
    slots=c(threshold='entity',
        qc_label='entity',
        method='enum',
        factor_name='entity',
        filtered='entity',
        flags='entity'
    ),
    prototype=list(name = 'Filter by fraction missing values',
        description = 'Filters by removing features where the percent number of missing values exceeds the threshold',
        type = 'filter',
        predicted = 'filtered',

        factor_name=entity(name='Factor name',
            type='character',
            description='Name of sample_meta column to use'
        ),

        threshold=entity(name = 'Missing value threshold (%)',
            description = 'Features with greather than THRESHOLD% missing values are excluded.',
            value = 20,
            type='numeric'),

        qc_label=entity(name = 'QC label',
            description = 'Label used to identify QC samples.',
            value = 'QC',
            type='character'),

        method=enum(name='Method',
            description='"within_all" applies filter within classes,"within_one" applies filter within any one class, "QC" applies filter within QC samples, "across" applies filter ignoring class.',
            value='QC',
            type='character',
            allowed=c('within_all','within_one','QC','across')),

        filtered=entity(name = 'Filtered DatasetExperiment',
            description = 'A DatasetExperiment object containing the filtered data.',
            type='DatasetExperiment',
            value=DatasetExperiment()
        ),
        flags=entity(name = 'Flags',
            description = '% missing values and a flag indicating whether the sample was rejected.',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @param ... slots and values for the new object
#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("mv_feature_filter","DatasetExperiment"),
    definition=function(M,D)
    {
        opt=param_list(M)

        smeta=D$sample_meta
        x=D$data

        s=strsplit(opt$method,'_')[[1]][1]

        filtered = filter_peaks_by_fraction(t(x), min_frac = opt$threshold/100, classes=smeta[[M$factor_name]], method=s,qc_label=opt$qc_label)
        #D$data = as.data.frame(t(filtered$df))

        flags<-data.frame(filtered$flags)

        output_value(M,'flags') = flags

        return(M)
    }
)

#' @param ... slots and values for the new object
#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("mv_feature_filter","DatasetExperiment"),
    definition=function(M,D) {

        x=D$data
        smeta=D$sample_meta
        vmeta=D$variable_meta

        flags=M$flags

        if (M$method=='within_all') {
            L=levels(smeta[[M$factor_name]])
            IN=apply(flags[,(length(L)+1):ncol(flags)],MARGIN=1,function(x) all(x==1))

        } else if (M$method=='within_one') {
            L=levels(smeta[[M$factor_name]])
            IN=apply(flags[,(length(L)+1):ncol(flags)],MARGIN=1,function(x) any(x==1))
        } else {
            IN=flags[,2]==1

        }

        D=D[,IN,drop=FALSE]

        output_value(M,'filtered') = D
        return(M)
    }
)


##### plots
#' plot for missing value sample filter
#'
#' plots a histogram of % missing values per sample
#' @import struct
#' @param ... slots and values for the new object
#' @return struct object
#' @export mv_feature_filter_hist
#' @examples
#' C = mv_feature_filter_hist()
mv_feature_filter_hist = function(...) {
    out=.mv_feature_filter_hist()
    out=struct::new_struct(out,...)
    return(out)
}


.mv_feature_filter_hist<-setClass(
    "mv_feature_filter_hist",
    contains='chart',
    prototype = list(name='Histogram of missing values per feature',
        description='A histogram of the % missing values per feature',
        type="histogram"
    )
)

#' @param ... slots and values for the new object
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("mv_feature_filter_hist",'mv_feature_filter'),
    definition=function(obj,dobj)
    {
        if (param_value(dobj,'method')=='within')
        {
            stop('plot not implemented for within class filter')
        }

        t=param_value(dobj,'threshold')
        A=output_value(dobj,'flags')
        n=colnames(A)
        A$x=100-((A[,1])*100) # filter report number of values, not number of missing values
        A$features=factor(A[,2],levels=c(1,0),labels=c('accepted','rejected'))
        out=ggplot(data=A, aes_(x=~x,fill=~features)) +
            geom_histogram(boundary=(100-t),color='white') +
            xlab('% missing values (per feature)') +
            ylab('Count') +
            scale_fill_Publication()+
            theme_Publication(base_size = 12) +
            ggtitle('Missing values')

        return(out)
    }
)
