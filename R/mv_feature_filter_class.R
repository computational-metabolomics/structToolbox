#' filter features by fraction of missing values
#'
#' Filters features by the percent number of missing values, based on class labels if required.
#' @param threshold The max percentage missing values in a feature, above which the feature is removed. Default  is 20.
#' @param qc_label The label of the QC samples in the named sample_meta column.
#' @param factor_name The name of the sample_meta column to use.
#' @param method "within_all" applies filter within classes,"within_one" applies
#' filter within any one class, "QC" applies filter within QC samples, "across" applies filter
#' ignoring class.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export mv_feature_filter
#' @examples
#' M = mv_feature_filter()
mv_feature_filter = function(threshold=20,qc_label='QC',method='QC',factor_name,...) {
    out=struct::new_struct('mv_feature_filter',
        threshold=threshold,
        qc_label=qc_label,
        method=method,
        factor_name,
        ...)
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
        libraries='pmp',
        .params=c('threshold','qc_label','method','factor_name'),
        .outputs=c('filtered','flags'),

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
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export mv_feature_filter_hist
#' @examples
#' C = mv_feature_filter_hist()
mv_feature_filter_hist = function(...) {
    out=struct::new_struct('mv_feature_filter_hist',...)
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
