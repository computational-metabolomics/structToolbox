#' filter features by fraction missing values
#'
#' filters features by the percent number of missing values
#' @export mv_feature_filter
#' @import pmp
#' @examples
#' M = mv_feature_filter()
mv_feature_filter<-setClass(
    "mv_feature_filter",
    contains = c('model'),
    slots=c(params.threshold='entity',
        params.qc_label='entity',
        params.method='enum',
        params.factor_name='entity',
        outputs.filtered='entity',
        outputs.flags='entity'
    ),
    prototype=list(name = 'Filter by fraction missing values',
        description = 'Filters by removing features where the percent number of missing values exceeds the threshold',
        type = 'filter',
        predicted = 'filtered',

        params.factor_name=entity(name='Factor name',
            type='character',
            description='Name of sample_meta column to use'
        ),

        params.threshold=entity(name = 'Missing value threshold (%)',
            description = 'Features with greather than THRESHOLD% missing values are excluded.',
            value = 20,
            type='numeric'),

        params.qc_label=entity(name = 'QC label',
            description = 'Label used to identify QC samples.',
            value = 'QC',
            type='character'),

        params.method=enum(name='Method',
            description='"within_all" applies filter within classes,"within_one" applies filter within any one class, "QC" applies filter within QC samples, "across" applies filter ignoring class.',
            value='QC',
            type='character',
            list=c('within_all','within_one','QC','across')),

        outputs.filtered=entity(name = 'Filtered dataset',
            description = 'A dataset object containing the filtered data.',
            type='dataset',
            value=dataset()
        ),
        outputs.flags=entity(name = 'Flags',
            description = '% missing values and a flag indicating whether the sample was rejected.',
            type='data.frame',
            value=data.frame()
        )
    )
)

#' @export
#' @template method_apply
setMethod(f="model.apply",
    signature=c("mv_feature_filter","dataset"),
    definition=function(M,D)
    {
        opt=param.list(M)

        smeta=dataset.sample_meta(D)
        x=dataset.data(D)

        s=strsplit(opt$method,'_')[[1]][1]

        filtered = filter_peaks_by_fraction(t(x), min_frac = opt$threshold/100, classes=smeta[[M$factor_name]], method=s,qc_label=opt$qc_label)
        #dataset.data(D) = as.data.frame(t(filtered$df))

        flags<-data.frame(filtered$flags)
        vmeta=dataset.variable_meta(D)

        if (opt$method=='within_all') {
            L=levels(smeta[[M$factor_name]])
            IN=apply(flags[,(length(L)+1):ncol(flags)],MARGIN=1,function(x) all(x==1))

        } else if (opt$method=='within_one') {
            L=levels(smeta[[M$factor_name]])
            IN=apply(flags[,(length(L)+1):ncol(flags)],MARGIN=1,function(x) any(x==1))
        } else {
            IN=flags[,2]==1

        }
        nmes=rownames(flags)[IN]
        vmeta=vmeta[nmes,,drop=FALSE]
        x=x[,IN,drop=FALSE]
        dataset.data(D) = x
        dataset.variable_meta(D)=vmeta

        output.value(M,'filtered') = D
        output.value(M,'flags') = flags

        return(M)
    }
)


##### plots
#' plot for missing value sample filter
#'
#' plots a histogram of % missing values per sample
#' @import struct
#' @export mv_feature_filter.hist
#' @examples
#' C = mv_feature_filter.hist()
mv_feature_filter.hist<-setClass(
    "mv_feature_filter.hist",
    contains='chart',
    prototype = list(name='Histogram of missing values per feature',
        description='A histogram of the % missing values per feature',
        type="histogram"
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart.plot",
    signature=c("mv_feature_filter.hist",'mv_feature_filter'),
    definition=function(obj,dobj)
    {
        if (param.value(dobj,'method')=='within')
        {
            stop('plot not implemented for within class filter')
        }

        t=param.value(dobj,'threshold')
        A=output.value(dobj,'flags')
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
