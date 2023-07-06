#' @eval get_description('DFA')
#' @export DFA
#' @include entity_objects.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = DFA(factor_name='Species')
#' M = model_apply(M,D)
DFA = function(factor_name,number_components=2,...) {
    out=struct::new_struct('DFA',
        factor_name=factor_name,
        number_components=number_components,
        ...)
    return(out)
}


.DFA<-setClass(
    "DFA",
    contains = c('model'),
    slots=c(
        number_components='entity',
        factor_name='entity',
        scores='DatasetExperiment',
        loadings='data.frame',
        eigenvalues='data.frame',
        that='DatasetExperiment'
    ),
    
    prototype=list(name = 'Discriminant Factor Analysis',
        description = paste0('Discriminant Factor Analysis (DFA) is a ',
            'supervised classification method. Using a linear combination of the ',
            'input variables, DFA finds new orthogonal axes (canonical values) ',
            'to minimize the variance within each given class and maximize ',
            'variance between classes.'),
        type = 'classification',
        predicted = 'that',
        citations=list(
            bibentry(
                bibtype='Book',
                title='Multivariate Statistical Methods: A Primer',
                author=as.person('B.F.J. Manly'),
                year='1986',
                publisher='Chapman and Hall',
                address='Boca Raton'
            )
        ),
        .params=c('factor_name','number_components'),
        .outputs=c('scores','loadings','eigenvalues','that'),
        
        factor_name=ents$factor_name,
        
        number_components=entity(value = 2,
            name = 'Number of components',
            description = 'The number of DFA components calculated.',
            type = c('numeric','integer')
        )
        
    )
)

#' @export
#' @template model_train
setMethod(f="model_train",
    signature=c("DFA","DatasetExperiment"),
    definition=function(M,D) {
        
        # column means
        cm = as.matrix(colMeans(D$data))
        
        # factors
        fn = levels(D$sample_meta[[M$factor_name]])
        
        # within covariance
        Sw=matrix(0,nrow=ncol(D$data),ncol=ncol(D$data))
        # between covariance
        Sb=Sw
        nk=matrix(0,nrow=length(fn),ncol=1)
        for (k in 1:length(fn)) {
            # samples in this class
            IN=D$sample_meta[[M$factor_name]]==fn[k] 
            # number in each class
            nk[k]=sum(IN) 
            # covariance for this class
            w=(nk[k]-1)*cov(D$data[IN,])
            # within covariance 
            Sw=Sw+w
            # between covariance
            m = as.matrix(colMeans(D$data[IN,])) # group mean
            b = nk[k]*(m - cm) %*% t(m - cm)
            Sb = Sb+b
        }
        # scale
        Sw=Sw/(nrow(D$data)-length(fn))
        Sb=Sb/(length(fn)-1)
        
        # fisher discriminant
        P = solve(Sw) %*% Sb
        
        # projection
        ev = eigen(P)
        
        # handle imaginary values (should be vv small)
        ev$values=Re(ev$values)
        ev$vectors=Re(ev$vectors)
        
        # reduce to number of desired components
        ev$values=ev$values[1:M$number_components]
        ev$vectors=ev$vectors[,1:M$number_components]
        
        scores = as.matrix(D$data) %*% ev$vectors
        
        ## store outputs
        #scores
        scores=as.data.frame(scores)
        colnames(scores)=as.character(interaction('DF',1:ncol(scores),sep=''))
        rownames(scores)=rownames(D$data)
        VM=data.frame('DFA'=colnames(scores))
        rownames(VM)=VM$DFA
        M$scores=DatasetExperiment(data=scores,sample_meta=D$sample_meta,variable_meta=VM)
        
        # loadings
        loadings=as.data.frame(ev$vectors)
        colnames(loadings)=colnames(scores)
        rownames(loadings)=colnames(D$data)
        M$loadings=loadings
        
        # eigenvalues
        M$eigenvalues=data.frame('eigenvalue'=ev$values,row.names = colnames(scores))
        
        return(M)
    }
)

#' @export
#' @template model_predict
setMethod(f="model_predict",
    signature=c("DFA","DatasetExperiment"),
    definition=function(M,D) {
        
        X=as.matrix(D$data)
        P=output_value(M,'loadings')
        that=X%*%as.matrix(P)
        
        that=as.data.frame(that)
        rownames(that)=rownames(X)
        varnames=colnames(M$loadings)
        colnames(that)=varnames
        
        # convert to DatasetExperiment for preprocessing output
        S=DatasetExperiment(data=that,sample_meta=D$sample_meta,variable_meta=varnames)
        M$that=S
        
        return(M)
    }
)


#' @eval get_description('dfa_scores_plot')
#' @import struct
#' @export dfa_scores_plot
#' @include DFA_class.R
#' @examples
#' D = iris_DatasetExperiment()
#' M = mean_centre() + DFA(factor_name='Species')
#' M = model_apply(M,D)
#' C = dfa_scores_plot(factor_name = 'Species')
#' chart_plot(C,M[2])
#'
dfa_scores_plot = function(
    components=c(1,2),
    points_to_label='none',
    factor_name,
    ellipse='all',
    label_filter=character(0),
    label_factor='rownames',
    label_size=3.88,
    ...) {
    out=struct::new_struct('dfa_scores_plot',
        components=components,
        points_to_label=points_to_label,
        factor_name=factor_name,
        ellipse=ellipse,
        label_filter=label_filter,
        label_factor=label_factor,
        label_size=label_size,
        ...)
    return(out)
}


.dfa_scores_plot<-setClass(
    "dfa_scores_plot",
    contains='chart',
    slots=c(
        # INPUTS
        components='entity',
        points_to_label='enum',
        factor_name='entity',
        ellipse='enum',
        label_filter='entity',
        label_factor='entity',
        label_size='entity'
    ),
    
    prototype = list(name='DFA scores plot',
        description='A scatter plot of the selected DFA components.',
        type="scatter",
        libraries=c('scales','ggplot2'),
        .params=c('components','points_to_label','factor_name','ellipse',
            'label_filter','label_factor','label_size'),
        
        components=entity(name='Components to plot',
            value=c(1,2),
            type='numeric',
            description=paste0('The components selected for plotting.'),
            max_length=2
        ),
        
        points_to_label=enum(name='Points to label',
            value='none',
            type='character',
            description=c(
                'none' = 'No samples labels are displayed.', 
                "all" = 'The labels for all samples are displayed.', 
                "outliers" = 'Labels for for potential outlier samples are displayed.'
            ),
            allowed=c('none','all','outliers')
        ),
        factor_name=ents$factor_name,
        ellipse=enum(
            name = 'Plot ellipses',
            description=c(
                "all" = 'Hotelling T2 ellipses (p=0.95) are plotted for all groups and all samples.',
                "group" = 'Hotelling T2 ellipses (p=0.95) are plotted for all groups.',
                "none" = 'Ellipses are not included on the plot.',
                "sample" = 'A Hotelling T2 ellipse (p=0.95) is plotted for all samples (ignoring group)'),
            allowed=c('all','group','none','sample'),
            value='all'
        ),
        label_filter=entity(
            name='Label filter',
            value=character(0),
            type='character',
            description=paste0(
                'Labels are only plotted for the named groups. If ',
                'zero-length then all groups are included.'
            )
        ),
        label_factor=entity(name='Factor for labels',
            description=paste0('The column name of sample_meta to use for ',
                'labelling samples on the plot. "rownames" will use the row ',
                'names from sample_meta.'),
            type='character',
            value='rownames',
            max_length=1),
        label_size=entity(name='Text size of labels',
            description='The text size of labels. Note this is not in Font Units.',
            type='numeric',
            value=3.88,
            max_length=1)
    )
)




#' @importFrom sp point.in.polygon
#' @import ggplot2
#' @importFrom scales squish
#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c("dfa_scores_plot",'DFA'),
    definition=function(obj,dobj)
    {
        
        if (obj$points_to_label=='outliers' & !(obj$ellipse %in% c('all','sample'))) {
            warning('Outliers are only labelled when plotting the sample ellipse')
        }
        opt=param_list(obj)
        scores=output_value(dobj,'scores')$data
        
        if (length(obj$factor_name)==1) {
            shapes = 19 # filled circles for all samples
        } else {
            shapes = factor(dobj$scores$sample_meta[[obj$factor_name[2]]])
        }
        
        if (obj$label_factor=='rownames') {
            slabels = rownames(dobj$scores$sample_meta)
        } else {
            slabels = dobj$scores$sample_meta[[obj$label_factor]]
        }
        opt$factor_name=opt$factor_name[[1]] # only use the first factor from now on
        
        x=scores[,opt$components[1]]
        y=scores[,opt$components[2]]
        xlabel=paste("DF",opt$components[[1]],sep='')
        ylabel=paste("DF",opt$components[[2]],sep='')
        
        # get the factor from meta data
        opt$groups=dobj$scores$sample_meta[[opt$factor_name]]
        
        # add a space to the front of the labels to offset them from the points, because nudge_x is in data units
        for (i in 1:length(slabels)) {
            slabels[i]=paste0('  ',slabels[i], '  ')
        }
        
        # filter by label_filter list if provided
        if (length(obj$label_filter)>0) {
            out=!(as.character(opt$groups) %in% obj$label_filter)
            slabels[out]=''
        }
        
        if (is(opt$groups,'factor') | is(opt$groups,'character')) {
            plotClass= createClassAndColors(opt$groups)
            opt$groups=plotClass$class
        }
        
        # build the plot
        A <- data.frame (group=opt$groups,x=x, y=y)
        
        if (length(obj$factor_name)==2) {
            out=ggplot (data=A, aes_(x=~x,y=~y,colour=~group,label=~slabels,shape=~shapes))
        }   else {
            out=ggplot (data=A, aes_(x=~x,y=~y,colour=~group,label=~slabels))
        }
        out=out+
            
            geom_point(na.rm=TRUE) +
            xlab(xlabel) +
            ylab(ylabel) +
            ggtitle('DFA Scores', subtitle=NULL)
        
        if (length(obj$factor_name)==2) {
            out=out+labs(shape=obj$factor_name[[2]],colour=obj$factor_name[[1]])
        } else {
            out=out+labs(shape=obj$factor_name[[1]])
        }
        
        if (obj$ellipse %in% c('all','group')) {
            out = out +stat_ellipse(type='norm') # ellipse for individual groups
        }
        
        if (is(opt$groups,'factor')) { # if a factor then plot by group using the colours from pmp package
            out=out+scale_colour_manual(values=plotClass$manual_colors,name=opt$factor_name)
        }
        else {# assume continuous and use the default colour gradient
            out=out+scale_colour_viridis_c(limits=quantile(opt$groups,c(0.05,0.95),na.rm = TRUE),oob=squish,name=opt$factor_name)
        }
        out=out+theme_Publication(base_size = 12)
        # add ellipse for all samples (ignoring group)
        if (obj$ellipse %in% c('all','sample')) {
            out=out+stat_ellipse(type='norm',mapping=aes(x=x,y=y),colour="#C0C0C0",linetype='dashed',data=A)
        }
        
        if (obj$ellipse %in% c('all','sample')) { # only do this if we plotted the sample ellipse
            # identify samples outside the ellipse
            build=ggplot_build(out)$data
            points=build[[1]]
            ell=build[[length(build)]]
            # outlier for DatasetExperiment ellipse
            points$in.ell=as.logical(sp::point.in.polygon(points$x,points$y,ell$x,ell$y))
            
            # label outliers if
            if (opt$points_to_label=='outliers')
            {
                if (!all(points$in.ell))
                {
                    temp=subset(points,!points$in.ell)
                    temp$group=opt$groups[!points$in.ell]
                    out=out+geom_text(data=temp,aes_(x=~x,y=~y,label=~label,colour=~group),size=obj$label_size,vjust="inward",hjust="inward")
                    
                }
            }
            # add a list of outliers to the plot object
            out$outliers=trimws(slabels[!points$in.ell])
        }
        
        # label all points if requested
        if (opt$points_to_label=='all') {
            out=out+geom_text(vjust="inward",hjust="inward")
        }
        
        return(out)
    }
)
