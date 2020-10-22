#' @eval get_description('kfold_xval')
#' @export kfold_xval
#' @examples
#' D = iris_DatasetExperiment()
#' I = kfold_xval(factor_name='Species') *
#'     (mean_centre() + PLSDA(factor_name='Species'))
#' I = run(I,D,balanced_accuracy())
#'
kfold_xval = function(folds=10,method='venetian',factor_name,...) {
    out=struct::new_struct('kfold_xval',
        folds=folds,
        method=method,
        factor_name=factor_name,
        ...)
    return(out)
}


.kfold_xval<-setClass(
    "kfold_xval",
    contains='resampler',
    slots=c(
        folds='entity',
        method='enum',
        factor_name='entity',
        results='data.frame',
        metric='data.frame',
        metric.train='numeric',
        metric.test='numeric'
    ),
    prototype = list(name='k-fold cross-validation',
        type="resampling",
        result='results',
        .params=c('folds','method','factor_name'),
        .outputs=c('results','metric','metric.train','metric.test'),
        description=paste0('k-fold cross-validation is an iterative approach ',
            'applied to validate models. The samples are divided into k "folds", ',
            'or subsets. Each subset is excluded from model training and used for ',
            'model validation once, resulting in a single left-out prediction for ',
            'each sample. Model performance metrics are then computed for the ',
            'training and test sets across all folds.'),
        folds=entity(
            name='Number of folds',
            description = 'The number of cross-validation folds.',
            type=c('numeric','integer'),
            value=5,
            max_length=1),
        method=enum(
            name = 'Fold selection method',
            description=c(
                'venetian' = 'Every nth sample is assigned to the same fold, where n is the number of folds.',
                'blocks' = 'Blocks of adjacent samples are assigned to the same fold.',
                'random' = 'Samples are randomly assigned to a fold.'
            ),
            type='character',
            allowed = c('venetian','blocks','random'),
            value='random'
        ),
        factor_name=ents$factor_name
    )
)

#' @export
#' @template run
setMethod(f="run",
    signature=c("kfold_xval",'DatasetExperiment','metric'),
    definition=function(I,D,MET=NULL)
    {
        X=D$data
        y=D$sample_meta[,I$factor_name,drop=FALSE]
        all_results=data.frame('actual'=rep(y[,1],param_value(I,'folds')),'predicted'=rep(y[,1],param_value(I,'folds')),'fold'=0,'in.test'=FALSE,'sampleid'=rep(rownames(X),param_value(I,'folds')))
        WF=models(I)
        
        # venetian 123123123123
        if (param_value(I,'method')=='venetian')
        {
            fold_id=rep(1:param_value(I,'folds'),length.out=nrow(X))
        } else if (param_value(I,'method')=='blocks')
        { # blocks 111122223333
            fold_id=rep(1:param_value(I,'folds'),length.out=nrow(X))
            fold_id=sort(fold_id)
        } else if (param_value(I,'method')=='random') {
            fold_id=rep(1:param_value(I,'folds'),length.out=nrow(X))
            fold_id=sample(fold_id,length(fold_id),replace = FALSE)
        } else {
            stop('unknown method for cross-validation. (try "venetian", "blocks" or "random")')
        }
        
        # for each value of k, split the data and run the workflow
        for (i in 1:param_value(I,'folds'))
        {
            fold_results=data.frame('actual'=y[,1],'predicted'=y,'fold'=i,'in.test'=FALSE,'sampleid'=rownames(X))
            fold_results[fold_id==i,4]=TRUE
            
            # prep the training data
            TrainX=X[fold_id!=i,,drop=FALSE]
            TrainY=as.data.frame(y[fold_id!=i,,drop=FALSE])
            dtrain=DatasetExperiment(data=TrainX,sample_meta=TrainY,variable_meta=D$variable_meta)
            
            TestX=X[fold_id==i,,drop=FALSE]
            TestY=as.data.frame(y[fold_id==i,,drop=FALSE])
            dtest=DatasetExperiment(data=TestX,sample_meta=TestY,variable_meta=D$variable_meta)
            
            if (is(WF,'model_OR_model_seq'))
                # HAS TO BE A model OR model_seq
            {
                WF=model_train(WF,dtrain)
                # apply the model
                WF=model_predict(WF,dtrain)
                p=predicted(WF)
                fold_results[fold_id!=i,2]=p[,1]
                
                # test set
                WF=model_predict(WF,dtest)
                p=predicted(WF)
                fold_results[fold_id==i,2]=p[,1]
            } else if (is(WF,'iterator'))
            {
                stop('not implemented yet')
            }
            # validation set...??
            # WF=predict(WF,dval)
            # p=predicted(WF[length(WF)])
            # val_result[,1]=p[,1]
            
            all_results[((nrow(X)*(i-1))+1):(nrow(X)*i),]=fold_results
        }
        models(I)=WF # last model
        output_value(I,'results')=all_results
        I=evaluate(I,MET)
        
        return(I)
    }
)

setMethod(f="evaluate",
    signature=c("kfold_xval","metric"),
    definition=function(I,MET)
    {
        results=output_value(I,'results')
        k=max(results$fold)
        ts.metric=numeric(k)
        te.metric=numeric(k)
        for (i in 1:k)
        {
            # training set
            ts=results[!results$in.test,]
            ts=ts[ts$fold==i,]
            MET=calculate(MET,ts$actual,ts$predicted)
            ts.metric[i]=value(MET)
        }
        # test set
        te=results[results$in.test,]
        MET=calculate(MET,te$actual,te$predicted)
        te.metric=value(MET)
        out=data.frame('metric'=class(MET)[1],'mean'=mean(te.metric),'sd'=sd(te.metric))
        
        output_value(I,'metric')=out
        output_value(I,'metric.train')=ts.metric
        output_value(I,'metric.test')=te.metric
        return(I)
    }
)

