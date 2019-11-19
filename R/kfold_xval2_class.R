#' kfold_xval model class
#'
#' Applies k-fold crossvalidation to a model or model.seq()
#' @export kfold_xval2
#' @examples
#' I = kfold_xval2()
kfold_xval2<-setClass(
    "kfold_xval2",
    contains='resampler',
    slots=c(params.folds='numeric',
        params.method='character',
        params.factor_name='entity',
        outputs.metric='data.frame'
    ),
    prototype = list(name='k-fold cross-validation',
        type="resampling",
        result='metric',
        params.folds=10,
        params.method='venetian'
    )
)

#' @export
#' @template run
setMethod(f="run",
    signature=c("kfold_xval2",'dataset','metric'),
    definition=function(I,D,MET=NULL)
    {
        X=dataset.data(D)


        WF=models(I)

        # venetian 123123123123
        if (param.value(I,'method')=='venetian')
        {
            fold_id=rep(1:param.value(I,'folds'),length.out=nrow(X))
        } else if (param.value(I,'method')=='blocks')
        { # blocks 111122223333
            fold_id=rep(1:param.value(I,'folds'),length.out=nrow(X))
            fold_id=sort(fold_id)
        } else if (param.value(I,'method')=='random') {
            fold_id=rep(1:param.value(I,'folds'),length.out=nrow(X))
            fold_id=sample(fold_id,length(fold_id),replace = FALSE)
        } else {
            stop('unknown method for cross-validation. (try "venetian", "blocks" or "random")')
        }

        # for each value of k, split the data and run the workflow
        for (i in 1:param.value(I,'folds'))
        {
            # prep the training data
            TrainX=X[fold_id!=i,,drop=FALSE]
            TrainY=Y[fold_id!=i,,drop=FALSE]
            dtrain=dataset(data=TrainX,sample_meta=TrainY)

            TestX=X[fold_id==i,,drop=FALSE]
            TestY=Y[fold_id==i,,drop=FALSE]
            dtest=dataset(data=TestX,sample_meta=TestY)

            if (is(WF,'model_OR_model.seq'))
                # HAS TO BE A model OR model.seq
            {
                WF=model.train(WF,dtrain)
                # apply the model
                WF=model.predict(WF,dtrain)
                p=predicted(WF)
                # metric
                if (MET@actual=='sample_meta') {
                    yhat=p
                } else if (MET@actual=='data') {
                    yhat=p$data
                } else {
                    stop('MET$actual not implemented yet')
                }
                YHATtr[fold_id!=i,]=yhat

                # test set
                WF=model.predict(WF,dtest)
                p=predicted(WF)

                if (MET@actual=='sample_meta') {
                    yhat=p
                } else if (MET@actual=='data') {
                    yhat=p$data
                } else {
                    stop('MET$actual not implemented yet')
                }
                YHAT[fold_id==i,]=yhat


            } else if (is(WF,'iterator'))
            {
                stop('not implemented yet')
            }
            # validation set...??
            # WF=predict(WF,dval)
            # p=predicted(WF[length(WF)])
            # val_result[,1]=p[,1]

            #all_results[((nrow(X)*(i-1))+1):(nrow(X)*i),]=fold_results
        }

        if (MET@actual=='data') {
            # if its a model sequence get the prediction from the penultimate step
            # for comparison with the predictions
            if (is(WF,'model_OR_model.seq')) {
                # apply model to data
                WF=model.apply(WF,D)
                n=length(WF)
                if (n>1) {# just in case a sequence of 1
                    Y=predicted(WF[n-1])$data
                }
            }
        }

        # test sets metric
        df=data.frame('training_set'=0,'test_set'=0,'metric'=class(MET)[[1]])
        MET=calculate(MET,Y,YHAT)
        df$training_set=value(MET)
        # training set metric
        MET=calculate(MET,Y,YHATtr)
        df$test_set=value(MET)
        I$metric=df
        return(I)
    }
)

