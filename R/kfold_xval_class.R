#' kfold_xval model class
#'
#' Applies k-fold crossvalidation to a model or model.seq()
#' @export kfold_xval
kfold_xval<-setClass(
  "kfold_xval",
  contains='resampler',
  slots=c(params.folds='numeric',
          params.method='character',
          outputs.results='data.frame',
          outputs.metric='data.frame',
          outputs.metric.train='numeric',
          outputs.metric.test='numeric'
  ),
  prototype = list(name='k-fold cross-validation',
                   type="resampling",
                   predicted='results',
                   params.folds=10,
                   params.method='venetian',
                   params=c('folds','method'),
                   outputs=c('results','metric','metric.train','metric.test'),
                   charts=c('metric')
  )
)

#' @export
setMethod(f="run",
          signature=c("kfold_xval",'dataset','metric'),
          definition=function(I,D,MET=NULL)
          {
            X=dataset.data(D)
            y=dataset.sample_meta(D)[,1,drop=FALSE]
            all_results=data.frame('actual'=rep(y[,1],param.value(I,'folds')),'predicted'=rep(y[,1],param.value(I,'folds')),'fold'=0,'in.test'=FALSE,'sampleid'=rep(rownames(X),param.value(I,'folds')))
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
              fold_results=data.frame('actual'=y[,1],'predicted'=y,'fold'=i,'in.test'=FALSE,'sampleid'=rownames(X))
              fold_results[fold_id==i,4]=TRUE

              # prep the training data
              TrainX=X[fold_id!=i,,drop=FALSE]
              TrainY=as.data.frame(y[fold_id!=i,,drop=FALSE])
              dtrain=dataset(data=TrainX,sample_meta=TrainY)

              TestX=X[fold_id==i,,drop=FALSE]
              TestY=as.data.frame(y[fold_id==i,,drop=FALSE])
              dtest=dataset(data=TestX,sample_meta=TestY)

              if (is(WF,'model_OR_model.seq'))
              # HAS TO BE A model OR model.seq
              {
                WF=model.train(WF,dtrain)
                # apply the model
                WF=model.predict(WF,dtrain)
                p=predicted(WF)
                fold_results[fold_id!=i,2]=p[,1]

                # test set
                WF=model.predict(WF,dtest)
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
            output.value(I,'results')=all_results
            I=evaluate(I,MET)

            return(I)
          }
)

#' @export
setMethod(f="evaluate",
          signature=c("kfold_xval","metric"),
          definition=function(I,MET)
          {
            results=output.value(I,'results')
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
            out=data.frame('metric'=class(MET),'mean'=mean(te.metric),'sd'=sd(te.metric))

            output.value(I,'metric')=out
            output.value(I,'metric.train')=ts.metric
            output.value(I,'metric.test')=te.metric
            return(I)
          }
)

# setMethod(f="chart.plot",
#           signature=c("kfold_xval"),
#           definition = function(obj,name,opt=NULL) {
#             # check if valid chart name
#             is.chart(obj,name) # error if not
#
#             out=switch(name,
#                        metric=metric_plot_kfold_xcv(obj,opt))
#
#             return(out)
#           }
#
# )
#
#
# # metric plot
# metric_plot_kfold_xcv=function(obj,opt=NULL)
# {
#   n=length(output.value(obj,'metric.train'))
#   A=data.frame(value=c(output.value(obj,'metric.train'),
#                        output.value(obj,'metric.test')),
#                dataset=c(rep('Training',n),'Test'))
#   A$dataset=factor(A$dataset,levels = c('Training','Test'),ordered=T)
#   plotClass=pmp::createClassAndColors(A$dataset)
#     out=ggplot(data=A,aes_(x=~dataset,y=~value,color=~dataset)) +
#       geom_boxplot()+
#     theme_Publication(base_size = 12)+
#     scale_color_manual(values=plotClass$manual_colors,name='dataset') +
#     theme(legend.position="none")
#   return(out)
# }
