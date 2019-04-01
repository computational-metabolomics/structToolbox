#' permutation test class
#'
#' Applies a permutation test to a model or model.seq()
#' @export permutation_test
permutation_test<-setClass(
  "permutation_test",
  contains='resampler',
  slots=c(params.number_of_permutations='numeric',
          outputs.results.permuted='data.frame',
          outputs.results.unpermuted='data.frame',
          outputs.metric='data.frame'
  ),
  prototype = list(name='permutation test',
                   type='permutation',
                   result='results',
                   params.number_of_permutations=10
  )
)

#' @export
setMethod(f="run",
          signature=c("permutation_test",'dataset','metric'),
          definition=function(I,D,MET=NULL)
          {

            X=dataset.data(D)
            y=dataset.sample_meta(D)
            # get the WF
            WF=models(I)
            n=param.value(I,'number_of_permutations')

            all_results_permuted=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)
            all_results_unpermuted=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)

            for (i in 1:n)
            {
              perm_results=data.frame('actual'=y[,1],'predicted'=y[,1],'permutation'=i)
              unperm_results=data.frame('actual'=y[,1],'predicted'=y[,1],'permutation'=i)
              # generate a random sample order
              order_x=sample.int(nrow(X))
              order_y=sample.int(nrow(X))
              order_y2=order_x # the same order for y

              # permute
              Xp=X[order_x,,drop=FALSE]
              Yp=as.data.frame(y[order_y,,drop=FALSE])
              Yp2=as.data.frame(y[order_y2,,drop=FALSE])

              # rebuild datasets
              Dp=D
              dataset.data(D)=Xp
              dataset.sample_meta(Dp)=Yp

              D2=D
              dataset.data(D2)=Xp
              dataset.sample_meta(D2)=Yp2

              if (is(WF,'model_OR_model.seq'))
              {
                ## permuted labels
                # train
                WF=model.train(WF,Dp)
                # predict
                WF=model.predict(WF,Dp)
                p=predicted(WF)
                perm_results[,2]=p[,1]
                all_results_permuted[((nrow(X)*(i-1))+1):(nrow(X)*i),]=perm_results

                ## real labels
                # train
                WF=model.train(WF,D2)
                # predict
                WF=model.predict(WF,D2)
                p=predicted(WF)
                unperm_results[,2]=p[,1]
                all_results_unpermuted[((nrow(X)*(i-1))+1):(nrow(X)*i),]=unperm_results
              }

              if (is(WF,'iterator'))
              {

                ## permuted
                WF=run(WF,Dp,MET)
                v=output.value(WF,'metric')

                if (i==1)
                {
                  all_results_permuted=v
                }
                else
                {
                  all_results_permuted=rbind(all_results_permuted,v)
                }


                ## real
                WF=run(WF,D2,MET)
                w=output.value(WF,'metric')
                if (i==1)
                {
                  all_results_unpermuted=w
                }
                else
                {
                  all_results_unpermuted=rbind(all_results_unpermuted,w)
                }

              }

            }
            # store results
            output.value(I,'results.permuted')=all_results_permuted
            output.value(I,'results.unpermuted')=all_results_unpermuted
            return(I)
          }
)


#' permutation_test.boxplot class
#'
#' plots the results of a permutation test as a boxplot
#' @export permutation_test.boxplot
permutation_test.boxplot<-setClass(
  "permutation_test.boxplot",
  contains='chart',
  prototype = list(name='permutation test',
                   type='boxplot',
                   description='a boxplot of the calculated metric for the model with permuted and unpermuted data'
  )
)

#' @export
setMethod(f="chart.plot",
          signature=c('permutation_test.boxplot','permutation_test'),
          definition=function(obj,dobj)
          {
            p=output.value(dobj,'results.permuted')
            u=output.value(dobj,'results.unpermuted')

            A=data.frame('value'=c(p$mean,u$mean),'dataset'=c(rep('Permuted',nrow(p)),rep('Unpermuted',nrow(u))))
            plotClass=pmp::createClassAndColors(A$dataset)
            out=ggplot(data=A,aes_(x=~dataset,y=~value,color=~dataset)) +
              geom_boxplot()+
              theme_Publication(base_size = 12)+
              scale_color_manual(values=plotClass$manual_colors,name='dataset') +
              theme(legend.position="none")
            return(out)
          }
)

#' permutation_test.violin class
#'
#' plots the results of a permutation test as a boxplot
#' @export permutation_test.violin
permutation_test.violin<-setClass(
  "permutation_test.violin",
  contains='chart',
  prototype = list(name='permutation test',
                   type='violin',
                   description='a violin plot of the calculated metric for the model with permuted and unpermuted data'
  )
)

#' @export
setMethod(f="chart.plot",
          signature=c('permutation_test.violin','permutation_test'),
          definition=function(obj,dobj)
          {
            p=output.value(dobj,'results.permuted')
            u=output.value(dobj,'results.unpermuted')

            A=data.frame('value'=c(p$mean,u$mean),'dataset'=c(rep('Permuted',nrow(p)),rep('Unpermuted',nrow(u))))
            plotClass=pmp::createClassAndColors(A$dataset)
            out=ggplot(data=A,aes_(x=~dataset,y=~value,color=~dataset)) +
              geom_violin(trim=F)+
              theme_Publication(base_size = 12)+
              scale_color_manual(values=plotClass$manual_colors,name='dataset') +
              theme(legend.position="none")
            return(out)
          }
)

#' permutation_test.hist class
#'
#' plots the results of a permutation test as histograms
#' @export permutation_test.hist
permutation_test.hist<-setClass(
  "permutation_test.hist",
  contains='chart',
  prototype = list(name='permutation test',
                   type='histogram',
                   description='a histogram plot of the calculated metric for the model with permuted and unpermuted data'
  )
)

#' @export
setMethod(f="chart.plot",
          signature=c('permutation_test.hist','permutation_test'),
          definition=function(obj,dobj)
          {
            p=output.value(dobj,'results.permuted')
            u=output.value(dobj,'results.unpermuted')

            A=data.frame('value'=c(p$mean,u$mean),'dataset'=c(rep('Permuted',nrow(p)),rep('Unpermuted',nrow(u))))
            plotClass=pmp::createClassAndColors(A$dataset)
            out=ggplot(data=A,aes_(x=~value,color=~dataset)) +
              geom_freqpoly(binwidth = 0.05)+
              theme_Publication(base_size = 12)+
              scale_color_manual(values=plotClass$manual_colors,name='dataset')
            return(out)
          }
)

#' permutation_test.scatter class
#'
#' plots the results of a permutation test as histograms
#' @export permutation_test.scatter
permutation_test.scatter<-setClass(
  "permutation_test.scatter",
  contains='chart',
  prototype = list(name='permutation test',
                   type='scatter',
                   description='a scatter plot of the calculated metric for the model with permuted and unpermuted data'
  )
)

#' @export
setMethod(f="chart.plot",
          signature=c('permutation_test.scatter','permutation_test'),
          definition=function(obj,dobj)
          {
            p=output.value(dobj,'results.permuted')
            u=output.value(dobj,'results.unpermuted')

            A=data.frame('value'=c(p$mean,u$mean),'dataset'=c(rep('Permuted',nrow(p)),rep('Unpermuted',nrow(u))))
            plotClass=pmp::createClassAndColors(A$dataset)
            out=ggplot(data=A,aes_(x=1:nrow(A),y=~value,color=~dataset)) +
              geom_point(na.rm=T)+
              theme_Publication(base_size = 12)+
              scale_color_manual(values=plotClass$manual_colors,name='dataset') +
              theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())
            return(out)
          }
)
