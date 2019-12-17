#' permutation test class
#'
#' Applies a permutation test to a model or model_seq()
#' @examples
#' I=permutation_test2()
#'
#' @export permutation_test2
permutation_test2 = function(...) {
    out=.permutation_test2()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.permutation_test2<-setClass(
    "permutation_test2",
    contains='resampler',
    slots=c(
        params_number_of_permutations='numeric',
        params_collect='character',
        outputs_results.permuted='data.frame',
        outputs_results.unpermuted='data.frame',
        outputs_metric='data.frame',
        outputs_collected='entity'
    ),
    prototype = list(name='permutation test',
                     type='permutation',
                     result='results',
                     params_number_of_permutations=10,
                     params_collect='vip',
                     outputs_collected=entity(name='collected output',
                                              type=c('logical','list'),
                                              value=NA,max_length=Inf)
    )
)

#' @export
#' @template run
setMethod(f="run",
          signature=c("permutation_test2",'DatasetExperiment','metric'),
          definition=function(I,D,MET=NULL)
          {

              X=D$data
              y=D$sample_meta
              # get the WF
              WF=models(I)
              n=param_value(I,'number_of_permutations')

              all_results_permuted=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)
              all_results_unpermuted=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)

              collected=list(permuted=list(),unpermuted=list())

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
                  Dp$data=Xp
                  Dp$sample_meta=Yp

                  D2=D
                  D2$data=Xp
                  D2$sample_meta=Yp2

                  if (is(WF,'model_OR_model_seq'))
                  {
                      ## permuted labels
                      # train
                      WF=model_train(WF,Dp)
                      # predict
                      WF=model_predict(WF,Dp)
                      p=predicted(WF)
                      perm_results[,2]=p[,1]
                      all_results_permuted[((nrow(X)*(i-1))+1):(nrow(X)*i),]=perm_results

                      if (!is.na(I$collect)) {
                          if (is(WF,'model')) {
                              collected$permuted=c(collected$permuted,list(output_value(WF,I$collect)))
                          } else {
                              # if sequence assume collecting from last index
                              collected$permuted=c(collected$permuted,list(output_value(WF[length(WF)],I$collect)))
                          }
                          I$collected=collected
                      }

                      ## real labels
                      # train
                      WF=model_train(WF,D2)
                      # predict
                      WF=model_predict(WF,D2)
                      p=predicted(WF)
                      unperm_results[,2]=p[,1]
                      all_results_unpermuted[((nrow(X)*(i-1))+1):(nrow(X)*i),]=unperm_results

                      if (!is.na(I$collect)) {
                          if (is(WF,'model')) {
                              collected$unpermuted=c(collected$unpermuted,list(output_value(WF,I$collect)))
                          } else {
                              # if sequence assume collecting from last index
                              collected$unpermuted=c(collected$unpermuted,list(output_value(WF[length(WF)],I$collect)))
                          }
                          I$collected=collected
                      }
                  }

                  if (is(WF,'iterator'))
                  {

                      ## permuted
                      WF=run(WF,Dp,MET)
                      v=output_value(WF,'metric')

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
                      w=output_value(WF,'metric')
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
              output_value(I,'results.permuted')=all_results_permuted
              output_value(I,'results.unpermuted')=all_results_unpermuted
              return(I)
          }
)


