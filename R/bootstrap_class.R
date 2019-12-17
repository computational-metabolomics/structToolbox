#' permutation test class
#'
#' Applies a permutation test to a model or model_seq()
#' @examples
#' I=bootstrap()
#'
#' @param ... slots and values for the new object
#' @export bootstrap
bootstrap = function(...) {
    out=.bootstrap()
    out=struct::.initialize_struct_class(out,...)
    return(out)
}


.bootstrap<-setClass(
    "bootstrap",
    contains='resampler',
    slots=c(
        params_number_of_permutations='numeric',
        params_collect='character',
        outputs_results='data.frame',
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

#' @param ... slots and values for the new object
#' @export
#' @template run
setMethod(f="run",
          signature=c("bootstrap",'DatasetExperiment','metric'),
          definition=function(I,D,MET=NULL)
          {

              X=D$data
              y=D$sample_meta
              # get the WF
              WF=models(I)
              n=param_value(I,'number_of_permutations')

              all_results=data.frame('actual'=rep(y[,1],n),'predicted'=rep(y[,1],n),'permutation'=0)

              collected=list()

              for (i in 1:n)
              {
                  results=data.frame('actual'=y[,1],'predicted'=y[,1],'permutation'=i)

                  # generate a random sample order
                  order=sample.int(nrow(X),replace = TRUE) # WITH REPLACEMENT

                  # permute
                  Xp=X[order,,drop=FALSE]
                  Yp=as.data.frame(y[order,,drop=FALSE])

                  # rebuild datasets
                  Dp=D
                  Dp$data=Xp
                  Dp$sample_meta=Yp

                  if (is(WF,'model_OR_model_seq'))
                  {
                      ## permuted labels
                      # train
                      WF=model_train(WF,Dp)
                      # predict
                      WF=model_predict(WF,Dp)
                      p=predicted(WF)
                      results[,2]=p[,1]
                      all_results[((nrow(X)*(i-1))+1):(nrow(X)*i),]=results

                      if (!is.na(I$collect)) {
                          if (is(WF,'model')) {
                              collected=c(collected,list(output_value(WF,I$collect)))
                          } else {
                              # if sequence assume collecting from last index
                              collected=c(collected,list(output_value(WF[length(WF)],I$collect)))
                          }
                          I$collected=collected
                      }


                  }

                  if (is(WF,'iterator'))
                  {

                     stop('not implemented yet')
                  }

              }
              # store results
              output_value(I,'results')=all_results
              return(I)
          }
)


