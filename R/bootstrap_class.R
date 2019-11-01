#' permutation test class
#'
#' Applies a permutation test to a model or model.seq()
#' @examples
#' I=bootstrap()
#'
#' @export bootstrap
bootstrap<-setClass(
    "bootstrap",
    contains='resampler',
    slots=c(
        params.number_of_permutations='numeric',
        params.collect='character',
        outputs.results='data.frame',
        outputs.metric='data.frame',
        outputs.collected='entity'
    ),
    prototype = list(name='permutation test',
                     type='permutation',
                     result='results',
                     params.number_of_permutations=10,
                     params.collect='vip',
                     outputs.collected=entity(name='collected output',
                                              type=c('logical','list'),
                                              value=NA,max_length=Inf)
    )
)

#' @export
#' @template run
setMethod(f="run",
          signature=c("bootstrap",'dataset','metric'),
          definition=function(I,D,MET=NULL)
          {

              X=dataset.data(D)
              y=dataset.sample_meta(D)
              # get the WF
              WF=models(I)
              n=param.value(I,'number_of_permutations')

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
                  dataset.data(Dp)=Xp
                  dataset.sample_meta(Dp)=Yp

                  if (is(WF,'model_OR_model.seq'))
                  {
                      ## permuted labels
                      # train
                      WF=model.train(WF,Dp)
                      # predict
                      WF=model.predict(WF,Dp)
                      p=predicted(WF)
                      results[,2]=p[,1]
                      all_results[((nrow(X)*(i-1))+1):(nrow(X)*i),]=results

                      if (!is.na(I$collect)) {
                          if (is(WF,'model')) {
                              collected=c(collected,list(output.value(WF,I$collect)))
                          } else {
                              # if sequence assume collecting from last index
                              collected=c(collected,list(output.value(WF[length(WF)],I$collect)))
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
              output.value(I,'results')=all_results
              return(I)
          }
)


