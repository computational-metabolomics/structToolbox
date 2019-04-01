#' vector nromalisation
#'
#' applies vector normalisation
#' @export vec_norm
#' @import pmp
vec_norm<-setClass(
  "vec_norm",
  contains = c('method'),
  slots=c(outputs.normalised='entity',
          outputs.coeff='entity'
  ),
  prototype=list(name = 'Vector normalisation',
                 description = 'Normalises each row such that the sum of squares is equal to 1',
                 type = 'normalisation',

                 outputs.normalised=entity(name = 'Normalised dataset',
                                           description = 'A dataset object containing the normalised data.',
                                           type='dataset',
                                           value=dataset()
                 ),
                 outputs.flags=entity(name = 'Normalisation coefficients',
                                      description = 'The normalisation coefficients calculated by PQN',
                                      type='data.frame',
                                      value=data.frame()
                 )
  )
)

#' @export
setMethod(f="method.apply",
          signature=c("vec_norm","dataset"),
          definition=function(M,D)
          {
            smeta=dataset.sample_meta(D)
            x=dataset.data(D)

            coeff = apply(x,MARGIN = 2,FUN = function(z) sqrt(sum(z, na.rm = TRUE)))
            normalised = apply(x,MARGIN = 2,FUN = function(z) z/sqrt(sum(z, na.rm=TRUE)))
            dataset.data(D) = as.data.frame(normalised)

            output.value(M,'normalised') = D
            output.value(M,'coeff') = data.frame('coeff'=coeff)

            return(M)
          }
)
