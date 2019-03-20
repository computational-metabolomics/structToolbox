#' PQN nromalisation
#'
#' applies PQN normalisation using QC samples as reference samples
#' @export pqn_norm
#' @import pmp
pqn_norm<-setClass(
  "pqn_norm",
  contains = c('method'),
  slots=c(params.qc_label='entity',
          outputs.normalised='entity',
          outputs.coeff='entity'
  ),
  prototype=list(name = 'Probabilistic Quotient Normalisation (PQN)',
                 description = 'PQN normalisation using QC samples as reference samples',
                 type = 'normalisation',
                 predicted = 'normalised',
                 params=c('qc_label'),
                 outputs=c('normalised','coeff'),

                 params.qc_label=entity(name = 'QC label',
                                        description = 'Label used to identify QC samples.',
                                        value = 'QC',
                                        type='character'),

                 outputs.normalised=entity(name = 'Normalised dataset',
                                         description = 'A dataset object containing the normalised data.',
                                         type='dataset',
                                         value=dataset()
                 ),
                 outputs.flags=entity(name = 'PQN coefficients',
                                      description = 'The normalisation coefficients calculated by PQN',
                                      type='data.frame',
                                      value=data.frame()
                 )
  )
)

#' @export
setMethod(f="method.apply",
          signature=c("pqn_norm","dataset"),
          definition=function(M,D)
          {
            opt=param.list(M)

            smeta=dataset.sample_meta(D)
            x=dataset.data(D)

            normalised = pqn_normalisation(t(x), classes=smeta[,1],qc_label=opt$qc_label) # operates on transpose of x
            dataset.data(D) = as.data.frame(t(normalised$df))

            output.value(M,'normalised') = D
            output.value(M,'coeff') = data.frame('coeff'=normalised$coef,row.names = rownames(x))

            return(M)
          }
)


##### plots
#' plot for PQN normalisation
#'
#' plots a histogram of the PQN coeffients
#' @import struct
#' @export pqn_norm.hist
pqn_norm.hist<-setClass(
  "pqn_norm.hist",
  contains='chart',
  prototype = list(name='Histogram of the PQN coefficients per feature',
                   description='A histogram of the PQN coefficients per feature',
                   type="histogram"
  )
)

#' @export
setMethod(f="chart.plot",
          signature=c("pqn_norm.hist",'pqn_norm'),
          definition=function(obj,dobj)
          {
            A=output.value(dobj,'coeff')

            out=ggplot(data=A, aes_(x=~coeff)) +
              geom_histogram(color='white') +
              xlab('PQN coefficient') +
              ylab('Count') +
              scale_fill_Publication()+
              theme_Publication(base_size = 12)

            return(out)
          }
)
