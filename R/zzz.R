# Workaround for cmd check "no visible bindings"
# e.g.
#   "chart_plot: no visible binding for global variable 'x'"

if(getRversion() >= "2.15.1"){
    utils::globalVariables(c('x','y','z','Feature','Sample','Peak area',
        'run_order','feature','group','fc','uci','xend','yend','group',
        'lci','pairs','segment'))
}





#' ontology cache
#'
#' A cached list of ontology terms obtained from the ontology lookup service 
#' (OLS) for ontology terms specified for objects in \code{structToolbox}.
#' 
#' @export ontology_cache
#' @return list of cached ontology terms
#' @seealso ontology
#' @examples
#' cache = ontology_cache()
ontology_cache=function() {
    return(ontology_cached)
}
