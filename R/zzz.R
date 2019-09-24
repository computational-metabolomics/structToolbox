# Workaround for cmd check "no visible bindings"
# e.g.
#   "chart.plot: no visible binding for global variable 'x'"

if(getRversion() >= "2.15.1"){
    utils::globalVariables(c('x','y','z','Feature','Sample','Peak area',
        'run_order','feature','group','fc','uci','xend','yend','group',
        'lci','pairs'))
}
