# Workaround for cmd check "no visible bindings"
# e.g.
#   "chart_plot: no visible binding for global variable 'x'"

if(getRversion() >= "2.15.1"){
    utils::globalVariables(c('x','y','z','Feature','Sample','Peak area',
        'run_order','feature','group','fc','uci','xend','yend','group',
        'lci','pairs','segment'))
}



#' @importFrom utils capture.output
get_description=function(id) {
    
    # object template
    M=new_struct(id)
    
    # title
    str=paste0('@title ', M$name)
    
    # get description
    str=c(str,paste0('@description ',M$description))
    
    # citations
    cits=citations(M)
    cits[length(cits)]=NULL
    for (k in seq_along(cits)) {
        cit=format(cits[[k]],style='latex')
        str=c(str,paste0('@references ',cit))
    }
    
    # get libraries
    if (length(M$libraries)>0) {
        str2=paste0('@details ',
            'This object makes use of functionality from the following packages:',
            '\\itemize{'
        )
        for (k in seq_along(M$libraries)) {
            str2=paste0(str2,'\\item{\\code{',M$libraries[k],'}}')
        }
        str2=paste0(str2,'}')
        str=c(str,str2)
    }
    
    # parameters
    P=formals(id)
    
    # for each parameter generate some text
    D=list()
    for (k in seq_along(P)) {
        
        # skip if ellipsis
        if (names(P)[k]=='...') {
            D[[k]]= paste0('@param ',names(P)[k], 
                ' Additional slots and values passed to \\code{struct_class}.')
            next
        }
        # get parameter as an object
        p = param_obj(M,names(P)[k])
        # if its an entity object then get its description
        if (is(p,'entity')) {
            d = p$description
            # ensure first character is upper case and last character is a fullstop.
            d=unlist(lapply(d,function(x){
                # first is upper
                substr(x,1,1) = toupper(substr(x,1,1))
                # last is .
                if (substr(x,nchar(x),nchar(x)) != '.') {
                    x=paste0(x,'.')
                }
                return(x)
            }))
            
            # if d has more than one entry and is a named vector then...
            if (length(d)>1) {
                # if it has names then
                if (!is.null(names(d))) {
                    # create a named list
                    it_list='\\itemize{'
                    for (j in seq_along(d)) {
                        it_list=paste0(it_list,'\\item{\\code{"',names(d)[j],'"}: ',d[j],'}')
                    }
                } else {
                    # no names so use a bulleted list
                    it_list='\\itemize{'
                    for (j in seq_along(d)) {
                        it_list=paste0(it_list,'\\item{',d[j],'}')
                    }
                }
                
                # add list
                it_list=paste0(it_list,'}')
                d=paste0(p$name,'. Allowed values are limited to the following: ',it_list)
            }
            
            # add the allowed types
            t = p$type
            
        } else {
            # if not an entity then there is no description
            d = ''
            t=class(P[[k]])
        }
        # collapse if more than 1
        t=paste0(t,collapse=', ')
        # enclose in brackets
        t=paste0('(',t,') ')
        # add to description
        d=paste0(t,d)
        
        # if the parameter has a default, then add on the text.
        if (!is(P[[k]],'name')){
            d=paste0(d, ' The default is ')
            if (length(P[[k]])>1) {
                d=paste0(d,'\\code{',capture.output(P[[k]]),'}.')
            } else {
                if (is.null(P[[k]])) {
                    d=paste0(d,'\\code{NULL}.')
                } else if (is(P[[k]],'character')) {
                    d=paste0(d,'\\code{"',P[[k]],'"}.')
                } else {
                    d=paste0(d,'\\code{',P[[k]],'}.')
                }
            }
        } else {
            # no default is provided
        }
        D[[k]]=paste0('@param ',names(P)[k], ' ', d)
    }
    str=c(str,D)
    
    str=c(str,
        paste0('@return ','A  \\code{', class(M)[1],'} object.' )
    )
    
    return(unlist(str))
}