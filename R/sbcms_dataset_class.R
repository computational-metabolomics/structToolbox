#' Data from sbcms
#'
#' SBCMS package data after correction, as a struct dataset object
#' @export sbcms_dataset
#' @return dataset object
#' @import datasets
#' @param filtered TRUE to load prefiltered data, or FALSE to load the unfiltered data
#' @examples
#' D = sbcms_dataset()
#' summary(D)
sbcms_dataset=function(filtered=FALSE) {

    if (filtered) {
        M = filter_by_name(mode='include',dimension='variable',names=to_filter)
        M = method.apply(M,sbcms_corrected)
        return(predicted(M))
    } else {
        return(sbcms_corrected)
    }
}
