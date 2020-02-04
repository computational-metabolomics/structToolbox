#' Data from sbcms
#'
#' SBCMS package data after correction, as a struct DatasetExperiment object
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export sbcms_DatasetExperiment
#' @return DatasetExperiment object
#' @import datasets
#' @param filtered TRUE to load prefiltered data, or FALSE to load the unfiltered data
#' @examples
#' D = sbcms_DatasetExperiment()
#' summary(D)
sbcms_DatasetExperiment=function(filtered=FALSE) {

    if (filtered) {
        M = filter_by_name(mode='include',dimension='variable',names=to_filter)
        M = model_apply(M,sbcms_corrected)
        return(predicted(M))
    } else {
        return(sbcms_corrected)
    }
}
