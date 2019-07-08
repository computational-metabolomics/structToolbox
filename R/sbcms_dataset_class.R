#' Data from sbcms
#'
#' SBCMS package data after correction, as a struct dataset object
#' @export sbcms_dataset
#' @return dataset object
#' @import datasets
#' @examples
#' D = sbcms_dataset()
#' summary(D)
sbcms_dataset=function() {
    return(sbcms_corrected)
}
