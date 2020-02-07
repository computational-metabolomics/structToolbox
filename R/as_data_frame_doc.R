#' Convert to data.frame
#'
#' convert the outputs of the input model into a data.frame.
#'
#' @param M a model object
#' @return A data.frame of model outputs
#' @examples
#' D = iris_DatasetExperiment()
#' M = filter_na_count(threshold=50,factor_name='Species')
#' M= model_apply(M,D)
#' df = as_data_frame(M)
#' @name as_data_frame
NULL
