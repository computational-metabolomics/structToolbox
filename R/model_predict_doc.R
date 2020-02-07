#' Model prediction
#'
#' Apply a model using the input DatasetExperiment. Assumes the model is trained
#' first.
#' @param M a model object
#' @param D a DatasetExperiment object
#' @return Returns a modified model object
#' @examples
#' M = example_model()
#' M = model_predict(M,iris_DatasetExperiment())
#' @name model_predict
NULL
