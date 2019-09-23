#' Model prediction
#'
#' Apply a model using the input dataset. Assumes the model is trained
#' first.
#' @param M a model object
#' @param D a dataset object
#' @return Returns a modified model object
#' @rdname predict
#' @export
#' @examples
#' M = example_model()
#' M = model.predict(M,iris_dataset())
#'
