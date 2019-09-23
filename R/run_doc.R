#' Runs an iterator, applying the chosen model multiple times.
#'
#' Running an iterator will apply the iterator a number of times to a dataset.
#' For example, in cross-validation the same model is applied multiple times to
#' the same data, splitting it into training and test sets. The input metric
#' object can be calculated and collected for each iteration as an output.
#' @param I an iterator object
#' @param D a dataset object
#' @param MET a metric object
#' @return Modified iterator object
#' @examples
#' D = iris_dataset() # get some data
#' MET = metric()  # use a metric
#' I = example_iterator() # initialise iterator
#' models(I) = example_model() # set the model
#' I = run(I,D,MET) # run
#' @name run
NULL
