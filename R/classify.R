
# ----------------------------------------------------------------------
# Simple kNN classifier
# ----------------------------------------------------------------------
#' kNN classifier 
#' 
#' Classify the input with a k nearest neighbors classifier.
#'  

#' @param distMatrix The distance matrix you are using to caluclate distances between observations.
#' @param r1 First row from which you want to start identifying nearest neighbors; 1 if training, 0 if scoring
#' @param k Number of k nearest neighbors you want to identify for classifying  
#' @param dset_train  The name of the data frame or matrix with which you are training your classifier
#' @param  cc The column in your training dataset that has the classifications of each observation
#' @return predictedClasses Return a vector of classified values for each observation in the prediction dataset
#' @export
#' @import assertthat doParallel foreach
#' @examples
#' # create artificial dataset
#' inputsTest   <- matrix(rnorm(100), ncol=2)
#' inputsTrain  <- matrix(rnorm(100), ncol=2)
#' classesTrain <- cbind(inputsTrain, c(rep(0, 25), rep(1, 25)))
#' c1 <- 1
#' c2 <- 2
#' p <- 2
#' distance_matrix<- distance(classesTrain, c1, c2, classesTrain, p)
#' # get the kNN predictions for the test set
#' names(classesTrain)<-c(V1="weight", V2="height", Animal="Cat")
#' k <- 1
#' r1 <- 1
#' cc <- 3
#' classes<- classify(distance_matrix, k, classesTrain, r1, cc)


classify <- function (distMatrix, k, dset_train, r1, cc) {
  assert_that(is.numeric(k))
  assert_that(is.matrix(distMatrix))
  assert_that(is.numeric(r1), r1 > 0)
  assert_that(is.numeric(cc), cc > 0)
  assert_that(is.matrix(dset_train) | is.data.frame(dset_train))
  neighbors <- apply(distMatrix, 2, order)
  
  # Compute and return the most frequent class in the k nearest neighbors
  predictedClasses <-  rep(NA, ncol(neighbors))
  for (obs in 1:ncol(neighbors)) {
    predictedClasses[obs] <- names(which.max(table(dset_train[neighbors[r1:(k+1), obs], cc])))
  }
  return(predictedClasses)
}

