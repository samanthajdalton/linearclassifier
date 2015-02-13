
# ----------------------------------------------------------------------
# Evaluate appropriate k for a Simple kNN classifier
# ----------------------------------------------------------------------
#' Decision for appropriate k 
#' 
#' Identifies the best k for the kNN classifier.
#'  

#' @param distMatrix The distance matrix you are using to caluclate distances between observations.
#' @param r1 First row from which you want to start identifying nearest neighbors; 1 if training, 0 if scoring
#' @param k Sequence of k nearest neighbors you want to identify for classifyingand tedting accuracy.
#' @param dset_train  The name of the data frame or matrix with which you are training your classifier
#' @param  cc The column in your training dataset that has the classifications of each observation
#' @return correctTest Returns a vector of percentage of correct classifications for each k in sequence
#' @export
#' @import assertthat 
#' @examples
#' # create artificial dataset
#' inputsTest   <- matrix(rnorm(100), ncol=2)
#' inputsTrain  <- matrix(rnorm(100), ncol=2)
#' classesTrain <- cbind(inputsTrain, c(rep(0, 25), rep(1, 25)))
#' distance_matrix<- distance(classesTrain, 1, 2, classesTrain, 2)
#' # get the kNN predictions for the test set
#' names(classesTrain)<-c(V1="weight", V2="height", Animal="Cat")
#' k <- seq(1, 25, 2)
#' evaluate_k(k, distance_matrix, classesTrain, 1, 3) 


evaluate_k<- function (k, distMatrix, dset_train, r1, cc) {
  assert_that(is.numeric(k))
  assert_that(is.matrix(distMatrix))
  assert_that(is.numeric(r1), r1 > 0)
  assert_that(is.numeric(cc), cc > 0)
  
   correctTest <- rep(NA, length(k))  
  for (iter in 1:length(k)) {
    # get the test error
    predictedClasses <- classify(distMatrix, k[iter], dset_train, r1, cc)
    correctTest[iter] <- mean(predictedClasses==dset_train[,cc])
  }
  return(correctTest)
}