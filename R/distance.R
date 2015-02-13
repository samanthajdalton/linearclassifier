
# ----------------------------------------------------------------------
# Distances for KNN
# ----------------------------------------------------------------------
#' Calculates distances for KNN
#' 
#' Classify distances bewteen observations in the training data or in the training vs validation.
#'  
#' @param dset_pred A data frame or a matrix where rows are observations and 
#' columns are features. If you are training a dataset this is your training dataset, 
#' if you are predicting a validation file, this is your validation file.
#' @param c1 Beginning column number for features in your dset_pred (assumes all features are in adjacent columns)
#' @param c2 Ending column number for features in your dset_pred   (assumes all features are in adjacent columns)

#' @param p Distance metric the classifier should use, the value can be 
#' either 1(L1 norm), 2(euclidian) or Inf. 
#' @param dset_train  The name of the data frame or matrix with which you are training your classifier
#' @return distMatrix A matrix of computed distances.
#' @export
#' @import assertthat doParallel foreach
#' @examples
#' # create artificial dataset
#' inputsTest   <- matrix(rnorm(100), ncol=2)
#' inputsTrain  <- matrix(rnorm(100), ncol=2)
#' classesTrain <- cbind(inputsTrain, c(rep(0, 25), rep(1, 25)))
#' # get the kNN predictions for the test set
#' names(classesTrain)<-c(V1="weight", V2="height", Animal="Cat")
#' test<-distance(classesTrain, 1, 2, classesTrain, 2)

distance<- function (dset_pred, c1, c2, dset_train, p) {
  assert_that(is.numeric(c1), c1 > 0)
  assert_that(is.numeric(c2), c2 > 0)
  assert_that(is.matrix(dset_train) | is.data.frame(dset_train))
  assert_that(is.matrix(dset_pred) | is.data.frame(dset_pred))
  
  distMatrix <- matrix(NA, nrow(dset_train),  nrow(dset_pred))
  cores<-detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  clusterExport(cl, c("dset_pred","distMatrix","c1","c2", "dset_train","p"), envir=environment())
  
  distMatrix <- foreach (obs=1:nrow(dset_train), .combine = "rbind") %dopar% {
    
    # getting the probe for the current observation
    
    probe <- as.numeric(dset_train[obs, c1:c2])
    #head(probe,17)
    #head(dset_pred[ ,c1:c2],15)
    probeExpanded <- matrix(rep(probe, times=nrow(dset_pred)), byrow = T, nrow=nrow(dset_pred))
    #head(probeExpanded)
    
    # computing distances between the probe and exemplars in the memory
    
    if (p %in% c(1,2)) {
      rowSums((abs(dset_pred[ ,c1:c2] - probeExpanded)^p) )^(1.0/p)
    } else if (p==Inf) {
      
      apply(abs(dset_pred[ ,c1:c2] - probeExpanded), 1, max)
      
    }  
  }
  stopCluster(cl)
  colnames(distMatrix) <- c(1:nrow(dset_pred))
  return (distMatrix)
  
}
