% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/evaluate_k.R
\name{evaluate_k}
\alias{evaluate_k}
\title{Decision for appropriate k}
\usage{
evaluate_k(k, distMatrix, dset_train, r1, cc)
}
\arguments{
\item{k}{Sequence of k nearest neighbors you want to identify for classifyingand tedting accuracy.}

\item{distMatrix}{The distance matrix you are using to caluclate distances between observations.}

\item{dset_train}{The name of the data frame or matrix with which you are training your classifier}

\item{r1}{First row from which you want to start identifying nearest neighbors; 1 if training, 0 if scoring}

\item{cc}{The column in your training dataset that has the classifications of each observation}
}
\value{
correctTest Returns a vector of percentage of correct classifications for each k in sequence
}
\description{
Identifies the best k for the kNN classifier.
}
\examples{
# create artificial dataset
inputsTest   <- matrix(rnorm(100), ncol=2)
inputsTrain  <- matrix(rnorm(100), ncol=2)
classesTrain <- cbind(inputsTrain, c(rep(0, 25), rep(1, 25)))
distance_matrix<- distance(classesTrain, 1, 2, classesTrain, 2)
# get the kNN predictions for the test set
names(classesTrain)<-c(V1="weight", V2="height", Animal="Cat")
k <- seq(1, 25, 2)
evaluate_k(k, distance_matrix, classesTrain, 1, 3)
}

