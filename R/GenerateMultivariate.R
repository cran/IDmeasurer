#' Generate dataset with multiple individual identity traits
#'
#' This functions generates a dataset with desired parameters (number of
#' individuals and number of observations per individual, number of variables
#' and covariance between variables, individuality). Unlike for the function
#' \code{GenerateUnivariate}, trait means are not customizable and are always set to 0.

#' @param nindivs Indicates how many individuals should be in dataset
#' @param nobs Indicates how many observations per individual should be in
#'   dataset
#' @param nvars Indicates how many trait variables should be in dataset.
#' @param covar Indicates covariance between variables in dataset. covar=0 for
#'   uncorrelated variables; covar=1 for fully correlated variables
#' @param individuality Indicates the ratio of between to within individual
#'   variation in each trait variable.
#' @return Data frame with the identity codes in the first column and trait
#'   variables in subsequent columns. Number of rows and columns depends on the
#'   parameters used to generate dataset.
#' @examples
#' id1 <- GenerateMultivariate(nindivs=10, nobs=10, nvars=2, covar=0, individuality=1)
#'
#' @family Operations with datasets
#' @export

GenerateMultivariate <- function(nindivs, nobs, nvars, covar, individuality){

  #build covariance matrix
  sigmavec <- rep(covar, nvars*nvars)
  sigma <- matrix(sigmavec, nrow=nvars, ncol=nvars, byrow=T)
  diag(sigma) <- 1

  #means for individuals and variables
  mu <- rep(0, nvars) # means of each variable set to zero
  means <- MASS::mvrnorm(nindivs, mu = mu, Sigma = sigma) # generating means for individuals by each variable with a given covariance matrix
  #plot(means[,1], means[,2], pch=20)

  #sds for each variable
  indsds <- means # just to build same matrix as for means
  for (i in 1:nvars){
    indsds[,i] <- rep(stats::sd(means[,i])/individuality, nindivs) # calculates sd of means for each variable and sets the individual sd to be individuality times the sd between means
  }

  indcodes <- seq(from=1, to=nindivs, by=1) #set up codes for individuals
  parammat <- matrix(rep(NA, nobs*nindivs*nvars), nrow=nindivs*nobs, ncol=nvars, byrow=F) #set up matrix of values for each variable


  # generate values for individuals by each variable; select variable
  for (j in 1:nvars) {

    paramvec <- c()
    id <- c()

    # generate nobs for each individual mean with respective sd; generate individual codes
    for (i in 1:nindivs) {
      paramvec <- c(paramvec, stats::rnorm(nobs, means[i,j], indsds[i,j]))
      id <- c(id, rep(indcodes[i], nobs))
    }
    parammat[,j] <- paramvec #put generated values for each parameter to the matrix and go to next variable

  }

  temp <- data.frame(id, parammat) # pair the matrix with individual codes
  temp$id <- as.factor(temp$id) # individual is factor

  return(temp)
}
