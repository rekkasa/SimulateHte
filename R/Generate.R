#' @export
generateBaselineData <- function(
  databaseSettings
) {

  observedCovariateList <- list()
  for (covariate in seq_along(databaseSettings$covariateDistributionSettings)) {
    covariateSettings <- databaseSettings$covariateDistributionSettings[[covariate]]
    if (covariateSettings$type == "normal") {
      tmp <- rnorm(
        n = databaseSettings$numberOfObservations,
        mean = covariateSettings$mean,
        sd = covariateSettings$covariance
      )
    }
    observedCovariateList[[covariate]] <- tmp
  }

  observedCovariates <- dplyr::bind_cols(observedCovariateList)
  names(observedCovariates) <- paste0(
    "x",
    1:databaseSettings$numberOfCovariates
  )

  return(observedCovariates)

}




#' @export
generateLinearPredictor <- function(
  data,
  modelSettings
) {

  numberOfModelElements <- nrow(modelSettings$modelMatrix)
  res <- list()
  if (numberOfModelElements > 0) {
    for (i in 1:numberOfModelElements) {
      trans <- modelSettings$transformationSettings[[i]]
      includedCovariates <- modelSettings$modelMatrix[i, ]
      res[[i]] <- apply(
        data[as.logical(includedCovariates)],
        1,
        trans
      )
    }
  }

  filledMatrix <- as.matrix(
    dplyr::bind_cols(1, res)
  )
  coefficientMatrix <- matrix(
    c(
      modelSettings$constant,
      modelSettings$coefficients
    )
  )
  ret <- filledMatrix %*% coefficientMatrix

  return(ret)

}
