#' Generate the baseline data
#'
#' @description
#' Generates the dataset of baseline covariates
#'
#' @param databaseSettings     The database settings defined using [createDatabaseSettings()]
#'
#' @return
#' A dataframe with the observed covariates for each patient in the
#' simulated dataset
#'
#' @export

generateBaselineData <- function(
  databaseSettings
) {

  observedCovariateList <- list()
  for (covariate in seq_along(databaseSettings$covariateDistributionSettings)) {
    covariateSettings <- databaseSettings$covariateDistributionSettings[[covariate]]
    if (covariateSettings$type == "normal") {
      tmp <- stats::rnorm(
        n    = databaseSettings$numberOfObservations,
        mean = covariateSettings$mean,
        sd   = covariateSettings$covariance
      )
      ret <- list(tmp)
    } else if (covariateSettings$type == "binomial") {
      tmp <- stats::rbinom(
        n    = databaseSettings$numberOfObservations,
        size = covariateSettings$size,
        prob = covariateSettings$prob
      )
      ret <- list(tmp)
    } else if (covariateSettings$type == "multivariate normal") {
      mvTmp <- MASS::mvrnorm(
        n     = databaseSettings$numberOfObservations,
        mu    = covariateSettings$mean,
        Sigma = covariateSettings$covariance
      )
      ret <- lapply(seq_len(ncol(mvTmp)), function(i) mvTmp[,i])
    }
    observedCovariateList <- c(observedCovariateList, ret)
  }

  observedCovariates <- dplyr::bind_cols(
    observedCovariateList,
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  )
  numberOfCovariates <- length(observedCovariateList)
  names(observedCovariates) <- paste0(
    "x",
    1:numberOfCovariates
  )

  return(observedCovariates)

}




#' Generate linear predictor
#'
#' @description
#' Generates the linear predictor of a model based on model settings
#'
#' @param data             The dataframe based on which the model will be defined
#' @param modelSettings    The model settings defined based on [createModelSettings()]
#'
#' @return
#' The linear predictor for each patient in the simulated dataset
#'
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

      if (!is.list(trans)) {
        trans <- list(trans)
      }

      includedCovariates <- modelSettings$modelMatrix[i, ]
      res[[i]] <- apply(
        sapply(
          1:ncol(data[as.logical(includedCovariates)]),
          function(x) trans[[x]](data[as.logical(includedCovariates)][[x]])
        ),
        1,
        prod
      )

    }
  }

  filledMatrix <- as.matrix(
    dplyr::bind_cols(
      1, res,
      .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
    )
  )
  coefficientMatrix <- matrix(
    c(
      modelSettings$constant,
      modelSettings$coefficients
    )
  )
  ret <- as.vector(filledMatrix %*% coefficientMatrix)

  return(ret)

}
