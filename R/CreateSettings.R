#' @export
createDatabaseSettings <- function(
  numberOfObservations,
  numberOfCovariates,
  covariateDistributionSettings # list of length = numberOfCovariates
) {
  return(
    list(
      numberOfObservations = numberOfObservations,
      numberOfCovariates = numberOfCovariates,
      covariateDistributionSettings = covariateDistributionSettings
    )
  )
}




#' @export
createNormalDistributionSettings <- function(
  mean = 0,
  covariance = 1
) {
  return(
    list(
      type = "normal",
      mean = mean,
      covariance = covariance
    )
  )
}




#' @export
createSimulationSettings <- function(
  outcome = "binary",
  databaseSettings,
  baselineRiskSettings, # createModelSettings
  treatmentEffectSettings  # createModelSettings
) {
  return(
    list(
      outcome = outcome,
      databaseSettings = databaseSettings,
      baselineRiskSettings = baselineRiskSettings,
      treatmentEffectSettings = treatmentEffectSettings
    )
  )
}




#' @export
createModelSettings <- function(
  type = "logistic",
  constant,
  modelMatrix,
  transformationSettings,
  coefficients
) {

  if (missing(coefficients) | missing(modelMatrix)) {
    message("Missing coefficients or model matrix\nAssuming model with only constant")
    modelMatrix <- diag(0)
    coefficients <- NULL
    transformationSettings <- list()
  }
  return(
    list(
      constant = constant,
      modelMatrix = modelMatrix,
      transformationSettings = transformationSettings,
      coefficients = coefficients
    )
  )
}




#' @export
createTreatmentEffectSettings <- function(
  type = "lp",
  modelSettings
) {
  return(
    list(
      type = type,
      modelSettings = modelSettings
    )
  )
}




#' @export
createBaselineRiskSettings <- function(
  type = "binary",
  modelSettings
) {
  return(
    list(
      type = type,
      modelSettings = modelSettings
    )
  )
}




#' @export
createPropensitySettings <- function(
  type = "binary",
  modelSettings
) {
  return(
    list(
      type = type,
      modelSettings = modelSettings
    )
  )
}
