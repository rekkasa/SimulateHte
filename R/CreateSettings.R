#' Create the database settings
#'
#' @description
#' Holds the all the settings for simulating a dataset.
#'
#' @param numberOfObservations               The number of patients in the dataset
#' @param numberOfCovariates                 The number of covariates iun the dataset
#' @param covariateDistributionSettings      A list containing the distribution settings for
#'                                           each covariate in the dataset
#'
#' @return
#' A list with the settings required to simulate a dataset
#'
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



#' Normal distribution settings
#'
#' @description
#' create the settings for sampling from a normal distribution
#'
#' @param mean        the mean of the target normal distribution
#' @param covariance  the covariance of the target normal distribution
#'
#' @return
#' a list with the settings for simulating from a normal distribution
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



#' Multivariate normal distribution settings
#'
#' @description
#' create the settings for sampling from a multivariate normal distribution
#'
#' @param mean        A vector with the means of the target normal distribution
#' @param covariance  The covariance matrix of the target multivariate normal distribution
#'
#' @return
#' a list with the settings for simulating from a multivariate normal distribution
#' @export

createMultivariateNormalDistributionSettings <- function(
  mean,
  covariance
  ) {
  return(
    list(
      type = "multivariate normal",
      mean = mean,
      covariance = covariance
    )
  )
}


#' Binomial distribution settings
#'
#' @description
#' Create the settings for sampling from a binomial distribution.
#'
#' @param size  Number of trials (zero or more).
#' @param prob  Probability of success on each trial.
#'
#' @return
#' A list with the settings for simulating from a binomial distribution.
#' @export

createBinomialDistributionSettings <- function(
  size = 1,
  prob
) {
  return(
    list(
      type = "binomial",
      size = size,
      prob = prob
    )
  )
}


#' Create model settings
#'
#' @description
#' Creates the settings required to define a model explaining the relationship
#' between covariates
#'
#' @param type                      The type of the dependent variable
#' @param constant                  The constant term of the model
#' @param modelMatrix               A logical matrix that instructs which covariates will be
#'                                  used. Each row represents a term in the model constructed
#'                                  multiplicatively based on the columns that represents
#'                                  each one of the dataset's covariates.
#' @param transformationSettings    A list of functions defining the transformation for each
#'                                  term of the model
#' @param coefficients              A vector with the coefficients of the model
#'
#' @return
#' A list with the required settings for the definition of a model
#'
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



#' Create treatment effect settings
#'
#' @description
#' Creates the settings required for modeling the effect of treatment
#'
#' @param type             The reference of treatment effect. If set to "lp", treatment
#'                         effect is modeled as a function of the linear predictor of
#'                         baseline risk
#' @param harm             Constant treatment-related harm (probability).
#' @param modelSettings    The settings required for defining the model of treatment
#'                         effect
#'
#' @return
#' A list of settings for the definition of the treatment effect model
#'
#' @export
createTreatmentEffectSettings <- function(
  type = "lp",
  harm = 0,
  modelSettings
) {
  return(
    list(
      type = type,
      modelSettings = modelSettings,
      harm = harm
    )
  )
}



#' Create baseline risk settings
#'
#' @description
#' Creates the settings required for defining the baseline risk model
#'
#' @param type             The type of the outcome
#' @param modelSettings    The settings required for the definition of the
#'                         baseline risk model
#'
#' @return
#' A list of settings for the definition of the baseline risk model
#'
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



#' Create propensity score settings
#'
#' @description
#' Creates the settings required for the definition of the model based on which
#' treatment is administered
#'
#' @param type             The type of the treatment variable. Default is "binary"
#' @param modelSettings    The settings required for the definition of the
#'                         propensity score model
#'
#' @return
#' A list of settings fir the definition of the propensity score model
#'
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
