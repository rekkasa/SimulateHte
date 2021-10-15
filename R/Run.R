#' Run data generation
#'
#' @description
#' Runs a single simulation based on the user-specified settings
#'
#' @param databaseSettings         The settings for the observed part of the dataset.
#'                                 Created from [createDatabaseSettings()]
#' @param propensitySettings       The settings for the true propensity score model.
#'                                 Created from [createPropensitySettings()]
#' @param baselineRiskSettings     The settings for the true baseline risk model.
#'                                 Created from [createBaselineRiskSettings()]
#' @param treatmentEffectSettings  The settings for the model of treatment effect.
#'                                 Created from [createTreatmentEffectSettings()]
#'
#' @return
#' The simulated dataset
#'
#' @export
#' @importFrom dplyr %>%

runDataGeneration <- function(
  databaseSettings,
  propensitySettings,
  baselineRiskSettings,
  treatmentEffectSettings
) {

  data <- generateBaselineData(
    databaseSettings = databaseSettings
  )

  riskLinearPredictor <- generateLinearPredictor(
    data = data,
    modelSettings = baselineRiskSettings$modelSettings
  )

  propensityLinearPredictor <- generateLinearPredictor(
    data = data,
    modelSettings = propensitySettings$modelSettings
  )

  if (treatmentEffectSettings$type == "lp") {
    dataForTreatedLp <- dplyr::tibble(
      lp = riskLinearPredictor
    )
  } else if (treatmentEffectSettings$type == "covariates") {
    dataForTreatedLp <- data
  }

  treatedLinearPredictor <- generateLinearPredictor(
    data          = dataForTreatedLp,
    modelSettings = treatmentEffectSettings$modelSettings
  )

  if (propensitySettings$type == "binary") {
    treatment <- stats::rbinom(
      n    = databaseSettings$numberOfObservations,
      size = 1,
      prob = stats::plogis(propensityLinearPredictor)
    )
  }

  if (treatmentEffectSettings$type == "covariates") {
    treatedLinearPredictor <- riskLinearPredictor + treatedLinearPredictor
  }

  res <- data.frame(
    data,
    untreatedRiskLinearPredictor = riskLinearPredictor,
    propensityLinearPredictor = propensityLinearPredictor,
    treatedLinearPredictor = treatedLinearPredictor,
    treatment = treatment
  )

  if (propensitySettings$type == "binary" & baselineRiskSettings$type == "binary") {
    res <- res %>%
      dplyr::mutate(
        treatedRiskLinearPredictor = treatedLinearPredictor,
        observedRiskLinearPredictor = treatment * dplyr::.data$treatedLinearPredictor +
          (1 - treatment) * dplyr::.data$untreatedRiskLinearPredictor
      )
    res$outcome <- stats::rbinom(
      n = databaseSettings$numberOfObservations,
      size = 1,
      prob = stats::plogis(res$observedRiskLinearPredictor)
    )

    res <- res %>%
      dplyr::mutate(
        trueBenefit = stats::plogis(dplyr::.data$untreatedRiskLinearPredictor) - stats::plogis(dplyr::.data$treatedRiskLinearPredictor)
      )
  }


  return(res)

}

expit <- function(x) {
  ret <- exp(x) / (1 + exp(x))
  return(ret)
}
