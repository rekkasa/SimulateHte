#' Run data generation
#'
#' @description
#' Runs a single simulation based on the user-specified settings
#'
#' @param databaseSettings         The settings for the observed part of the dataset.
#'                                 Created from [creaeDatabaseSettings()]
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
    treatmentLinearPredictor <- generateLinearPredictor(
      data = data.frame(
        lp = riskLinearPredictor
      ),
      modelSettings = treatmentEffectSettings$modelSettings
    )
  }

  if (propensitySettings$type == "binary") {
    treatment <- rbinom(
      n = databaseSettings$numberOfObservations,
      size = 1,
      prob = exp(propensityLinearPredictor) / (1 + exp(propensityLinearPredictor))
    )
  }

  res <- data.frame(
    data,
    untreatedRiskLinearPredictor = riskLinearPredictor,
    propensityLinearPredictor = propensityLinearPredictor,
    treatmentLinearPredictor = treatmentLinearPredictor,
    treatment = treatment
  )

  if (propensitySettings$type == "binary" & baselineRiskSettings$type == "binary") {
    res <- res %>%
      dplyr::mutate(
        treatedRiskLinearPredictor = riskLinearPredictor + treatmentLinearPredictor,
        observedRiskLinearPredictor = treatment * treatedRiskLinearPredictor + (1 - treatment) * untreatedRiskLinearPredictor
      )
    res$outcome <- rbinom(
      n = databaseSettings$numberOfObservations,
      size = 1,
      prob = exp(res$observedRiskLinearPredictor) / (1 + exp(res$observedRiskLinearPredictor))
    )
  }


  return(res)

}
t
