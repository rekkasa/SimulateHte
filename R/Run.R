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
    riskLinearPredictor = riskLinearPredictor,
    propensityLinearPredictor = propensityLinearPredictor,
    treatmentLinearPredictor = treatmentLinearPredictor,
    treatment = treatment
  )

  if (propensitySettings$type == "binary" & baselineRiskSettings$type == "binary") {
    res <- res %>%
      dplyr::mutate(
        observedRiskLinearPredictor = riskLinearPredictor + treatment * treatmentLinearPredictor
      )
    res$outcome <- rbinom(
      n = databaseSettings$numberOfObservations,
      size = 1,
      prob = exp(res$observedRiskLinearPredictor) / (1 + exp(res$observedRiskLinearPredictor))
    )
  }


  return(res)

}
