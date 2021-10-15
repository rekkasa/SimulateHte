#' Calculate Precision in Estimation Heterogeneous treatment Effects
#' @description
#' Estimate the unobservable squared loss of absolute treatment benefit
#'
#' @param data               The simulated dataset. It must contain the following
#'                           columns: *untreatedRiskLinearPredictor* contains
#'                           the linear predictor of risk if left untreated;
#'                           *treatedRiskLinearPredictor* contains the linear
#'                           predictor of true risk if left untreated;
#'                           *riskLinearPredictor* contains the predicted risk
#'                           linear predictor.
#' @param predictedBenefit   A vector of predicted absolute benefits for *data*.
#'
#' @export

calculatePEHE <- function(
  data,
  predictedBenefit
) {
  if (is.null(data$treatedRiskLinearPredictor) |
      is.null(data$untreatedRiskLinearPredictor) |
      is.null(data$riskLinearPredictor)) {
    stop("Columns required in data: treatedRiskLinearPredictor,
         untreatedRiskLinearPredictor, riskLinearPredictor")
  }
  trueBenefit <- as.vector(
    stats::plogis(data$untreatedRiskLinearPredictor) - stats::plogis(data$treatedRiskLinearPredictor)
  )

  pehe <- (trueBenefit - predictedBenefit)^2
  pehe <- pehe[!is.na(pehe)]
  n <- length(pehe)
  pehe <- sqrt(sum(pehe) / n)

  return(pehe)
}
