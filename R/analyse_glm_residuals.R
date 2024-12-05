# =============================================================================
# Residual analysis for GLM models
# earlyc@stanford.edu - March, 2023
# =============================================================================

#' This function evaluates residuals from 'glm' model objects. It returns
#' a normality plot, residuals vs. fitted, and a lag-plot with a trend line
#' (glm(residual ~ residual -1)) to evaluate autocorrelation. It also
#' provides a Shapiro-Wilk normality test, Levene test for homogeneity of
#' variance, a Runs test for randomness, and a t-test for mean zero.
#'
#' @param glm_object
#' @return Residual plots: normality, residuals vs. fitted data, ACF plot
#'
#' Residual tests: Mean zero, Shapiro-Wilk normality test, Levene Test for
#' homogeneity of variance, and Ljung-Box test for autocorrelation
#'
#' @export
#'
#' @usage
#' analyse_glm_residuals(glm_object)
#'
#' @importFrom magrittr %>%
#' @importFrom car leveneTest
#' @importFrom DescTools RunsTest
#' @importFrom stats dt
#' @importFrom stats residuals
#' @importFrom stats fitted
#' @importFrom stats shapiro.test
#' @importFrom stats sd
#' @importFrom stats qqnorm
#' @importFrom stats qqline
#' @importFrom broom glance
#' @importFrom graphics abline
#' @importFrom graphics par


# .............................................................................
# analyseGlmResiduals
# .............................................................................

# To run this function, you must have car, and DescTools installed
analyse_glm_residuals <- function(glm_object) {
  library(broom)
  # Load and prepare data -----------------------------------------------------
  resi <- glm_object$residuals
  resDf <- as.data.frame(resi)
  fit <- glm_object$fitted.values
  # Subset residuals by sign
  resDf$sign <- as.factor(ifelse(resDf < 0,
                                 "negative",
                                 "positive"))

  # Make plots ----------------------------------------------------------------
  par(mar = c(3, 3, 3, 3), mai=c(0.4,0.4,0.4,0.4)) # Adjust margins

  # Define layout matrix
  mat_layout <- matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE)
  layout(mat_layout)

  # Normality Plot in the first row, first column
  stats::qqnorm(resDf$resi, cex.main = 0.75)
  stats::qqline(resDf$resi)

  # Residuals vs Fitted Plot in the second row, first column
  graphics::plot(x = fit, y = resi,
                 xlab = "Fitted values",
                 ylab = "Residuals",
                 main = "Residuals versus fitted values",
                 cex.main = 0.75)
  graphics::abline(h=0)

  # ACF Plot in the second row, second column
  autocorrelation <- stats::acf(glm_object$residuals,
                         type = "correlation",
                         main = " ") # Blank default title, NULL doesn't work
  title("ACF Plot",
        cex.main = 0.75)

  # Run tests -----------------------------------------------------------------
  norm <- stats::shapiro.test(resDf$resi)
  levene <- car::leveneTest(resDf$resi ~ sign, data = resDf)
  box <- stats::Box.test(resi, type = "Ljung-Box")
  result <- list(normality = norm,
                 levene = levene,
                 box = box)
  names(result) <- c("Residual Normality Test",
                     "Levene's Test",
                     "Ljung-Box Test")
  print(result)

  # Run t-test for mean = 0 ---------------------------------------------------
  stDev <- stats::sd(resDf$resi)
  mean <- mean(resDf$resi)
  degF <- broom::glance(glm_object)$df.null
  t.value  <- abs(mean/stDev)
  p.value <- dt(t.value, df=degF)
  tResult <- cbind("Residual mean"=mean, "t-value"=t.value, "p-value"=p.value)
  rownames(tResult) <- c("")
  cat(paste("$ `t-Test for residual mean zero`", "\n"))
  print(tResult)
  cat(paste("Alternative hypothesis: true mean is not equal to 0", "\n"))
}
