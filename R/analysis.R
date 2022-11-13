
# load libraries and data ---------------------------------------------------------------------

library(data.table)
library(dplyr)

data("riskfall")

# Explore dataset -----------------------------------------------------------------------------

ind <- grep(
  pattern = "altered", invert = TRUE, value = TRUE,
  x = grep(pattern = "posturography|pss|hsps", x = names(riskfall), value = TRUE)
)

transpose(l = riskfall[j = lapply(.SD, function(i) shapiro.test(i)$p.value), .SDcols = ind],
          keep.names = "vars")[V1 > 0.05]

cors <- correlation::correlation(riskfall, p_adjust = "none", method = "spearman")

cors <- as.data.table(cors)

# Posturometría, Estrés percibido (PSS) y Puntaje del "persona con alta sensibilidad" (HSPS)

cors[
  i = (Parameter1 %like% "posturography" | Parameter2 %like% "posturography") &
       ((Parameter1 %like% "pss" | Parameter2 %like% "pss") |
          (Parameter1 %like% "hsps" | Parameter2 %like% "hsps")),
  j = .(Parameter1, Parameter2, rho, p, n_Obs)
][i = order(p) & p < 0.05]

#  Hallazgos
#
#  -  La clasificación para la posturografía con ojos cerrados se correlacionó
#     negativamente con el puntaje de HSPS, tanto con Pearson y Spearman (recomendado por no-normalidad).
#  -  No se observaron otros hallazgos entre los parámetros de posturografía,
#     PSS y HSPS.

riskfall[, posturography_cat_ec := factor(posturography_cat_ec)][]

ordinal_model <- MASS::polr(posturography_cat_ec ~ hsps_score, riskfall, Hess = TRUE)
model_significance <- summary(ordinal_model) |> coefficients() |> as.data.frame()
model_significance$p_value <-  pnorm(model_significance[,"t value"]) * 2
print(model_significance)

predictor <- seq(
  from = min(riskfall$hsps_score, na.rm = TRUE),
  to = max(riskfall$hsps_score, na.rm = TRUE),
  by = 0.05
)
probs <- marginaleffects::predictions(ordinal_model,
        newdata = data.frame(hsps_score = predictor),
        conf_level = .95,
        type = "probs")

probs <- data.table(probs)

library(ggplot2)
ggplot(probs, aes(hsps_score, predicted, fill = group)) +
  facet_grid(cols = vars(group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3) +
  geom_line(aes(col = group)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ggsci::scale_color_d3() +
  ggsci::scale_fill_d3() +
  labs(x = "HSPS Score", y = "P( Category | HSPS Score )",
       fill = "Category", col = "Category",
       title = "Postural instability and HSPS Score",
       subtitle = paste(strwrap("Probability of being in a certain category of postural stability on the eyes closed assessment for a given HSPS score.", 80), collapse = "\n")) +
  ggdist::theme_ggdist()

ggplot(probs, aes(hsps_score, predicted, fill = factor(group))) +
  # facet_grid(cols = vars(group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3, position = "fill") +
  # geom_line(aes(col = group)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ggsci::scale_color_d3() +
  ggsci::scale_fill_d3() +
  labs(x = "HSPS Score", y = "P( Category | HSPS Score )",
       fill = "Category", col = "Category",
       title = "Postural instability and HSPS Score",
       subtitle = paste(strwrap("Probability of being in a certain category of postural stability on the eyes closed assessment for a given HSPS score.", 80), collapse = "\n")) +
  ggdist::theme_ggdist()

