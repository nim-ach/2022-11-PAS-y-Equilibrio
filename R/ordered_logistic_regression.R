
# load libraries and data ---------------------------------------------------------------------

library(data.table)

data("riskfall")

# Explore dataset -----------------------------------------------------------------------------

riskfall[, posturography_cat_ec := factor(posturography_cat_ec)][]

ordinal_model <- MASS::polr(
  formula = posturography_cat_ec ~ hsps_score, 
  data = riskfall, 
  Hess = TRUE)

model_significance <- ordinal_model |> 
  summary() |> 
  coefficients() |> 
  as.data.frame()

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
        type = "probs") |> 
  data.table()


# Visualize models --------------------------------------------------------

.plot = FALSE

if (.plot) {

  ## Load libraries
  
  library(ggplot2)
  
  ## Conditional probability
  
  ggplot(probs, aes(hsps_score, estimate, fill = group)) +
    #facet_grid(cols = vars(group)) +
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
  
  ## Cummulative probability
  
  ggplot(probs, aes(hsps_score, estimate, fill = factor(group))) +
    # facet_grid(cols = vars(group)) +
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
  
}

