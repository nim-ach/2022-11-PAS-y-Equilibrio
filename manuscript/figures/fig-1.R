
# preparamos el espacio de trabajo ----------------------------------------

library(ggplot2)

source("manuscript/misc/setup.R")

# create plot -------------------------------------------------------------

mod <- muestra[posturography_cat_ec != 2, glm(factor(posturography_cat_ec) ~ hsps_score, binomial)]

predictor <- seq(
  from = min(muestra$hsps_score, na.rm = TRUE),
  to = max(muestra$hsps_score, na.rm = TRUE),
  by = 0.05
)

probs <- predict(mod, newdata = data.frame(hsps_score = predictor), type = "link", se.fit = TRUE)[-3L] |> 
  as.data.table() |> 
  cbind(data.frame(hsps_score = predictor))

ci_prob <- function(p) stats::qnorm((p+1)/2, lower.tail = TRUE)

a <- ggplot(probs, aes(hsps_score, plogis(fit))) +
  geom_ribbon(aes(ymin = plogis(fit - se.fit * ci_prob(p = .95)),
                  ymax = plogis(fit + se.fit * ci_prob(p = .95)),
                  fill = "95%")) +
  geom_ribbon(aes(ymin = plogis(fit - se.fit * ci_prob(p = .80)),
                  ymax = plogis(fit + se.fit * ci_prob(p = .80)),
                  fill = "80%")) +
  geom_ribbon(aes(ymin = plogis(fit - se.fit * ci_prob(p = .5)),
                  ymax = plogis(fit + se.fit * ci_prob(p = .5)),
                  fill = "50%")) +
  geom_line(col = "white") +
  geom_hline(yintercept = .5, lty = 2) +
  scale_fill_manual(values = c("95%" = "#FDE0DD", "80%" = "#FA9FB5", "50%" = "#C51B8A"), 
                    aesthetics = c("col", "fill")) +
  ggdist::theme_ggdist() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "HSPS score", y = "Probability",
       fill = "CI")

b <- ggplot(probs, aes(hsps_score, exp(fit))) +
  geom_ribbon(aes(ymin = exp(fit - se.fit * ci_prob(p = .95)),
                  ymax = exp(fit + se.fit * ci_prob(p = .95)),
                  fill = "95%")) +
  geom_ribbon(aes(ymin = exp(fit - se.fit * ci_prob(p = .80)),
                  ymax = exp(fit + se.fit * ci_prob(p = .80)),
                  fill = "80%")) +
  geom_ribbon(aes(ymin = exp(fit - se.fit * ci_prob(p = .5)),
                  ymax = exp(fit + se.fit * ci_prob(p = .5)),
                  fill = "50%")) +
  geom_line(col = "white") +
  scale_fill_manual(values = c("95%" = "#FDE0DD", "80%" = "#FA9FB5", "50%" = "#C51B8A"), 
                    aesthetics = c("col", "fill")) +
  ggdist::theme_ggdist() +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = "HSPS score", y = "OR",
       fill = "CI")

fig <- ggpubr::ggarrange(a, b, ncol = 2, labels = c("A.", "B."), common.legend = TRUE)

print(fig)

ggsave("manuscript/figures/fig-1.pdf", plot = fig, width = 8, height = 8)
ggsave("manuscript/figures/fig-1.jpeg", plot = fig, width = 8, height = 8, dpi = 300)
