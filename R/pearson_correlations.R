
# load libraries and data ---------------------------------------------------------------------

library(data.table)
library(dplyr)

data("riskfall")

# Explore dataset -----------------------------------------------------------------------------

ind <- grep(
  pattern = "altered", invert = TRUE, value = TRUE,
  x = grep(pattern = "posturography|hsps", x = names(riskfall), value = TRUE)
)

transpose(l = riskfall[j = lapply(.SD, function(i) shapiro.test(i)$p.value), .SDcols = ind],
          keep.names = "vars")[V1 > 0.05]

cors <- correlation::correlation(riskfall[, ..ind], p_adjust = "none", method = "pearson")

cors <- as.data.table(cors)

# Posturometría, Estrés percibido (PSS) y Puntaje del "persona con alta sensibilidad" (HSPS)

cors[
  i = (Parameter1 %like% "posturography" | Parameter2 %like% "posturography") &
       ((Parameter1 %like% "pss" | Parameter2 %like% "pss") |
          (Parameter1 %like% "hsps" | Parameter2 %like% "hsps")),
  j = .(Parameter1, Parameter2, r, p, n_Obs)
][i = order(abs(r))]
#>              Parameter1 Parameter2          r         p n_Obs
#> 1: posturography_cat_ec hsps_score -0.3539526 0.0214678    42

#  Hallazgos
#
#  -  La clasificación para la posturografía con ojos cerrados se correlacionó
#     negativamente con el puntaje de HSPS, tanto con Pearson.
#  -  No se observaron otros hallazgos entre los parámetros de posturografía,
#     PSS y HSPS.
