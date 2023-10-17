
# Preparamos el entorno de trabajo ----------------------------------------

## Cargamos los datos
data("riskfall")

## Cargamos librerias
library(data.table)

ind <- grep(pattern = "altered|corrected", 
            invert = TRUE, 
            value = TRUE,
            x = grep(pattern = "posturography|hsps|sex|^age|eight$|bmi",
                     x = names(riskfall), 
                     value = TRUE))

muestra <- riskfall[, .SD, .SDcols = ind
][, sex := `levels<-`(sex, c("Female", "Male"))][]

remove(ind, riskfall)


# Realizamos tablas -------------------------------------------------------


ec_mod <- muestra[posturography_cat_ec != 2, glm(factor(posturography_cat_ec) ~ hsps_score, binomial)]
eo_mod <- muestra[posturography_cat_eo != 2, glm(factor(posturography_cat_eo) ~ hsps_score, binomial)]

parameters::model_parameters(ec_mod, exponentiate = T)
parameters::model_parameters(eo_mod, exponentiate = T)


ec_tab <- gtsummary::tbl_regression(ec_mod, exponentiate = TRUE, intercept = TRUE)
eo_tab <- gtsummary::tbl_regression(eo_mod, exponentiate = TRUE, intercept = TRUE)
