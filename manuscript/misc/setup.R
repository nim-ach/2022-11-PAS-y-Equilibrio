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
][, sex := `levels<-`(sex, c("Mujer", "Hombre"))][]

remove(ind, riskfall)
