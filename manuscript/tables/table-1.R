# Preparamos el espacio de trabajo ----------------------------------------

## Cargamos datos
source("manuscript/misc/setup.R")

## Librería para tablas
library(gtsummary)
library(gt)

## Creamos etiquetas para los nombres de las variables
var_names <- c("Sexo", "Edad", "Peso", "Altura", "IMC", "IMC categoría", 
               "Posturografía puntaje", "Posturografía categoría", 
               "Mov. AP OA", "Mov. Lat. OA", "Puntaje OA", "Categoría OA",
               "Mov. AP OC", "Mov. Lat. OC", "Puntaje OC", "Categoría OC", 
               "Mov. AP OAE", "Mov. Lat. OAE", "Puntaje OAE", "Categoría OAE",
               "Mov. AP OCE", "Mov. Lat. OCE", "Puntaje OCE", "Categoría OCE",
               "Mov. anterior %", "Mov. posterior %", "Mov. izquierda %", "Mov. derecha %", 
               "Mov. anterior cm", "Mov. posterior cm", "Mov. izquierda cm", "Mov. derecha cm", 
               "Puntaje HSPS")

## Le asignamos las etiquetas a la variable correspondiente
muestra <- mapply(FUN = `attr<-`, 
                  x = muestra, 
                  which = "label", 
                  value = var_names, 
                  SIMPLIFY = FALSE) |> 
  as.data.table()

# Construimos la tabla ----------------------------------------------------

theme_gtsummary_language("es")

tabla1 <- tbl_summary(muestra, 
                      by = "sex",
                      missing = "no",
                      statistic = list(all_continuous() ~ "{mean} ± {sd}",
                                       all_categorical() ~ "{n} ({p}%)"),
                      type = contains("cat") ~ "categorical") |> 
  add_overall() |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Sexo**") |> 
  add_difference(test = all_continuous() ~ "t.test",
                 include = grep("cat|sex", names(muestra), value = TRUE, invert = TRUE),
                 pvalue_fun = ~style_pvalue(.x, digits = 3)) |> 
  modify_header(estimate = "**Diferencia**") 

tabla1 |> 
  as_gt() |> 
  gtsave(filename = "manuscript/tables/table-1.html")

if (interactive()) {
  print(tabla1)
}

rm(var_names, tabla1)
