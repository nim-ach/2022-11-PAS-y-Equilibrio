
# Import libraries ----------------------------------------------------------------------------

library(data.table)

# Download the dataset from google sheet ------------------------------------------------------

riskfall <- data.table::fread(input = "data-raw/raw_data.csv")

comma_to_dot <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(
      gsub(pattern = "\\,", replacement = "\\.", x = x)
    )
  }
  return(x)
}

riskfall[height < 10, height := height * 100]
riskfall[weight > 200, weight := weight/10]
riskfall[, bmi := round(weight/(height/100)^2, 2)]

riskfall <- within(riskfall, {
  # Body mass index to factor
  bmi_category = data.table::fcase(tolower(bmi_category) %like% "insuf", "Infrapeso",
                        tolower(bmi_category) %like% "norm", "Normal",
                        tolower(bmi_category) %like% "sobre", "Sobrepeso",
                        tolower(bmi_category) %like% "obesi", "Obesidad") |>
    factor(levels = c("Infrapeso", "Normal", "Sobrepeso", "Obesidad"))

  # Change comma decimal character to dot decimal numeric
  bp_pam = comma_to_dot(bp_pam)

  # Sex to factor
  sex = factor(sex)

  # Remove empty level of sit-to-stand category
  sit_to_stand_cat = factor(sit_to_stand_cat, levels = c("limitacion minima", "limitacion leve"))

  # `id` to factor
  id = factor(id)

  # Remove names
  rm(first_name, last_name)
})

# Export data ---------------------------------------------------------------------------------

save(riskfall, file = "data/riskfall.rda", ascii = FALSE, version = 2)

