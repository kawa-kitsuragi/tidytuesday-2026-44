packages <- c("tidyverse", "tidytuesdayR", "glue")

lapply(packages, \(x) library(x, character.only = TRUE))

tuesdata <- tt_load(2026, week = 44)

glimpse(tuesdata$grambank_languages)

View(tuesdata$grambank_parameters)

# Some cleanup and factor-izing of the language data
tuesdata$grambank_languages <- tuesdata$grambank_languages |>
  mutate(macroarea = as_factor(macroarea),
         language_fam = fct_lump_n(family_name, 30, other_level = "Other")) |>
  mutate(language_fam = fct_na_value_to_level(language_fam, 
                                              level = "Unknown family")) |>
  mutate(macroarea = fct_na_value_to_level(macroarea, 
                                           level = "Unknown region"))

# GB030 is a logical parameter considering gender in third person pronouns
# like English (he/she/they)

third_gend_lang_data <- tuesdata$grambank_languages |>
  inner_join(tuesdata$grambank_values, by = join_by(language_id)) |>
  filter(parameter_id == "GB030") |>
  select(language_id,
         language_name,
         macroarea,
         latitude,
         longitude,
         language_fam,
         value) |>
  mutate(has_gendered_third = as.logical(value)) |>
  select(!value) 
  
# Actually, let's make this a function for any ID

get_values <- function(param_id) {
  df <- tuesdata$grambank_languages |>
    inner_join(tuesdata$grambank_values, by = join_by(language_id)) |>
    filter(parameter_id == param_id) |>
    select(value) |>
    rename_with(~param_id, value)
}

params_about_gender <- tuesdata$grambank_parameters |>
  filter(str_detect(feature_name, "gender"))

# 14, which is maybe bigger than I'm really interested in, but that's okay

values_gen <- map(params_about_gender$parameter_id,
                  \(param_id) get_values(param_id)) |>
  unnest_wider(values_gen)