library(palmerpenguins)
library(stargazer)
library(gt)
library(gtsummary)

library(psych)
describe(penguins) |> 
  rownames_to_column() |> 
  select(-c(vars)) |> 
  gt()

model1 <- penguins |>
  lm(formula = flipper_length_mm ~ body_mass_g) |> 
  tbl_regression()  |> 
  bold_p(t = 0.10) |> 
  add_glance_table(include = c(nobs, r.squared))

model2 <- penguins |>
  lm(formula = flipper_length_mm ~ body_mass_g + bill_length_mm) |> 
  tbl_regression()  |> 
  bold_p(t = 0.10) |> 
  add_glance_table(include = c(nobs, r.squared))

model3 <- penguins |>
  lm(formula = flipper_length_mm ~ body_mass_g + bill_length_mm + bill_depth_mm) |> 
  tbl_regression()  |> 
  bold_p(t = 0.10) |> 
  add_glance_table(include = c(nobs, r.squared))

tbl_merge(
  tbls = list(model1, model2, model3),
  tab_spanner = c("Model 1", "Model 2", "Model 3")) |> 
  modify_table_body(~.x %>% arrange(row_type == "glance_statistic"))

