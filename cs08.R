#' ---
#' title:  "cs08: Statistical Modeling and Reproducible Output"
#' format:
#'   html: default
#' ---
# Load necessary libraries for data manipulation, spatial data, modeling, and reproducible examples
#install.packages("broom")
#install.packages("reprex")
library(tidyverse)
library(sf)
library(spData)
library(knitr)
library(reprex) # For creating reproducible examples
library(broom) # For tidying model outputs
library(viridis)
library(ggplot2)
#' 
#' ## 1. Estimate the Linear Model
#' 
#' ### 1.1 Data Preparation
#' Load the `world` dataset and prepare the variables. We log-transform GDP per capita (`gdpPercap`) and remove missing values (`NA`) to facilitate the linear regression fit.
#' 
#' 1. Load the world dataset.

world <- world
world_new <- world %>%
  select(name_long, lifeExp, gdpPercap, continent, geom) %>%
  mutate(log_gdp = log(gdpPercap)) %>%
  drop_na(lifeExp, log_gdp)

#'1.2 Model Estimation and Summary
#'Fit a linear model (`lm`) to predict `lifeExp` using `log_gdp`.

lm_model <- lm(lifeExp ~ log_gdp, data = world_new)

#' 2. Use `tidy(lm_model)` to summarize model output for readability.

tidy_model <- tidy(lm_model)

#' 3. Use `knitr::kable()` to display the final table, specifying caption and digits=2

kable(tidy_model, caption = "Linear Regression Model Summary: Life Expectancy vs. Log GDP", digits = 2)

#' 2. Visualize the Model Predictions
#' 
#' 1. Use `world_data` and pipe it to `mutate()` to create 
#'     1. `predicted_lifeExp` column using `predict(lm_model, world_data)`
#'     2. `residuals` column using `residuals(lm_model)`
#'     
world_new <- world_new %>%
  mutate(predicted_lifeExp = predict(lm_model, world_new)) %>%
  mutate(residuals = residuals(lm_model))

#' 2. Start the plot with `ggplot()` mapping `log_gdp` to `x`, `lifeExp` to `y`, and `continent` to `color`.
#' 3. Use `geom_point()` for the observations.
#' 4. Use `geom_smooth(method = "lm")` to add the fitted linear model line. If you want to see the standard error on the regression, include ` se = TRUE`.

ggplot(data = world_new, aes(x = log_gdp, y = lifeExp)) + geom_point(aes(color = continent)) + geom_smooth(method = "lm", se = TRUE)

ggplot() + geom_sf(data = world_new, aes(fill = residuals)) + scale_fill_continuous(palette = "viridis")

#' ## 4. Illustrating a Problem with `reprex`
 
#' 1.  **Identify the problematic code.** (Here, it is the code generating the unintended density plot).
#' 2.  **Ensure necessary setup is included.** The code must include loading the required packages (`tidyverse` and `spData`) and the data (`data(world)`).
#' 3.  **Copy the code snippet to the clipboard.**
#' 4.  **Run `reprex()`** in the console (e.g., `reprex(venue="html")` or `reprex(venue="github")`) to generate the formatted, reproducible output.
