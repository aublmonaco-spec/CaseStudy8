#' ---
#' title:  "cs08: Statistical Modeling and Reproducible Output"
#' subtitle: "Linear Regression on Global Spatial Data & Creating Reproducible Examples"
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

# Load the `world` dataset and prepare the variables. We log-transform GDP per capita (`gdpPercap`) and remove missing values (`NA`) to facilitate the linear regression fit.
# Use select() to keep the following columns (name_long, lifeExp, gdpPercap, continent, and geom).
# Use mutate() and log() to create a new column, log_gdp, that is the log of gdpPercap.
# Use drop_na() specifying lifeExp and log_gdp to remove observations with missing values in these key analysis columns.

world <- world
world_new <- world %>%
  select(name_long, lifeExp, gdpPercap, continent, geom) %>%
  mutate(log_gdp = log(gdpPercap)) %>%
  drop_na(lifeExp, log_gdp)

# Fit a linear model (`lm`) to predict `lifeExp` using `log_gdp`.
# Use lm() with the formula lifeExp ~ log_gdp and specify data = world_data.

lm_model <- lm(lifeExp ~ log_gdp, data = world_new)

# Use tidy(lm_model) to summarize model output for readability.

tidy_model <- tidy(lm_model)

# Use knitr::kable()` to display the final table, specifying caption and digits=2

kable(tidy_model, caption = "Linear Regression Model Summary: Life Expectancy vs. Log GDP", digits = 2)

# Use `world_new` and pipe it to `mutate()` to create 
#'     1. `predicted_lifeExp` column using `predict(lm_model, world_data)`
#'     2. `residuals` column using `residuals(lm_model)`
   
world_new <- world_new %>%
  mutate(predicted_lifeExp = predict(lm_model, world_new)) %>%
  mutate(residuals = residuals(lm_model))

# Used`ggplot()` mapping `log_gdp` to `x`, `lifeExp` to `y`, and `continent` to `color`, added geom_point()` for the observations and `geom_smooth(method = "lm")` to add the fitted linear model line. Can use se = TRUE to see the standard error on the regression

ggplot(data = world_new, aes(x = log_gdp, y = lifeExp)) + geom_point(aes(color = continent)) + geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black") + labs(title = "Relationship Between Life Expectancy and Log GDP Per Capita", y = "Life Expectancy (Years)", x = "Log(GDP Per Capita)", color = "Continent")

# Used world_new to create a map with geom_sf, and specified the color palette with scale_fill_continuous

ggplot() + geom_sf(data = world_new, aes(fill = residuals)) + scale_fill_continuous(palette = "viridis")

#' ## 4. Illustrating a Problem with `reprex`
 
#' 1.  **Identify the problematic code.** (Here, it is the code generating the unintended density plot).
#' 2.  **Ensure necessary setup is included.** The code must include loading the required packages (`tidyverse` and `spData`) and the data (`data(world)`).
#' 3.  **Copy the code snippet to the clipboard.**
#' 4.  **Run `reprex()`** in the console (e.g., `reprex(venue="html")` or `reprex(venue="github")`) to generate the formatted, reproducible output.
