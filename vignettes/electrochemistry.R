## ----eval=FALSE, include=TRUE--------------------------------------------
#  install.packages('devtools')
#  devtools::install_github('SPOMAN/osc')

## ------------------------------------------------------------------------
library(osc)
# The file variable should contain the path to your datafile
file <- system.file('extdata/cv/cv_example.txt', package = 'osc')
df <- echem_read(file)

## ------------------------------------------------------------------------
df

## ----fig.width = 7-------------------------------------------------------
plot(df)

## ----fig.width = 7-------------------------------------------------------
df <- area(df, sw = 1, x1 = -1.85, x2 = -1.4)
plot_area(df)

## ----fig.width = 7-------------------------------------------------------
file <- system.file('extdata/cv/5cv_example.txt', package = 'osc')
df <- echem_read(file)
plot(df)

## ----fig.width = 7-------------------------------------------------------
library(tidyverse)

df$data %>% 
  ggplot(aes(potential, current, color = sweep)) +
  ggthemes::theme_few() +
  geom_path()

## ----fig.width = 7-------------------------------------------------------
file <- system.file('extdata/cv/Electrolysis.txt', package = 'osc')
df <- echem_read(file)
plot(df)

