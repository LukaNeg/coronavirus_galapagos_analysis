#####################################
#### LOAD PACKAGES AND FUNCTIONS ####
#####################################

### Load packages:
library(tidyverse)
library(lubridate)

##############################
### Load custom functions: ###

### Transparent colors function:
t_col <- function(color, percent = 50) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3], max = 255, alpha = (100-percent)*255/100)
  invisible(t.col)
}

### function to standardize continuous variables for effect size comparison to logical variables
### and to ensure standardization within each species for global comparison
### https://www.listendata.com/2017/04/how-to-standardize-variable-in-regression.html
standard <- function(x) (x - mean(x, na.rm=T)) / (2*sd(x, na.rm=T))
