packages <- FALSE

if (packages){
  install.packages('tidyverse')
  install.pakages('WDI')
  install.packages('wid')
}
library(WDI)
library(tidyverse)
library(wid)
gini <- WDI(
  countries <- c('IN','SA')
)
