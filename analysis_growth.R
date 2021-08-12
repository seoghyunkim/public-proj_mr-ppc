# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------

source("format_fishdata.R")


# analysis ----------------------------------------------------------------

fit_gfs <- lm(growth ~ green_sunfish + redbreast_sunfish + growth_occasion,
              data = filter(df_growth, Species == "green_sunfish"))

summary(fit_gfs)


fit_rbs <- lm(growth ~ green_sunfish + redbreast_sunfish + growth_occasion,
              data = filter(df_growth, Species == "redbreast_sunfish"))

summary(fit_rbs)
