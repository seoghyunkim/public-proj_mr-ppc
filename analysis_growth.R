# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------

source("format_fishdata.R")

df_growth_sk <-read_csv("./df_growth_sk.csv")
df_growth_sk$growth_occasion <- as.factor(df_growth_sk$growth_occasion)

# analysis ----------------------------------------------------------------

fit_gfs <- lm(growth ~ green_sunfish + redbreast_sunfish + growth_occasion,
              data = filter(df_growth_sk, Species == "green_sunfish"))

summary(fit_gfs)


fit_rbs <- lm(growth ~ green_sunfish + redbreast_sunfish + growth_occasion,
              data = filter(df_growth_sk, Species == "redbreast_sunfish"))

summary(fit_rbs)
