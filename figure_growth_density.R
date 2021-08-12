
# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------

source("format_fishdata.R")


# figure ------------------------------------------------------------------

df_growth %>% 
  ggplot() +
  geom_point(mapping = aes(x = redbreast_sunfish,
                           y = growth)) +
  facet_grid(rows = vars(growth_occasion),
             cols = vars(Species)) +
  theme_bw()
  
  
