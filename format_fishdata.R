
# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# read data ---------------------------------------------------------------

df_pit <- read_csv("data_raw/data_pit.csv")
df_non_target <- read_csv("data_raw/data_non_target.csv")


# format for density ------------------------------------------------------

df_pit_sunfish <- df_pit %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  drop_na(Species) %>% 
  group_by(Occasion, Tag_ID_cor) %>% 
  slice(which.max(Date)) %>% 
  filter(Species %in% c("redbreast_sunfish",
                        "bluegill",
                        "green_sunfish"),
         #Occasion == 2,
         Potential_error != "y",
         Mortality != "y",
         Tag_ID_cor != "a900.226001167828")

df_non_target_sunfish <- df_non_target %>% 
  mutate(Comment = ifelse(is.na(Comment),
                          yes = "none",
                          no = Comment),
         Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  drop_na(Species) %>% 
  filter(Species %in% c("RBS", "BLG", "GSF"),
         #Occasion == 2,
         Comment != "dead") %>% 
  mutate(Species = case_when(Species == "RBS" ~ "redbreast_sunfish",
                             Species == "BLG" ~ "bluegill",
                             Species == "GSF" ~ "green_sunfish"))
  
df_fish <- df_pit_sunfish %>% 
  bind_rows(df_non_target_sunfish)

df_section <- df_fish %>% 
  group_by(Occasion,
           Section,
           Species) %>% 
  summarize(abundance = n()) %>% 
  group_by(Section, Species) %>% 
  summarize(abundance = mean(abundance)) %>% 
  pivot_wider(id_cols = Section,
              names_from = Species,
              values_from = abundance,
              values_fill = 0)


# format for growth -------------------------------------------------------

df_growth <- df_pit_sunfish %>% 
  select(-Time,
         -Site,
         -Recap,
         -Mortality,
         -Potential_error,
         -Comments) %>% 
  pivot_wider(id_cols = c(Tag_ID_cor, Species),
              names_from = Occasion,
              values_from = c(Length, Date, Section)) %>%
  mutate(index1 = is.na(Length_1) + is.na(Length_2),
         index2 = is.na(Length_2) + is.na(Length_3)) %>% 
  filter(index1 == 0 | index2 == 0) %>% 
  mutate(duration_1 = as.numeric(Date_2 - Date_1),
         duration_2 = as.numeric(Date_3 - Date_2),
         growth_1 = 100 * (log(Length_2) - log(Length_1)) / duration_1,
         growth_2 = 100 * (log(Length_3) - log(Length_2)) / duration_2) %>%
  pivot_longer(cols = c(starts_with("growth")),
               names_to = "growth_occasion",
               values_to = "growth") %>%
  pivot_longer(cols = c("Section_1", "Section_2"),
               names_to = "section_occasion",
               values_to = "Section") %>%
  separate(col = c("growth_occasion"),
           into = c(NA, "growth_occasion"),
           sep = "_") %>%
  separate(col = c("section_occasion"),
           into = c(NA, "section_occasion"),
           sep = "_") %>%
  filter(growth_occasion == section_occasion) %>%
  left_join(df_section, by = "Section") %>% 
  drop_na(growth)

