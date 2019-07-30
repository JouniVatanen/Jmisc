library(tidyverse)
library(vroom)

df.postno <- vroom("./data-raw/postnumber_2016.txt", skip = 4,
                   col_types = cols(.default = col_character())) %>% {
                     bind_rows(
                       mutate(., PN = paste0(str_sub(Postinumeroalue, 1, 2), "0")) %>%
                         distinct(PN, .keep_all = TRUE),
                       mutate(., PN = str_sub(Postinumeroalue, 1, 3)))} %>%
  mutate_at(vars(Maakunta), list(Alue = ~case_when(
    . %in% c("01","05","07","08") ~ "Etelä-Suomi",
    . %in% c("02","04","06","13","14","15","16") ~ "Länsi-Suomi",
    . %in% c("09","10","11","12") ~ "Itä-Suomi",
    . %in% c("17","18","19") ~ "Pohjois-Suomi",
    TRUE ~ "Tuntematon"))) %>%
  rename(Maakunnan.nimi = `Maakunnan nimi`) %>%
  select("Maakunta", "Maakunnan.nimi", "Alue", "PN") %>%
  distinct(PN, .keep_all = TRUE)

use_data(df.postno, internal = TRUE)
