# Load required packages
if (!require("checkpoint")) install.packages("checkpoint"); library(checkpoint)

# Use the date few weeks after R-3.6.1. was released
checkpoint("2019-07-28", R.version = "3.6.1",
           checkpointLocation = Sys.getenv("USERPROFILE"))

# Load libraries
library(dplyr)
library(tidyr)
library(vroom)
library(stringr)
library(janitor)
library(devtools)

# Load postinumbers data
fi_postnumbers_2016 <- vroom(
  "./data-raw/postnumbers_2016.txt", skip = 4,
  col_types = cols(.default = col_character()),
  .name_repair = janitor::make_clean_names
  ) %>% {
    # Shorten postnumbers to first three characters
    bind_rows(
      mutate(., postinumero = paste0(str_sub(postinumeroalue, 1, 2), "0")) %>%
        distinct(postinumero, .keep_all = TRUE),
      mutate(., postinumero = str_sub(postinumeroalue, 1, 3)))
    } %>%
  # Add new area variable by Maakunta
  mutate_at(vars(maakunta), list(alue = ~case_when(
    . %in% c("01", "05", "07", "08") ~ "Etelä-Suomi",
    . %in% c("02", "04", "06", "13", "14", "15", "16") ~ "Länsi-Suomi",
    . %in% c("09", "10", "11", "12") ~ "Itä-Suomi",
    . %in% c("17", "18", "19") ~ "Pohjois-Suomi",
    TRUE ~ "Tuntematon"))) %>%
  # Select
  select(
    "postinumero", "maakunta", "maakunnan_nimi", "kuntaryhma",
    "kuntaryhman_nimi", "seutukunta", "seutukunnan_nimi", "suuralue",
    "suuralueen_nimi", "alue"
    ) %>%
  distinct(postinumero, .keep_all = TRUE)

# Load industries data
fi_industries_2008 <- vroom(
  "./data-raw/industries_2008.txt", skip = 3,
  .name_repair = janitor::make_clean_names) %>%
  filter(taso %in% c(1, 2)) %>%
  mutate(
    tol_1 = if_else(taso == 1, koodi, NA_character_),
    tol_1_name = if_else(taso == 1, nimike, NA_character_)) %>%
  fill(tol_1, tol_1_name) %>%
  filter(taso == 2) %>%
  rename(tol_2 = koodi, tol_2_name = nimike) %>%
  mutate(tol_0 = case_when(
      tol_1 %in% c("A", "B") ~ "A+B",
      tol_1 %in% c("D", "E") ~ "D+E",
      tol_1 %in% c("J", "L", "M", "N") ~ "J+L+M+N",
      TRUE ~ tol_1)) %>%
  select(tol_0, tol_1, tol_1_name, tol_2, tol_2_name)

# Load people names data
fi_people_names <- vroom(
  "./data-raw/people_names.txt",
  .name_repair = janitor::make_clean_names
    ) %>%
  # Turn n from character to integer
  mutate_at(vars(n), ~(str_replace_all(., " ", "") %>% as.integer))

# Save data to use in package: Jmisc
use_data(fi_postnumbers_2016, overwrite = TRUE)
use_data(fi_people_names, overwrite = TRUE)
use_data(fi_industries_2008, overwrite = TRUE)
