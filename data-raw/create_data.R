# Load libraries
library(dplyr)
library(vroom)
library(stringr)
library(janitor)
library(devtools)

# Load postinumbers data
fi_postnumbers_2016 <- vroom(
  "./data-raw/postnumber_2016.txt", skip = 4,
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
