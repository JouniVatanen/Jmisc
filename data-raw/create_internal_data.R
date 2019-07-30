# Load libraries
library(tidyverse)
library(vroom)
library(devtools)
library(data.table)

# Load postinumero data
df.postno <- vroom::vroom(
  "./data-raw/postnumber_2016.txt", skip = 4,
  col_types = cols(.default = col_character())
  ) %>% {
    bind_rows(
      mutate(., Postinumero = paste0(str_sub(Postinumeroalue, 1, 2), "0")) %>%
        distinct(Postinumero, .keep_all = TRUE),
      mutate(., Postinumero = str_sub(Postinumeroalue, 1, 3)))
    } %>%
  mutate_at(vars(Maakunta), list(Alue = ~case_when(
    . %in% c("01", "05", "07", "08") ~ "Etelä-Suomi",
    . %in% c("02", "04", "06", "13", "14", "15", "16") ~ "Länsi-Suomi",
    . %in% c("09", "10", "11", "12") ~ "Itä-Suomi",
    . %in% c("17", "18", "19") ~ "Pohjois-Suomi",
    TRUE ~ "Tuntematon"))) %>%
  rename(
    Maakunnan.nimi = `Maakunnan nimi`,
    Kuntaryhman.nimi = `Kuntaryhmän nimi`,
    Seutukunnan.nimi = `Seutukunnan nimi`,
    Kuntaryhma = Kuntaryhmä,
    Suuralueen.nimi = `Suuralueen nimi`) %>%
  select(
    "Maakunta", "Maakunnan.nimi", "Kuntaryhma", "Kuntaryhman.nimi",
    "Seutukunta", "Seutukunnan.nimi", "Suuralue", "Suuralueen.nimi",
    "Alue", "Postinumero"
    ) %>%
  distinct(Postinumero, .keep_all = TRUE) %>%

  # Save to data.table and setkey
  data.table() %>%
  setkey(Postinumero)

# Save to use in package: Jmisc:::df.postno
use_data(df.postno, internal = TRUE, overwrite = TRUE)
