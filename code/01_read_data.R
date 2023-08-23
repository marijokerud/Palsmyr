#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)

Sys.setlocale()

info_dataset <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_info.xlsx", sheet = "Pals-lok-info", col_names = TRUE)

artslinjer_raw <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Arter_artlinje", col_names = TRUE)
new_name <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "new_name", col_names = TRUE) #, encoding ="UTF-8"
markslaglinjer <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Artl_Markslag", col_names = TRUE)
sjiktlinjer <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Arltl_sjikt", col_names = TRUE)

strukturlinjer <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = " ", col_names = TRUE)
teledybdelinjer <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "Teledyp", col_names = TRUE)
ant_sprekker <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "antall-sprekker", col_names = TRUE)
bredde_sprekker <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "bredde-sprekker", col_names = TRUE)

#CLEAN DATA SET
#Extract values in Art
species_name <- artslinjer_raw %>% 
  select(Art) %>% 
  distinct() 

#Remove special characters and non-breaking space
new_name <- new_name %>% 
  mutate(Art = str_replace_all(string = Art, 
                                           pattern = "[[:space:]]", 
                                           replacement = " ")) %>%  #Art has a non-breaking space
  mutate(species = str_replace_all(string = species, 
                               pattern = "[[:space:]]", 
                               replacement = " ")) %>%   #Art has a non-breaking space
  mutate(Art = str_replace_all(string = Art, 
                                   pattern = "[\u00f8]", 
                                   replacement = "o")) #Special char Ø in Art needs to be written to o.


#ADD NEW variables
artslinjer <- artslinjer_raw %>%
  left_join(new_name, by = "Art")



