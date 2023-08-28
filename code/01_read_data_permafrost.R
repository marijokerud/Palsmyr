#LOAD DATA
library(readxl)
library(xlsx)
library(tidyverse)
library(labdsv)

Sys.setlocale()
Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

info_dataset <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_info.xlsx", sheet = "Pals-lok-info", col_names = TRUE)

#IMPORT DATA
permafrostdepth_raw <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "Teledyp", col_names = TRUE)
ant_sprekker <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "antall-sprekker", col_names = TRUE)
bredde_sprekker <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "bredde-sprekker", col_names = TRUE)

#CLEAN DATA SET
permafrostdepth <- permafrostdepth_raw %>% 
  rename(area= "Omr-info", site=Navn, year=År, line="Lj-nr", segment="Linje-del", depth ="Teledyp", height = "Høyde") %>% 
  unite("lineID", line:segment, sep = ".", remove = FALSE) %>% 
  select(site, year, lineID, line, segment, depth, height)

