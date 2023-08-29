#LOAD DATA
library(readxl)
library(xlsx)
library(tidyverse)
library(labdsv)

Sys.setlocale()
Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

info_dataset <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_info.xlsx", sheet = "Pals-lok-info", col_names = TRUE)

#IMPORT DATA
structureline <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "Markslag", col_names = TRUE)
layerPSL <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "Sjikt", col_names = TRUE)


#CLEAN DATA SET
structureline %>%
  select(Markslag) %>% 
  unique()

#Create matrix for pals
palsstructurePSL1 <- matrix(rep(c("palsring", "palsring",
                                  "myrflate", "myrflate",
                                  "palsplatå", "palsplatå", 
                                  "dam", "dam", 
                                  "brottkant", "brottkant", 
                                  "lagg", "lagg", 
                                  "tue", "myrflate", 
                                  "pals", "pals", 
                                  "pøl", "pøl", 
                                  "nypals", "pals", 
                                  "myrflate/palsring", "myrflate", 
                                  "dam-flik", "dam", 
                                  "NA", "NA", 
                                  "myrflate, lagg", "myrflate", 
                                  "palskant", "pals", 
                                  "NN", "NN"), times=1), ncol=2, byrow=TRUE)
colnames(palsstructurePSL1) <- c('Markslag', 'palsstructure1')
palsstructurePSL1 <- as.data.frame(palsstructurePSL1)

palsstructurePSL2 <- matrix(rep(c("palsring", "pals",
                                  "myrflate", "myr",
                                  "palsplatå", "pals", 
                                  "dam", "dam", 
                                  "brottkant", "pals", 
                                  "lagg", "myr", 
                                  "tue", "myr", 
                                  "pals", "pals", 
                                  "pøl", "pals", 
                                  "nypals", "pals", 
                                  "myrflate/palsring", "myr", 
                                  "dam-flik", "dam", 
                                  "NA", "NA", 
                                  "myrflate, lagg", "myr", 
                                  "palskant", "pals", 
                                  "NN", "NN"), times=1), ncol=2, byrow=TRUE)
colnames(palsstructurePSL2) <- c('Markslag', 'palsstructure2')
palsstructurePSL2 <- as.data.frame(palsstructurePSL2)

structurelinePSL <- structureline %>% 
  rename(area= "Omr-info", site=Navn, year=År, line="Lj-nr", segment="Linje-del") %>% 
  unite("lineID", line:segment, sep = ".", remove = FALSE) %>% 
  filter(!row_number() %in% c(7419)) %>%  #remove lineID=32.151 in 2019 because of NA
  left_join(palsstructurePSL1, by = "Markslag") %>% 
  left_join(palsstructurePSL2, by = "Markslag") %>% 
  mutate(overdisp = 1:13378)
  #mutate(pals = rep(c("pals", "yes", "myr", "no")))
  #select(site, year, line) %>% 
  #unique()

