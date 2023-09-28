#LOAD DATA
library(readxl)
library(xlsx)
library(tidyverse)
library(labdsv)

#Sys.setlocale()
Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

#info_dataset <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_info.xlsx", sheet = "Pals-lok-info", col_names = TRUE)

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
  rename(area= "Omr-info", Site=Navn, Year=År, line="Lj-nr", segment="Linje-del") %>% 
  unite("lineID", line:segment, sep = ".", remove = FALSE) %>% 
  filter(!row_number() %in% c(7419, 10616)) %>%  #remove lineID=32.151 in 2019 because of NA
  left_join(palsstructurePSL1, by = "Markslag") %>% 
  left_join(palsstructurePSL2, by = "Markslag") %>%
  mutate(site = case_when(Site == 'Dovre, Haukskardmyrin' ~ "DovreHaukskardmyrin",
                          Site == 'Dovre, Haugtjørnin' ~ "DovreHaugtjørnin",
                          .default = as.character(Site))) %>% 
  mutate(year = case_when(Year == '2005' ~ 0,
                          Year == '2010' ~ 1,
                          Year == '2015' ~ 2,
                          Year == '2020' ~ 3,
                          Year == '2004' ~ 0,
                          Year == '2009' ~ 1,
                          Year == '2014' ~ 2,
                          Year == '2019' ~ 3, 
                          Year == '2006' ~ 0,
                          Year == '2011' ~ 1,
                          Year == '2016' ~ 2,
                          Year == '2021' ~ 3, 
                          Year == '2008' ~ 0,
                          Year == '2013' ~ 1,
                          Year == '2018' ~ 2)) %>%
  mutate(pals = case_when(palsstructure2 == 'pals' ~ 1,
                          palsstructure2 == 'myr' ~ 0,
                          palsstructure2 == 'dam' ~ 0)) %>% 
  mutate(myr = case_when(palsstructure2 == 'pals' ~ 0,
                          palsstructure2 == 'myr' ~ 1,
                          palsstructure2 == 'dam' ~ 0)) %>% 
  mutate(dam = case_when(palsstructure2 == 'pals' ~ 0,
                          palsstructure2 == 'myr' ~ 0,
                          palsstructure2 == 'dam' ~ 1)) %>% 
  mutate(overdisp = 1:13377) %>% 
  select(area, site, Year, year, lineID, line, segment, Markslag, palsstructure1, palsstructure2, pals, overdisp)

years <- structurelinePSL %>% 
  select(site, year) %>% 
  unique()

