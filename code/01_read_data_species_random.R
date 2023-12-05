#LOAD DATA
library(readxl)
library(xlsx)
library(tidyverse)
library(labdsv)

Sys.setlocale()
Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

#info_dataset <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_info.xlsx", sheet = "Pals-lok-info", col_names = TRUE)

#IMPORT DATA
speciesline.raw <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Arter_artlinje", col_names = TRUE)
#speciesline.random <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Arltl_sjikt", col_names = TRUE) #dominerende art i hvert punkt
structureline.random <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Artl_Markslag", col_names = TRUE)
new.name <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "new_name", col_names = TRUE)


#RANDOM STUCTURE LINE
structureline.random %>%
  select(Markslag) %>% 
  unique()

#Create matrix for pals
palsstructureSP <- matrix(rep(c("brottkant", "myrflate", 
                                  "dam", "dam",
                                  "lagg", "myrflate",
                                  "myrflate", "myrflate",
                                  "pals", "pals",
                                  "Pals", "pals",
                                  "pals/myrflate", "myrflate",
                                  "palsplatå", "pals",
                                  "palsring", "myrflate",
                                  "palsring/lagg", "myrflate",
                                  "pøl", "myrflate",
                                  "Pøl", "myrflate",
                                  "tue", "myrflate"), times=1), ncol=2, byrow=TRUE)
colnames(palsstructureSP) <- c('Markslag', 'palsstructure')
palsstructureSP <- as.data.frame(palsstructureSP)

palslineSP <- structureline.random %>% 
  rename(area= Omr.info, Site=Navn, Year=År, line=Lj.nr, segment=Linje.del) %>% 
  mutate(site = case_when(Site == 'Dovre, Haukskardmyrin' ~ "DovreHaukskardmyrin",
                          Site == 'Dovre, Haugtjørnin' ~ "DovreHaugtjørnin",
                          .default = as.character(Site))) %>% 
  unite("lineID", line:segment, sep = ".", remove = FALSE) %>% 
  left_join(palsstructureSP, by = "Markslag") %>% 
  filter(palsstructure=="pals") %>% 
  select(site, lineID, palsstructure)


#CLEAN DATA SET
#Extract values in Art
species.name <- speciesline.raw %>% 
  select(Art) %>% 
  distinct() 


new.name <- new.name %>% 
  mutate(Art = str_replace_all(string = Art, 
                               pattern = "[[:space:]]", 
                               replacement = " ")) %>%  #Art has a non-breaking space
  mutate(species = str_replace_all(string = species, 
                                   pattern = "[[:space:]]", 
                                   replacement = " "))

#ADD NEW variables
speciesline <- speciesline.raw %>%
  rename(area= Omr.info, Site=Navn, Year=År, line=Lj.nr, segment=Linje.del, pinpoint=Punkt) %>% 
  unite("lineID", line:segment, sep = ".", remove = FALSE) %>%
  unite("pinpointID", line:pinpoint, sep = ".", remove = FALSE) %>% 
  left_join(new.name, by = "Art") %>% 
  left_join(palslineSP, by ="lineID") %>% 
  select(area, site, Year, lineID, pinpointID, line, segment,pinpoint, species, type, stratum, functional_type, palsstructure) %>% 
  filter(palsstructure == "pals") %>% 
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
  mutate(field = case_when(stratum == 'field' ~ 1,
                          stratum == 'bottom' ~ 0,
                          stratum == 'shrub' ~ 0)) %>% 
  mutate(bottom = case_when(stratum == 'field' ~ 0,
                         stratum == 'bottom' ~ 1,
                         stratum == 'shrub' ~ 0)) %>% 
  mutate(shrub = case_when(stratum == 'field' ~ 0,
                         stratum == 'bottom' ~ 0,
                         stratum == 'shrub' ~ 1)) %>% 
  mutate(overdisp = 1:7750) 

write.xlsx(speciesline, "data/speciesline.xlsx")




### LEFT OVER CODE
species.name.fixed <- speciesline.fixed %>% 
  select(Artgr) %>% 
  distinct()

#Create matrix for pals
palsstructureSP <- matrix(rep(c("pals", "pals", "Pals", "pals", "palsplatå", "pals"), times=1), ncol=2, byrow=TRUE)
colnames(palsstructureSP) <- c('Markslag', 'palsstructure')
palsstructureSP <- as.data.frame(palsstructureSP)

palslineSP <- palslineSP %>% 
  rename(area= Omr.info, site=Navn, year=År, line=Lj.nr, segment=Linje.del) %>% 
  unite("lineID", line:segment, sep = ".", remove = FALSE)

palslineSP <- palslineSP %>% 
  left_join(palsstructureSP, by = "Markslag") %>% 
  select(lineID, palsstructure) %>% 
  unique()

unite("lineID", line:segment, sep = ".", remove = FALSE) %>% 


#Did not need after all, use Sys.setlocale to handle ÆØÅ
#Remove special characters and non-breaking space
new.name <- new.name %>% 
  mutate(Art = str_replace_all(string = Art, 
                               pattern = "[\u00f8]", 
                               replacement = "o")) #Special char Ø in Art needs to be written to o.

















