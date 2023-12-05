#LOAD DATA
library(readxl)
library(xlsx)
library(tidyverse)
library(labdsv)

Sys.setlocale()
Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

info_dataset <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_info.xlsx", sheet = "Pals-lok-info", col_names = TRUE)

#IMPORT DATA
speciesline.raw <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Arter_artlinje", col_names = TRUE)
#speciesline.random <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Arltl_sjikt", col_names = TRUE)
#speciesline.fixed <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "Sjikt", col_names = TRUE)

structureline.random <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Artl_Markslag", col_names = TRUE)

new.name <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "new_name", col_names = TRUE)

#rename(area= "Omr-info", Site=Navn, Year=År, line="Lj-nr", segment="Linje-del") %>% 

#CLEAN DATA SET
#Extract values in Art
species.name <- speciesline.raw %>% 
  select(Art) %>% 
  distinct() 

species.name.fixed <- speciesline.fixed %>% 
  select(Artgr) %>% 
  distinct()

new.name <- new.name %>% 
  mutate(Art = str_replace_all(string = Art, 
                               pattern = "[[:space:]]", 
                               replacement = " ")) %>%  #Art has a non-breaking space
  mutate(species = str_replace_all(string = species, 
                                   pattern = "[[:space:]]", 
                                   replacement = " "))

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

#ADD NEW variables
speciesline <- speciesline.raw %>%
  rename(area= Omr.info, site=Navn, year=År, line=Lj.nr, segment=Linje.del, pinpoint=Punkt) %>% 
  unite("lineID", line:segment, sep = ".", remove = FALSE) %>% 
  left_join(new.name, by = "Art") %>% 
  left_join(palslineSP, by ="lineID", relationship = "many-to-one") %>% 
  select(area, site, year, lineID, line, segment,pinpoint, species, type, functional_type, palsstructure) %>% 
  filter(palsstructure == "pals")

write.xlsx(speciesline, "data/speciesline.xlsx")



#Did not need after all, use Sys.setlocale to handle ÆØÅ
#Remove special characters and non-breaking space
new.name <- new.name %>% 
  mutate(Art = str_replace_all(string = Art, 
                               pattern = "[\u00f8]", 
                               replacement = "o")) #Special char Ø in Art needs to be written to o.



