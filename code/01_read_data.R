#LOAD DATA
install.packages("Rtools")
library(readxl)
library(xlsx)
library(tidyverse)
library(labdsv)

Sys.setlocale()
Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

info_dataset <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_info.xlsx", sheet = "Pals-lok-info", col_names = TRUE)

#IMPORT DATA
speciesline_raw <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Arter_artlinje", col_names = TRUE)
new_name <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "new_name", col_names = TRUE)
palslineSP <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Artl_Markslag", col_names = TRUE)
layerSP <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Arltl_sjikt", col_names = TRUE)

structureline <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "Markslag", col_names = TRUE)
permafrostdepth <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "Teledyp", col_names = TRUE)
layerPSL <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_artslinjer.xlsx", sheet = "Sjikt", col_names = TRUE)
ant_sprekker <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "antall-sprekker", col_names = TRUE)
bredde_sprekker <- read_excel(path = "data/pals_database/20.2.2023_Uttrekk_strukturlinjer.xlsx", sheet = "bredde-sprekker", col_names = TRUE)

###SPECIES LINES###
#CLEAN DATA SET
#Extract values in Art
species_name <- speciesline_raw %>% 
  select(Art) %>% 
  distinct() 

new_name <- new_name %>% 
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
speciesline <- speciesline_raw %>%
  rename(area= Omr.info, site=Navn, year=År, line=Lj.nr, segment=Linje.del, pinpoint=Punkt) %>% 
  unite("lineID", line:segment, sep = ".", remove = FALSE) %>% 
  left_join(new_name, by = "Art") %>% 
  left_join(palslineSP, by ="lineID", relationship = "many-to-one") %>% 
  select(area, site, year, lineID, line, segment,pinpoint, species, type, functional_type, palsstructure) %>% 
  filter(palsstructure == "pals")

write.xlsx(speciesline, "data/speciesline.xlsx")



###STRUCTURE LINES###
#CLEAN DATA SET

structurelinePSL <- structureline %>% 
  rename(area= "Omr-info", site=Navn, year=År, line="Lj-nr", segment="Linje-del") %>% 
  unite("lineID", line:segment, sep = ".", remove = FALSE) %>% 
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
colnames(palsstructurePSL1) <- c('Markslag', 'palsstructure')
palsstructurePSL1 <- as.data.frame(palsstructurePSL1)



#Did not need after all, use Sys.setlocale to handle ÆØÅ
#Remove special characters and non-breaking space
new_name <- new_name %>% 
  mutate(Art = str_replace_all(string = Art, 
                               pattern = "[\u00f8]", 
                               replacement = "o")) #Special char Ø in Art needs to be written to o.



