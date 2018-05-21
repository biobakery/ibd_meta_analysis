library(tidyverse)
library(stringr)
library(readxl)

# Read raw  data --------------------------------------------------------------
load("raw/metadata.RData", verbose = T)
FMT_Trial_Master <- read_excel("raw/FMT_Trial_Master.xlsx")
colnames(FMT_Trial_Master) = make.names(colnames(FMT_Trial_Master))

FMT_Trial_Master = FMT_Trial_Master %>% rename(subject = Study.ID) 

map_bidmc = map_bidmc %>% 
  left_join(FMT_Trial_Master) %>% 
  filter(!is.na(donor)) %>% 
  mutate(Project = "BIDMC-FMT") %>% 
  mutate(DonorID = subject) %>% 
  mutate(OriginalID = str_c(subject, ".", time, "A")) %>% 
  mutate(Location = "Stool") %>% 
  mutate(Diagnosis = "CD") %>% 
  mutate(Gender = recode(Gender, "0" = "Female", "1" = "Male"))


# Create BIDMC-FMT_common.txt -------------------------------------------------
# Create table
common = map_bidmc %>% 
  select(Project, DonorID, OriginalID, Location, Diagnosis, Gender, Age)

write_tsv(common, "processed/BIDMC-FMT_common.txt")
saveRDS(common, "processed/BIDMC-FMT_common.rds")

# Create CS-PRISM_special.txt --------------------------------------------------
special = map_bidmc %>% 
  mutate(Antibiotics = recode(Antibiotic.Rx., "0" = "No", "1" = "Yes")) %>% 
  mutate(Immunosuppressants = recode(Immunomodulator.type. %>% is.na %>% as.numeric, "1" = "No", "0" = "Yes")) %>% 
  mutate(Steroids = recode(PR.Steroids, "0" = "No", "1" = "Yes")) %>% 
  mutate(DiseaseLocation = Location.of.disease..Montreal.Classification.) %>% 
  select(Project, DonorID, OriginalID, Antibiotics, Immunosuppressants, Steroids, DiseaseLocation)

write_tsv(special, "processed/BIDMC-FMT_special.txt")
saveRDS(special, "processed/BIDMC-FMT_special.rds")
