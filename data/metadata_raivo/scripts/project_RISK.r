library(tidyverse)
library(stringr)
library(readr)

# Read data -------------------------------------------------------------------
d = read.table("raw/RISK_genus.tsv", sep = "\t", row.names = 1, nrow = 10) %>% t

tc = textConnection("b", open = "w")
write.table(d, tc, quote = F, sep = "\t")

d2 = read.table(textConnection(textConnectionValue(tc)), sep = "\t")
close(tc)

metadata = d2
rownames(metadata) = NULL

# Read in Melanies mapping
mm = read_delim("~/Raivo/Projects/IBD_metaanalysis/metadata/raw/complete_mapping_1070_Aug28.csv", "\t", escape_double = FALSE, col_names = FALSE)
mm = mm %>% 
  mutate(sample = str_replace(X2, "_S", ".S") %>% str_replace("SKBTI-", "SKBTI")) %>% 
  select(sample, OriginalID = X2, DonorID = X1)

metadata = metadata %>% 
  left_join(mm)



# Create RISK_common.txt ------------------------------------------------------
common = metadata %>% 
  mutate(Project = "RISK") %>% 
  select(Project, DonorID, OriginalID, 
         Location = sample_location, Diagnosis = Diagnosis, Gender = GenderT, 
         Age = AgeDxYrs) %>% 
  mutate(Location = recode(Location, "stool" = "Stool")) %>% 
  mutate(Diagnosis = recode(Diagnosis, "Not IBD" = "Control"))

write_tsv(common, "processed/RISK_common.txt")
saveRDS(common, "processed/RISK_common.rds")

# Create RISK_specific.txt ----------------------------------------------------
special = metadata %>% 
  mutate(Project = "RISK") %>% 
  select(Project, DonorID, OriginalID, 
         Antibiotics = AB_exposure, PCDAI, Race = race, Ulcering = ulcering) %>%
  mutate(Antibiotics = recode(Antibiotics, "NoAB" = "No", "YesAB" = "Yes"))
  
write_tsv(special, "processed/RISK_special.txt")
saveRDS(special, "processed/RISK_special.rds")






