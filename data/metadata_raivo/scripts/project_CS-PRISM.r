library(tidyverse)
library(stringr)

# Read data --------------------------------------------------------------------
# Metadata from Eric
d = read.table("raw/metadata-cln.tsv", sep = "\t", row.names = 1) %>% t

tc = textConnection("b", open = "w")
write.table(d, tc, quote = F, sep = "\t")

d2 = read.table(textConnection(textConnectionValue(tc)), sep = "\t")
close(tc)

metadata = d2
rownames(metadata) = NULL

# samples2data to 
sample2project = read_tsv("sample2project.txt", col_names = c("GID", "StoolID", "OriginalID", "DonorID", "SequencingRun", "Technology", "Project"))

# Transform metadata ----------------------------------------------------------
metadata= metadata %>% 
  mutate(Project = "CS-PRISM") %>% 
  mutate(DonorID = str_c("P", Patient)) %>% 
  mutate(GID = Gnum) %>% 
  mutate(Diagnosis = recode(Diagnosis, "HC" = "Control")) %>% 
  left_join(sample2project %>% select(GID, OriginalID))

# Create CS-PRISM_common.txt ---------------------------------------------------
# Create table
common = metadata %>%  
  filter(GID != "", !is.na(OriginalID)) %>% 
  select(Project, DonorID, OriginalID, Location, Diagnosis, Gender = Sex, Age = Age) %>% 
  mutate(Location = str_replace(Location, "\\(.*\\)", "") %>% na_if("")) %>% 
  mutate(Location = coalesce(Location, "Stool"))

write_tsv(common, "processed/CS-PRISM_common.txt")
saveRDS(common, "processed/CS-PRISM_common.rds")


# Create CS-PRISM_special.txt --------------------------------------------------
special = metadata %>% 
  filter(GID != "", !is.na(OriginalID)) %>% 
  select(Project, DonorID, OriginalID, AntiDiarrhoea = anti.diarrhoea, 
         Mesalamine = mesalamine, Antibiotics = antibiotic, Steroids = steroids,
         Immunosuppressants = immunosuppressant, Supplements = supplement,
         FecalCalprotectin = Fecal.Calprotectin) %>% 
  mutate(Antibiotics = na_if(Antibiotics, "")) %>% 
  mutate(AntiDiarrhoea = na_if(AntiDiarrhoea, "")) %>% 
  mutate(Mesalamine = na_if(Mesalamine, "")) %>% 
  mutate(Steroids = na_if(Steroids, "")) %>% 
  mutate(Immunosuppressants = na_if(Immunosuppressants, "")) %>% 
  mutate(Supplements = na_if(Supplements, "")) %>% 
  mutate(FecalCalprotectin = FecalCalprotectin %>% as.numeric)


write_tsv(special, "processed/CS-PRISM_special.txt")
saveRDS(special, "processed/CS-PRISM_special.rds")



