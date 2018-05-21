library(tidyverse)
library(readxl)
library(stringr)
library(forcats)

# Read data -------------------------------------------------------------------------------
lss_metadata = read_excel("raw/lss-metadata.xlsx")
samples = read.table("raw/sampList")

# samples2data to 
sample2project = read_tsv("sample2project_updated.txt", col_names = c("GID", "StoolID", "OriginalID", "DonorID", "SequencingRun", "Technology", "Project"))

# Merge the three files ---------------------------------------------------------------------
samples = samples %>% 
  extract(V1, c("OriginalID", "GID"), "(.*_.*)_(.*)")

lss_metadata = lss_metadata %>% 
  mutate(OriginalID = str_c(Project_Specific_Id, "_mo", LSS_Month)) %>% 
  mutate(Age = round((Encounter_Date - Birth_Date) / 365.25) %>% as.numeric) %>% 
  mutate(Project = "LSS-PRISM") %>% 
  mutate(DonorID = str_c("P", Project_Specific_Id)) %>% 
  mutate(Location = "Stool") %>% 
  mutate(Gender = Sex) %>% 
  inner_join(samples) %>% 
  select(-OriginalID) %>% 
  inner_join(select(sample2project, GID, OriginalID))


# Create LSS-PRISM_common.txt -------------------------------------------------------------
common = lss_metadata %>% 
  mutate(Diagnosis = recode(Diagnosis, "Crohn's_Disease" = "CD", "Indeterminate_colitis" = "IC", "Ulcerative_colitis" = "UC", "Healthy_control" = "HC")) %>% 
  select(Project, DonorID, OriginalID, Location, Diagnosis, Gender, Age)

write_tsv(common, "processed/LSS-PRISM_common.txt")
saveRDS(common, "processed/LSS-PRISM_common.rds")

##

## Create LSS-PRISM_special.txt
special = lss_metadata %>% 
  mutate(Project = "LSS-PRISM") %>%
  select(Project, DonorID, OriginalID, LSSMonth = LSS_Month, SmokingStatus = smoking_status, 
         HBI = HBI_Total, FecalCalprotectin = Fecal_Calprotectin_Results) %>% 
  mutate(FecalCalprotectin = str_replace(FecalCalprotectin, "(_)?ug/g", "") %>% as.numeric)

write_tsv(special, "processed/LSS-PRISM_special.txt")
saveRDS(special, "processed/LSS-PRISM_special.rds")
##



