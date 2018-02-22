library(tidyverse)
source("scripts/source.R")
study <- "LSS"

dir.create(paste0("processed/", study, "/metadata/"),
           recursive = T,
           showWarnings = F)
template <- read.csv('scripts/template.csv',
                     stringsAsFactors = F)
meta_raw <- read_tsv(paste0("raw/", study, "/metadata/lss-metadata.txt"))
sample_mapping <- read_tsv(paste0("raw/", study, "/metadata/sampList"), 
                           col_names = F) %>% 
  mutate(
    Project_Specific_Id = gsub("_.*", "", X1, perl = T) %>% 
      as.integer,
    LSS_Month = X1 %>% 
      gsub("_G.*", "", ., perl = T) %>% 
      gsub(".*_mo", "", ., perl = T) %>% 
      as.integer,
    GID = gsub(".*_", "", X1, perl = T)
  )
meta_raw_merged <- meta_raw %>%
  left_join(sample_mapping)
meta_curated <- meta_raw_merged %>% 
  mutate(dataset_name = study,
         study_accession = "LSS", # this needs to be fixed
         PMID = "28191884", # this needs to be fixed
         sample_accession = GID,
         sampleID = paste0(study_accession, ":", sample_accession),
         subjectID = paste0(study_accession, ":", Project_Specific_Id),
         body_site = "stool", # need to check this
         sample_type = "stool", # need to add function for this
         disease = Diagnosis %>% 
           recode("Crohn's_Disease" = "CD",
                  "Ulcerative_colitis" = "UC",
                  "Indeterminate_colitis" = "IC",
                  "Healthy_control" = "control"),
         control = Diagnosis %>% 
           recode("Crohn's_Disease" = "not applicable",
                  "Ulcerative_colitis" = "not applicable",
                  "Indeterminate_colitis" = "not applicable",
                  "Healthy_control" = "healthy"),
         age = NA, # largely missing, available are categorical
         gender = Sex %>% 
           recode("Male" = "m",
                  "Female" = "f")
  ) %>% select(template$col.name %>% one_of)

for(var in setdiff(template$col.name, colnames(meta_curated))) {
  meta_curated[[var]] <- NA
}
meta_curated <- meta_curated[, template$col.name]
write.table(meta_curated,
            file = paste0("processed/", study, "/metadata/metadata.txt"),
            quote = F,
            sep = "\t",
            row.names = F)
