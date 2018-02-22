library(tidyverse)
source("scripts/source.R")
study <- "RISK"

template <- read.csv('scripts/template.csv',
                     stringsAsFactors = F)
meta_raw_merged <- read_tsv(paste0("raw/", study, "/metadata/metadata_RISK.txt")) %>% 
  transpose_df
colnames(meta_raw_merged) <- meta_raw_merged[1, ]
meta_raw_merged <- meta_raw_merged[-1, ]

meta_curated <- meta_raw_merged %>% 
  mutate(dataset_name = study,
         study_accession = "RISK", # this needs to be fixed
         PMID = "28191884", # this needs to be fixed
         sample_accession = ID,
         sampleID = paste0(study_accession, ":", sample_accession),
         subjectID = paste0(study_accession, ":", participantID),
         body_site = SampleLocation %>% 
           recode("Ileum" = "TI",
                  "Rectum" = "rectum",
                  "stool" = "stool"),
         sample_type = body_site %>% 
           recode("TI" = "biopsy",
                  "rectum" = "biopsy",
                  "stool" = "stool"),
         disease = Diagnosis %>% 
           recode("CD" = "CD",
                  "non-IBD" = "control"),
         age = AgeAtDx %>% as.numeric,
         gender = GENDER %>% 
           recode("M" = "m",
                  "F" = "f")
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
